{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module FormatXml (
    processFile,
    EncodingName,
    FormatMode(..)
  )where

import System.IO
import System.IO.Temp
import Data.List
import Text.XML.HaXml.SAX
import Text.XML.HaXml.Types
import Control.Monad.State
import Control.Monad.Reader

import Data.Char
import Data.Maybe
import Text.Regex.TDFA

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import qualified Data.Text as TXT
import Data.Text.Encoding

import Control.Parallel.Strategies (rdeepseq, withStrategy)

type EncodingName = String

eol :: [Char]
eol = "\r\n"

data FormatMode = ModeBeautify -- ^ Форматирование "ёлочкой"
                  {
                    identString :: EncodingName -- ^ Строка, которая будет использоваться для отступов.
                  } 
                | ModeStrip -- ^ Удаление всех незначащих пробельных символов
                | ModeLegacy -- ^ Режим совместимости со старой утилитой на питоне
                deriving (Eq)

processFile :: Handle   -- ^ Исходный файл с XML документом
            -> FilePath -- ^ Наименование источника с входным документом. Необходимо для передчи в
                        -- SAX-парсер. Обычно - это имя файла содержащего исходный XML.
            -> Handle   -- ^ Файл в который будет помещен результат преобразования.
            -> Maybe EncodingName -- ^ Здесь можно указать кодировку входного файла. Если Nothing,
                                  -- то кодировка будет определена на основании BOM и заголовка XML.
            -> Maybe EncodingName -- ^ Здесь можно указать кодировку выходного файла. Если Nothing,
                                  -- то будет использована кодировка входного файла.
            -> FormatMode -- ^ Режим преобразования
            -> IO ()
processFile inH inFileName outH inputEncoding outputEncoding' mode' = do
  
  hSetBinaryMode inH True
  hSetBinaryMode outH True
  
  let getInputEncoding :: IO (Maybe EncodingName, BS.ByteString)
      getInputEncoding = case inputEncoding of
        Just e -> return (Just e, BS.empty)
        Nothing -> getXmlEncoding inH
          
  (inputEncodingName', inpt') <- getInputEncoding
  (inpt, inputEncodingName) <- case inputEncodingName' of
        Just inputEncodingName -> do
    
          inputEncoding' <- mkTextEncoding' inputEncodingName
      
          inpt'' <- withSystemTempFile "xmlbeauty.header" $ \_ tmpH ->
                    if BS.null inpt' then
                      return ""
                    else
                      do hSetBinaryMode tmpH True
                         BS.hPutStr tmpH inpt'
                         hSeek tmpH AbsoluteSeek 0
                         hSetEncoding tmpH inputEncoding'
                         -- Все эти strict* для того что-бы можно было закрыть
                         -- временный файл сразуже
                         !x <- withStrategy rdeepseq `liftM` hGetContents tmpH
                         return x
                    
          hSetEncoding inH inputEncoding'
          inpt''' <- hGetContents inH
          return (inpt'' ++ inpt''', inputEncodingName)
        
        Nothing -> do
          -- Если кодировку определить не смогли, то считаем, что
          -- входной поток данных в UTF-8. Нам необходимо
          -- раскодировать из UTF-8 не только еще не прочитанную часть
          -- файла, но и небольшую зачитанную заголовачную
          -- часть. Т.к. механизм чтения (Prelude.hGetContents и
          -- hSetEncoding) не позволяет это сделать, то используем
          -- здесь функционал Data.Text.Encoding.
          inpt'' <- BS.hGetContents inH
          return (TXT.unpack $ decodeUtf8 $ BS.append inpt' inpt'', "UTF-8")
  
  let (elms', err') = saxParse inFileName inpt
      elms = map SaxElement' elms' ++ [SaxError' err']
  
  withSystemTempFile "xmlbeauty.xml" $ \_ tmpH ->
    do let outputEncodingName = fromMaybe inputEncodingName outputEncoding'
       -- Если выходная кодировка не указана явно, то подразумевается что выходная кодировка должна
       -- совпадать с входной.
       hSetBinaryMode tmpH True
       hSetEncoding tmpH =<< mkTextEncoding' outputEncodingName
   
       let c = FormattingConfig {outputEncoding = case mode' of
                                                    ModeLegacy -> outputEncodingName
                                                    _          -> getEncodingName4XmlHeader outputEncodingName,
                                 mode = mode'}
           s = FormattingState {identLevel=0, elems=elms,
                                lastElem = LastElemNothing, penultElem = LastElemNothing,
                                postponedCharData = [], skippedIdent = SkippedNothing,
                                result = ""}
           !x = case mode' of 
             ModeStrip      -> printTreeStrip c s
             ModeBeautify _ -> printTreeBeauty c s
             ModeLegacy     -> printTreeBeauty c s
         
       hPutStr tmpH x
       
       hSeek tmpH AbsoluteSeek 0
       hSetBinaryMode tmpH True
       
       y <- hGetContents tmpH
       hPutStr outH y
       

data BomTestResult = FullyMatch EncodingName | SemiMatch EncodingName | NotMatch

-- | Определяет кодировку XML файла по BOM или из псевдо атрибута encoding заголовка XML
getXmlEncoding :: Handle                  -- ^ Handle файла содержащего XML
               -> IO (Maybe EncodingName, -- ^ если кодировку удалось определить, то возвращает ее как 'Just x'
                      BS.ByteString)      -- ^ байты, которые были зачитаны из файла Handle для определения кодировки
getXmlEncoding inH = do
  t <- bomTest'
  case t of
    (Just e, s)  -> return (Just e, s)
    (Nothing, s) -> xmlDeclTest s
  where
    bomTest' :: IO (Maybe EncodingName, BS.ByteString)
    bomTest' = do
      cr <- checkNextByte BS.empty
      case cr of
        (FullyMatch enc, s) -> return (Just enc, s)
        (NotMatch, s)       -> return (Nothing, s)
      where
        checkNextByte :: BS.ByteString -> IO (BomTestResult, BS.ByteString)
        checkNextByte s = hIsEOF inH >>= \eof ->
                          if eof then return (NotMatch, s)
                          else do
                            test_str <- BS.hGet inH 1 >>= \c -> return $ s `BS.append` c
                            case bomTest test_str of
                              x@NotMatch       -> return (x, test_str)
                              x@(FullyMatch _) -> return (x, BS.empty)
                              SemiMatch _      -> checkNextByte test_str
          
          where bomTest :: BS.ByteString -> BomTestResult
                bomTest xml =
                  let utf8'BOM       = BS.pack [0xef, 0xbb, 0xbf]
                      utf16be'BOM    = BS.pack [0xFE, 0xFF]
                      utf16le'BOM    = BS.pack [0xFF, 0xFE]
                      utf32be'BOM    = BS.pack [0x00, 0x00, 0xFE, 0xFF]
                      utf32le'BOM    = BS.pack [0xFF, 0xFE, 0x00, 0x00]
                      utf7'BOMstart  = BS.pack [0x2B, 0x2F, 0x76]
                      utf1'BOM       = BS.pack [0xF7, 0x64, 0x4C]
                      utfEBCDIC'BOM  = BS.pack [0xDD, 0x73, 0x66, 0x73]
                      scsu'BOM       = BS.pack [0x0E, 0xFE, 0xFF]
                      bocu1'BOM      = BS.pack [0xFB, 0xEE, 0x28]
                      gb18030'BOM    = BS.pack [0x84, 0x31, 0x95, 0x33]
                      
                      tests = [checkBOM utf8'BOM "UTF-8",
                               checkBOM utf16be'BOM "UTF-16BE",
                               checkBOM utf16le'BOM  "UTF-16LE",
                               checkBOM utf32be'BOM  "UTF-32BE",
                               checkBOM utf32le'BOM  "UTF-32LE",
                               check_utf7'BOM,
                               checkBOM utf1'BOM "UTF-1",
                               checkBOM utfEBCDIC'BOM "UTF-EBCDIC",
                               checkBOM scsu'BOM "SCSU",
                               checkBOM bocu1'BOM "BOCU-1",
                               checkBOM gb18030'BOM "GB18030"]
                     
                      checkBOM bom enc = case () of
                            _
                              | BS.isPrefixOf bom xml -> FullyMatch enc
                              | BS.isPrefixOf xml bom -> SemiMatch enc
                              | otherwise -> NotMatch
                          
                      check_utf7'BOM =
                            if xml `BS.isPrefixOf` utf7'BOMstart then
                              -- Для UTF-7 последний символ BOM может содержать
                              -- любой из четырех символов.
                              let xml' = BS.drop (BS.length utf7'BOMstart) xml
                              in case () of
                                _
                                  -- Проверим что что строка не кончилась на первой части BOM
                                  | BS.null xml'  -> SemiMatch "UTF-7"
                                  -- Проверим входит ли наш символ в группу допустимых концов BOM
                                  | BS.elem (BS.head xml') (BS.pack [0x38, 0x39, 0x2B, 0x2F]) -> FullyMatch "UTF-7"
                                  | otherwise -> NotMatch
                            else NotMatch
                      fully_matched = find (\x -> case x of; FullyMatch _ -> True; _ -> False) tests
                      semi_matched  = find (\x -> case x of; SemiMatch _ -> True; _ -> False) tests
                      
                  --in fromMaybe (fromMaybe NotMatch semi_matched) fully_matched
                  in flip fromMaybe fully_matched $ fromMaybe NotMatch semi_matched

    xmlDeclTest :: BS.ByteString -> IO (Maybe EncodingName, BS.ByteString)
    xmlDeclTest already_read_str = do
      -- Считаем что по крайней мере до конца xml-заголовока идут только однобайтовые символы
      eof <- hIsEOF inH
      case () of
        _
          | eof -> return (Nothing, already_read_str)
          | BS.length already_read_str > 1000 -> return (Nothing, already_read_str)
          | True -> do new_str <- BS.hGet inH howMatchRead >>= \c -> return $ already_read_str `BS.append` c
                       let test_str = dropWhile isSpace $ C8.unpack new_str
                           (_, _, _, enc) = test_str =~ "<\\?xml[ \t](.*\\bencoding=\"([^\"]+)\")?.*\\?>" :: (String, String, String, [String])
                       case enc of
                         _:"":_     -> return (Nothing, new_str)
                         _:e:_      -> return (Just e, new_str)
                         _          -> xmlDeclTest new_str

            where howMatchRead = if already_read_str_length < min_length
                                 then min_length - already_read_str_length
                                 else 1
                                   where min_length = length "<?xml encoding=\".\"?>"
                                         already_read_str_length = BS.length already_read_str
                                        


mkTextEncoding' :: EncodingName -> IO TextEncoding
mkTextEncoding' en =
  -- Если кодировка совпадает с одной из обязательно реализованных в стандартной
  -- библиотеке то выбираем ее явно, если нет, то используем mkTextEncoding
  case normalized of
    "ASCII"    -> return latin1
    "ISO88591" -> return latin1
    "LATIN1"   -> return latin1
    "UTF8"     -> return utf8
    "UTF16"    -> return utf16
    "UTF16LE"  -> return utf16le
    "UTF16BE"  -> return utf16be
    "UTF32LE"  -> return utf32le
    "UTF32BE"  -> return utf32be
    "WINDOWS1251" -> mkTextEncoding "CP1251"
    "WINDOWS866"  -> mkTextEncoding "CP866"
    _          -> mkTextEncoding (map toUpper en)
    
    where normalized = filter (`notElem` "_- ") (map toUpper en)


-- | Возвращает имя кодировки для вставки в атрибут encoding заголовка XML
getEncodingName4XmlHeader :: EncodingName -> String
getEncodingName4XmlHeader en =
  case normalized of
    "UTF16"   -> "UTF-16"
    "UTF16LE" -> "UTF-16"
    "UTF16BE" -> "UTF-16"
    "UTF32"   -> "UTF-32"
    "UTF32LE" -> "UTF-32"
    "UTF32BE" -> "UTF-32"
    "CP1251"  -> "WINDOWS-1251"
    "WINDOWS1251" -> "WINDOWS-1251"
    "CP866"   -> "WINDOWS-866"
    "WINDOWS866"  -> "WINDOWS-866"
    "ASCII"   -> "ISO-8859-1"
    "LATIN1"  -> "ISO-8859-1"
    _         -> en
    
    where normalized = filter (`notElem` "_- ") (map toUpper en)


type Formatting a = ReaderT FormattingConfig (State FormattingState) a


-- | Последний обработанный элемент
data LastElem = LastElemNothing -- ^ Признак того что тип элемента неизвестен
              | LastElemXmlHeader | LastElemProcessingInstruction | LastElemOpenTag | LastElemCloseTag | LastElemChars | LastElemComment
              deriving (Eq)

data FormattingConfig = FormattingConfig { outputEncoding :: String,
                                           mode :: FormatMode
                                         }

type SkippedCountType = Integer
data SkippedIdent = SkippedNothing | SkippingJustEnded | SkippedCount SkippedCountType
                  deriving (Eq)
data FormattingState = FormattingState { identLevel :: Int,
                                         elems :: [SaxElementWrapper],
                                         lastElem :: LastElem,
                                         penultElem :: LastElem,
                                         postponedCharData :: String,
                                         skippedIdent :: SkippedIdent,
                                         result :: String
                                       }

runFormatting :: FormattingConfig -> FormattingState -> Formatting a -> FormattingState
runFormatting cfg st act =
  flip execState st $
  flip runReaderT cfg
  act




data SaxElementWrapper = SaxElement' SaxElement | SaxError' (Maybe String)



escapeCharacterData :: String -> String
escapeCharacterData = concatMap $ \c ->
  case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    c'   -> [c']

escapeAttributeData :: String -> [Char]
escapeAttributeData = escapeAttributeData' . escapeCharacterData
  where
    escapeAttributeData' = concatMap $ \c ->
      case c of
        '"' -> "&quot;"
        c'   -> [c']

decodeRefEntity :: EntityRef -> Maybe String
decodeRefEntity "amp"   = Just "&"
decodeRefEntity "quot"  = Just "\""
decodeRefEntity "apos"  = Just "'"
decodeRefEntity "lt"    = Just "<"
decodeRefEntity "gt"    = Just ">"
decodeRefEntity _       = Nothing

decodeRefChar :: Int -> [Char]
decodeRefChar c = [chr c]

formatCharData :: forall t. t -> t
formatCharData s = s -- "<![CDATA[" ++ s ++ "]]>"

showElement :: SaxElement -> String
showElement (SaxProcessingInstruction (target, value)) =  "<?" ++ target ++ " " ++ value ++ "?>"
showElement (SaxElementOpen name attrs)                =  "<"  ++ name ++ showAttributes attrs ++ ">"
showElement (SaxElementClose name)                     =  "</" ++ name ++ ">"
showElement (SaxElementTag name attrs)                 =  "<"  ++ name ++ showAttributes attrs ++ "/>"
showElement (SaxCharData s)                            =  formatCharData . escapeCharacterData $ s
showElement (SaxComment a)                             =  "<!--" ++ a ++ "-->"
showElement (SaxReference (RefEntity name))            =  "&" ++ name ++ ";"
showElement (SaxReference (RefChar c))                 =  "&#" ++ show c ++ ";"
showElement _                                          =  ""

showAttributes :: [Attribute] -> String
showAttributes [] = ""
showAttributes attrs =
  ' ' : unwords (showAttributes' attrs)
  where
    showAttributes' :: [Attribute] -> [String]
    showAttributes' [] = []
    showAttributes' (a:as) = showAttr a : showAttributes' as
      where
        showAttr :: Attribute -> String
        showAttr (attrName, AttValue attrvs) =
          case attrName of
            N name ->
              name ++ "=\"" ++ showAttrValues attrvs ++ "\""
            QN Namespace {nsPrefix=nsPrefix'} name ->
              nsPrefix' ++ ":" ++ name ++ "=\"" ++ showAttrValues attrvs ++ "\""
          where
            showAttrValues :: [Either String Reference] -> String
            showAttrValues [] = []
            showAttrValues (Left str : vs)  = escapeAttributeData str ++ showAttrValues vs
            showAttrValues (Right ref : vs) =
              case ref of
                RefEntity name -> fromMaybe ("&" ++ name ++ ";") $ escapeAttributeData `liftM` decodeRefEntity name
                RefChar   c    -> escapeAttributeData . decodeRefChar $ c
              ++ showAttrValues vs


  
print' :: String -> Formatting ()
print' s = do
  st <- get
  put $ st { result = result st ++ s }


setLastElem :: LastElem -> Formatting ()
setLastElem le = do
  st <- get
  put $ st {lastElem = le, penultElem = lastElem st}
  
getLastElem :: Formatting LastElem
getLastElem = do
  st <- get
  return $ lastElem st


identMore :: Formatting ()
identMore = do
  x <- get
  put $ x { identLevel = identLevel x +1 }

identLess :: Formatting ()
identLess = do
  x <- get
  when (identLevel x > 0) $ put $ x { identLevel = identLevel x -1 }

skipIdent :: Formatting ()
skipIdent = do
  st <- get
  case skippedIdent st of
    SkippedCount cnt -> put $ st {skippedIdent = SkippedCount (cnt + 1)}
    _ -> put $ st {skippedIdent = SkippedCount 1}
  
skipIdentSimply :: Formatting ()
skipIdentSimply = do
  st <- get
  case skippedIdent st of
    SkippedNothing -> put $ st {skippedIdent = SkippingJustEnded}
    _ -> return ()

unskipIdent :: Formatting ()
unskipIdent = do
  st <- get
  case skippedIdent st of
    SkippedNothing -> return ()
    SkippingJustEnded ->
      put $ st {skippedIdent = SkippedNothing}
    SkippedCount 1 ->
      put $ st {skippedIdent = SkippingJustEnded}
    SkippedCount cnt ->
      put $ st {skippedIdent = SkippedCount (cnt - 1)}

getSkippedIdent :: Formatting SkippedIdent
getSkippedIdent = do
  st <- get
  return $ skippedIdent st
  

putPostponedCharData :: String  -> Formatting ()
putPostponedCharData e = do
  st <- get
  put $ st {postponedCharData = postponedCharData st ++ e}

savePostponedCharData :: LastElem  -- ^ Следующий элемент
                      -> Formatting ()
savePostponedCharData next = do
  st <- get
  let prev = lastElem st
      postponedCharData' = postponedCharData st
      skippedIdent' = skippedIdent st
      toPrint = case () of
        _
          | prev == LastElemOpenTag && next == LastElemCloseTag -> postponedCharData'
          | skippedIdent' /= SkippedNothing -> postponedCharData'
          | prev /= LastElemChars && next /= LastElemChars -> ""
          | otherwise -> postponedCharData'

  unless (null toPrint) $ do
    print' $ showElement $ SaxCharData toPrint
    setLastElem LastElemChars
    
  put $ st {postponedCharData = []}
  

unwrapSaxElem :: SaxElementWrapper -> Maybe SaxElement
unwrapSaxElem we =
  case we of
    SaxElement' e -> Just e
    SaxError' Nothing -> Nothing
    SaxError' (Just s) -> error s

printTreeBeauty  :: FormattingConfig -> FormattingState -> String
printTreeBeauty cfg st =
  case elems st of
    [] -> 
      -- После последнего элемента добавляем перенос строки
      let lastNewLine = case mode cfg of
            ModeBeautify _ -> eol
            ModeLegacy     -> ""
      in case lastElem st of
        LastElemCloseTag -> lastNewLine
        LastElemComment  -> lastNewLine
        LastElemProcessingInstruction -> lastNewLine
        _                -> ""
            
    e:ex -> case unwrapSaxElem e of
      Just x -> let st' = runFormatting cfg st{elems=ex, result = ""} $ printElemBeauty x
                in result st' ++ printTreeBeauty cfg st'
      _ -> printTreeBeauty cfg st{elems=ex}


ensureEndsWithSpace :: String -> String
ensureEndsWithSpace [] = []
ensureEndsWithSpace s =
  if isSpace (last s)
    then s
    else s ++ " "

encodingPattern = "\\bencoding=\"[^\"]+\""

printElemBeauty :: SaxElement -> Formatting ()
printElemBeauty e = do
  cfg <- ask
  case e of
    x@(SaxProcessingInstruction ("xml", _)) -> do savePostponedCharData LastElemXmlHeader
                                                  lastElem' <- getLastElem
                                                  case lastElem' of
                                                    LastElemNothing  -> return ()
                                                    _                -> print' eol
                                                  
                                                  conditionalPrintIdent $ showElement $ changeEncodingInProcessingInstruction x $ outputEncoding cfg
                                                  when (mode cfg == ModeLegacy) $ print' eol
                                                  setLastElem LastElemXmlHeader
      where
        changeEncodingInProcessingInstruction (SaxProcessingInstruction (target, value)) encodingName =
          let (pre, _, post) = value =~ encodingPattern :: (String, String, String)
          in SaxProcessingInstruction (target, case mode cfg of
                                          ModeLegacy -> "version=\"1.0\" encoding=\"" ++ encodingName ++ "\""
                                          _ -> ensureEndsWithSpace pre ++ "encoding=\"" ++ encodingName ++ "\"" ++ post)
  
        
    x@(SaxProcessingInstruction _) ->  when (mode cfg /= ModeLegacy) $ do
                                         savePostponedCharData LastElemProcessingInstruction
                                         conditionalSkipIdentSimply
                                         conditionalNewLine
                                         conditionalPrintIdent (showElement x) 
                                         setLastElem LastElemProcessingInstruction
      
    x@(SaxComment s)        ->  when (mode cfg /= ModeLegacy) $ do
                                  savePostponedCharData LastElemComment
                                  conditionalSkipIdentSimply
                                  let isEmacsInstructions s' = s' =~ " -\\*- +.+:.+ -\\*- " :: Bool
                                  lastElem' <- getLastElem
                                  case lastElem' of
                                    
                                    LastElemXmlHeader -> unless (isEmacsInstructions s) (print' eol) -- Emacs instruction must be placed on the first line of file
                                    _ -> conditionalNewLine
                                  conditionalPrintIdent (showElement x)
                                  setLastElem LastElemComment
    
    x@(SaxElementTag _ _)   -> do savePostponedCharData LastElemOpenTag
                                  conditionalSkipIdentSimply
                                  conditionalNewLine
                                  conditionalPrintIdent (showElement x)
                                  setLastElem LastElemCloseTag
                                  
    x@(SaxElementOpen _ _)  -> do savePostponedCharData LastElemOpenTag
                                  conditionalNewLine
                                  conditionalPrintIdent (showElement x)
                                  conditionalIdentMore
                                  setLastElem LastElemOpenTag
                                  
    x@(SaxElementClose _ )  -> do savePostponedCharData LastElemCloseTag
                                  
                                  lastElem' <- getLastElem
                                  conditionalIdentLess
                                  case lastElem' of
                                    LastElemChars   -> return ()
                                    LastElemOpenTag -> return ()
                                    _               -> do conditionalNewLine
                                                          conditionalPrintIdent ""
                                  unskipIdent
                                  print' $ showElement x
                                  setLastElem LastElemCloseTag
    
    x@(SaxCharData s)       -> do
      lastElem' <- getLastElem
      case () of
        _
          | all isSpace s && lastElem' /= LastElemChars -> putPostponedCharData s
          | otherwise -> do savePostponedCharData LastElemChars
                            print' $ showElement x
                            setLastElem LastElemChars
        
                                            
    (SaxReference (RefChar c)) -> printElemBeauty $ SaxCharData $ decodeRefChar c
    
    x@(SaxReference (RefEntity name)) -> do
      case decodeRefEntity name of
        Just s -> printElemBeauty $ SaxCharData s
        Nothing -> do print' $ showElement x
                      setLastElem LastElemChars
      

  where
    conditionalNewLine = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      when (lastElem' /= LastElemChars && lastElem' /= LastElemNothing && skippedIdent' == SkippedNothing) $ print' eol
      
    conditionalPrintIdent x = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      if lastElem' /= LastElemChars && skippedIdent' == SkippedNothing then printIdent x else print' x
      where
        printIdent :: String -> Formatting ()
        printIdent s = do cfg <- ask
                          st <- get
                          let identString' = case mode cfg of
                                              ModeBeautify i -> i
                                              ModeLegacy -> "\t"
                          print' $ concat (replicate (identLevel st) identString') ++ s

    conditionalIdentMore = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      if lastElem' /= LastElemChars && skippedIdent' == SkippedNothing then identMore else skipIdent
        
    conditionalIdentLess = do
      skippedIdent' <- getSkippedIdent
      when (skippedIdent' `elem` [SkippedNothing, SkippingJustEnded]) identLess
                            
    conditionalSkipIdentSimply = do
      lastElem' <- getLastElem
      when (lastElem' == LastElemChars) skipIdentSimply

    

printTreeStrip  :: FormattingConfig -> FormattingState -> String
printTreeStrip cfg st =
  case elems st of
    [] -> ""
    e:ex -> case unwrapSaxElem e of
      Just e' -> let st' = runFormatting cfg st{elems=ex, result = ""} $ printElemStrip e'
                in result st' ++ printTreeStrip cfg st'
      _ -> printTreeStrip cfg st{elems=ex}
    

printElemStrip :: SaxElement -> Formatting ()
printElemStrip e = do
  cfg <- ask
  case e of
    x@(SaxProcessingInstruction ("xml", _)) -> do savePostponedCharData LastElemXmlHeader
                                                  print' $ showElement $ changeEncodingInProcessingInstruction x $ outputEncoding cfg
                                                  setLastElem LastElemXmlHeader
      where
        changeEncodingInProcessingInstruction (SaxProcessingInstruction (target, value)) encodingName =
          let (pre, _, post) =  value =~ encodingPattern :: (String, String, String)
          in SaxProcessingInstruction (target, ensureEndsWithSpace pre ++ "encoding=\"" ++ encodingName ++ "\"" ++ post)
  
        
    x@(SaxProcessingInstruction _) -> do savePostponedCharData LastElemProcessingInstruction
                                         conditionalSkipIdentSimply
                                         print' (showElement x) 
                                         setLastElem LastElemProcessingInstruction
      
    x@(SaxComment _)        -> do savePostponedCharData LastElemComment
                                  conditionalSkipIdentSimply
                                  print' (showElement x)
                                  setLastElem LastElemComment
    
    x@(SaxElementTag _ _)   -> do savePostponedCharData LastElemOpenTag
                                  conditionalSkipIdentSimply
                                  print' (showElement x)
                                  setLastElem LastElemCloseTag
                                  
    x@(SaxElementOpen _ _)  -> do savePostponedCharData LastElemOpenTag
                                  print' (showElement x)
                                  conditionalIdentMore
                                  setLastElem LastElemOpenTag
                                  
    x@(SaxElementClose _ )  -> do savePostponedCharData LastElemCloseTag
                                  unskipIdent
                                  print' $ showElement x
                                  setLastElem LastElemCloseTag
    
    x@(SaxCharData s)       -> do
      lastElem' <- getLastElem
      case () of
        _
          | all isSpace s && lastElem' /= LastElemChars -> putPostponedCharData s
          | otherwise -> do savePostponedCharData LastElemChars
                            print' $ showElement x
                            setLastElem LastElemChars
        
                                            
    (SaxReference (RefChar c)) -> printElemStrip $ SaxCharData $ decodeRefChar c
    
    x@(SaxReference (RefEntity name)) -> do
      case decodeRefEntity name of
        Just s -> printElemStrip $ SaxCharData s
        
        Nothing -> do print' $ showElement x
                      setLastElem LastElemChars
      

  where
    conditionalIdentMore = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      unless (lastElem' /= LastElemChars && skippedIdent' == SkippedNothing) skipIdent
        
    conditionalSkipIdentSimply = do
      lastElem' <- getLastElem
      when (lastElem' == LastElemChars) skipIdentSimply
