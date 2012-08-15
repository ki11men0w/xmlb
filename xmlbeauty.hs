{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import System.Console.CmdArgs
import System.IO
import System.IO.Temp
import System.Directory
import System.FilePath
import Data.List
import Text.XML.HaXml.SAX
import Text.XML.HaXml.Types
import Control.Monad.State
import Control.Monad.Reader

import Data.Char
import Data.Maybe
import Control.Exception (SomeException, catch)
import Prelude hiding (catch)
import Text.Regex.Posix

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import qualified Data.Text as TXT
import Data.Text.Encoding

import Control.Parallel.Strategies (rdeepseq, withStrategy)

programVersion = "2.1.0.0 (haskell)"

defaultSpaceIdent = 3


-- | Флаги коммандной строки
data Flags = Flags
             { backup :: Bool,
               output_encoding :: Maybe String,
               input_encoding :: Maybe String,
               spaces :: Maybe Int,
               inFileNames :: [String]
             } deriving (Data, Typeable)
                                
opts' = getProgName >>= \programName -> return $
        Flags { backup =
                   def
                   &= help "Backup original files",
                output_encoding =
                  def
                  &= help ("Encoding for OUTPUT documents (default is encoding of input document)")
                  &= explicit &= name "e" &= name "encoding" &= name "o"
                  &= typ  "ENC",
                input_encoding =
                  def
                  &= help "Encoding for INPUT documents. If not specified then the content of an input XML-document will be used to realize its encoding. If all failed then UTF-8 will be used"
                  &= typ  "ENC",
                spaces =
                  def
                  &= help ("Use this number of spaces instead of tabs for identation (default is " ++ show defaultSpaceIdent ++ ")")
                  &= opt (show defaultSpaceIdent),
                
                inFileNames =
                  def &= args &= typ "XMLFILE1 [XMLFILE2 ...]"
              }
        &= program programName
        &= summary ("XML Beautifier version " ++ programVersion)
        &= details ["Beautifies (makes human readable) xml file(s). If input data is/are common file(s) and no redirection of STDOUT is specified than input files will be changed inplace. If STDIN is not a terminal (e.g. because of redirection) than result will be printed to STDOUT.",
                    "usage: " ++ programName ++ " OPTIONS XMLFILE1 [XMLFILE2 ...]",
                    "       " ++ programName ++ " OPTIONS < somefile.xml > somefile.xml"]


checkOptions opts = do
  hIsTerminalDevice stdin >>= \t -> checkSources t (inFileNames opts)
  where
    showOption (x:[]) = "-" ++ [x]
    showOption x      = "--" ++ x

    checkSources False (x:_) = error $ "As a data source, you must specify either STDIN or file(s) listed in the command line, but not both"
    checkSources _ _ = return ()

  

type EncodingName = String

data BomTestResult = FullyMatch EncodingName | SemiMatch EncodingName | NotMatch

getXmlEncoding :: Handle -> IO (Maybe EncodingName, BS.ByteString)
getXmlEncoding inH = do
  t <- bomTest'
  case t of
    (Just e, s)  -> return (Just e, s)
    (Nothing, s) -> xmlDeclTest s
  where
    bomTest' :: IO (Maybe EncodingName, BS.ByteString)
    bomTest' = do
      cr <- checkNextByte $ BS.empty
      case cr of
        (FullyMatch enc, s) -> return (Just enc, s)
        (NotMatch, s)       -> return (Nothing, s)
      where
        checkNextByte :: BS.ByteString -> IO (BomTestResult, BS.ByteString)
        checkNextByte s = hIsEOF inH >>= \eof ->
                          if eof then return (NotMatch, s)
                          else do
                            test_str <- BS.hGet inH 1 >>= \c -> return $ BS.concat [s, c]
                            let bt = bomTest test_str
                            case bomTest test_str of
                              x@(NotMatch)     -> return (x, test_str)
                              x@(FullyMatch e) -> return (x, BS.empty)
                              x@(SemiMatch _)  -> checkNextByte test_str
          
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
                     
                      checkBOM bom enc = case 1 of
                            _
                              | BS.isPrefixOf bom xml -> FullyMatch enc
                              | BS.isPrefixOf xml bom -> SemiMatch enc
                              | otherwise -> NotMatch
                          
                      check_utf7'BOM =
                            if BS.isPrefixOf xml utf7'BOMstart then
                              -- Для UTF-7 последний символ BOM может содержать 
                              -- любой из четырех символов.
                              let xml' = BS.drop (BS.length utf7'BOMstart) xml
                              in case 1 of
                                _
                                  -- Проверим что что строка не кончилась на первой части BOM
                                  | BS.null xml'  -> SemiMatch "UTF-7"
                                  -- Проверим входит ли наш символ в группу допустимых концов BOM
                                  | BS.elem (BS.head xml') (BS.pack [0x38, 0x39, 0x2B, 0x2F]) -> FullyMatch "UTF-7"
                                  | otherwise -> NotMatch
                            else NotMatch
                      fully_matched = find (\x -> case x of; FullyMatch enc -> True; otherwithe -> False) tests
                      semi_matched  = find (\x -> case x of; SemiMatch enc -> True; otherwithe -> False) tests
                      
                  in case fully_matched of
                    Just x -> x
                    Nothing -> case semi_matched of
                      Just x -> x
                      Nothing -> NotMatch

    xmlDeclTest :: BS.ByteString -> IO (Maybe EncodingName, BS.ByteString)
    xmlDeclTest already_read_str = do
      -- Считаем что по крайней мере до конца xml-заголовока идут только однобайтовые символы
      eof <- hIsEOF inH
      case 1 of
        _
          | eof -> return (Nothing, already_read_str)
          | BS.length already_read_str > 1000 -> return (Nothing, already_read_str)
          | True -> do new_str <- BS.hGet inH howMatchRead >>= \c -> return $ BS.concat [already_read_str, c]
                       let test_str = dropWhile isSpace $ C8.unpack new_str
                           (_, _, _, enc) = test_str =~ "<\\?xml[ \t](.*encoding=\"([^\"]+)\")?.*\\?>" :: (String, String, String, [String])
                       case enc of
                         _:"":_     -> return (Nothing, new_str)
                         _:e:_      -> return (Just e, new_str)
                         otherwithe -> xmlDeclTest new_str

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
    "UTF8"     -> return utf8
    "UTF16"    -> return utf16
    "UTF16LE"  -> return utf16le
    "UTF16BE"  -> return utf16be
    "UTF32LE"  -> return utf32le
    "UTF32BE"  -> return utf32be
    "WINDOWS1251" -> mkTextEncoding ("CP1251")
    "WINDOWS866"  -> mkTextEncoding ("CP866")
    _          -> mkTextEncoding (map toUpper en)
    
    where normalized = filter (\x -> not $ elem x "_- ") (map toUpper en)  

-- Возвращает имя кодировки для вставки в атрибут encoding заголовка XML
getEncodingName4XmlHeader :: EncodingName -> String
getEncodingName4XmlHeader en =
  case normalized of
    "UTF16LE" -> "UTF-16"
    "UTF16BE" -> "UTF-16"
    "UTF32LE" -> "UTF-32"
    "UTF32BE" -> "UTF-32"
    "CP1251"  -> "WINDOWS-1251"
    "CP866"   -> "WINDOWS-866"
    "ASCII"   -> "ISO-8859-1"
    _         -> en
    
    where normalized = filter (\x -> not $ elem x "_- ") (map toUpper en)  


type Parsing a = ReaderT ParseConfig (State ParseState) a

data LastElem = LastElemNothing | LastElemXmlHeader | LastElemProcessingInstruction | LastElemOpenTag | LastElemCloseTag | LastElemChars | LastElemComment
              deriving (Eq)

data ParseConfig = ParseConfig { 
                                 outputEncoding :: String,
                                 identString :: String
                               }

type SkippedCountType = Integer
data SkippedIdent = SkippedNothing | SkippingJustEnded | SkippedCount SkippedCountType
                  deriving (Eq)
data ParseState = ParseState { identLevel :: Int,
                               elems :: [SaxElementWrapper],
                               lastElem :: LastElem,
                               penultElem :: LastElem,
                               postponedCharData :: String,
                               skippedIdent :: SkippedIdent,
                               result :: String
                             }

data SaxElementWrapper = SaxElement' SaxElement | SaxError' (Maybe String)

parseDoc :: Handle -> FilePath -> Handle ->Maybe EncodingName -> Maybe EncodingName -> String -> IO ()
parseDoc inH inFileName outH inputEncoding outputEncoding identString = do
  
  hSetBinaryMode inH True
  
  let getInputEncoding :: IO (Maybe EncodingName, BS.ByteString)
      getInputEncoding = case inputEncoding of
        Just e -> return (Just e, BS.empty)
        Nothing -> getXmlEncoding inH
          
  (inputEncodingName', inpt') <- getInputEncoding
  (inpt, inputEncodingName) <- case inputEncodingName' of
        Just inputEncodingName -> do
    
          inputEncoding <- mkTextEncoding' inputEncodingName
      
          inpt' <- withSystemTempFile "xmlbeauty.header" $ \_ -> \tmpH ->
                    if BS.null inpt' then
                      return ""
                    else
                      do hSetBinaryMode tmpH True
                         BS.hPutStr tmpH inpt'
                         hSeek tmpH AbsoluteSeek 0
                         hSetEncoding tmpH inputEncoding
                         -- Все эти strict* для того что-бы можно было закрыть
                         -- временный файл сразуже
                         !x <- (withStrategy rdeepseq) `liftM` hGetContents tmpH
                         return x
                    
          hSetEncoding inH inputEncoding
          inpt'' <- hGetContents inH
          return $ (inpt' ++ inpt'', inputEncodingName)
        
        Nothing -> do
          -- Если кодировку определить не смогли, то считаем, что
          -- входной поток данных в UTF-8. Нам необходимо
          -- раскодировать из UTF-8 не только еще не прочитанную часть
          -- файла, но и небольшую зачитанную заголовачную
          -- часть. Т.к. механизм чтения (Prelude.hGetContents и
          -- hSetEncoding) не позволяет это сделать, то используем
          -- здесь функционал Data.Text.Encoding.
          inpt'' <- BS.hGetContents inH
          return $ (TXT.unpack $ decodeUtf8 $ BS.append inpt' inpt'', "UTF-8")
  
  let (elms', err') = saxParse inFileName (inpt)
      elms = (map SaxElement' elms') ++ [SaxError' err']
  
  withSystemTempFile "xmlbeauty.xml" $ \_ -> \tmpH ->
    do let outputEncodingName = fromMaybe inputEncodingName outputEncoding
       -- Если выходная кодировка не указана явно, то подразумевается что выходная кодировка должна
       -- совпадать с входной.
       hSetBinaryMode tmpH True
       hSetEncoding tmpH =<< mkTextEncoding' (fromMaybe inputEncodingName outputEncoding)
   
       let c = ParseConfig {outputEncoding = getEncodingName4XmlHeader outputEncodingName,
                            identString = identString}
           s = ParseState {identLevel=0, elems=elms,
                           lastElem = LastElemNothing, penultElem = LastElemNothing,
                           postponedCharData = [], skippedIdent = SkippedNothing,
                           result = ""}
           !x = printTree c s
         
       hPutStr tmpH x
       
       hSeek tmpH AbsoluteSeek 0
       hSetBinaryMode tmpH True
       
       y <- hGetContents tmpH
       hPutStr outH y
       

setIdent :: Int -> Parsing ()
setIdent x = do
  z <- get
  put $ z { identLevel = x }

getIdent :: Parsing Int
getIdent = do
  x <- get
  return $ identLevel x
  
identMore :: Parsing ()
identMore = do
  x <- get
  put $ x { identLevel = identLevel x +1 }

identLess :: Parsing ()
identLess = do
  x <- get
  put $ x { identLevel = identLevel x -1 }
  
  
print' :: String -> Parsing ()
print' s = do
  st <- get
  put $ st { result = (result st) ++ s }

printIdent :: String -> Parsing ()
printIdent s = do cfg <- ask
                  st <- get
                  --print' $ replicate (identLevel x) '\t' ++ s
                  print' $ concat (take (identLevel st) (repeat (identString cfg))) ++ s

unwrapSaxElem :: SaxElementWrapper -> Maybe SaxElement
unwrapSaxElem we = 
  case we of
    SaxElement' e -> Just e
    SaxError' Nothing -> Nothing
    SaxError' (Just s) -> error s
    
setLastElem :: LastElem -> Parsing ()
setLastElem le = do
  st <- get
  put $ st {lastElem = le, penultElem = lastElem st}
  
getLastElem :: Parsing LastElem
getLastElem = do
  st <- get
  return $ lastElem st
  
skipIdent :: Parsing ()
skipIdent = do
  st <- get
  case skippedIdent st of
    SkippedCount cnt -> put $ st {skippedIdent = SkippedCount (cnt + 1)}
    _ -> put $ st {skippedIdent = SkippedCount 1}
  
skipIdentSimply :: Parsing ()
skipIdentSimply = do
  st <- get
  case skippedIdent st of
    SkippedNothing -> put $ st {skippedIdent = SkippingJustEnded}
    _ -> return ()

unskipIdent :: Parsing ()
unskipIdent = do
  st <- get
  case skippedIdent st of
    SkippedNothing -> return ()
    SkippingJustEnded -> do
      put $ st {skippedIdent = SkippedNothing}
    SkippedCount 1 -> do
      put $ st {skippedIdent = SkippingJustEnded}
    SkippedCount cnt -> do
      put $ st {skippedIdent = SkippedCount (cnt - 1)}

getSkippedIdent :: Parsing SkippedIdent
getSkippedIdent = do
  st <- get
  return $ skippedIdent st
  
putPostponedCharData :: String  -> Parsing ()
putPostponedCharData e = do
  st <- get
  put $ st {postponedCharData = postponedCharData st ++ e}

clearPostponedCharData :: Parsing ()
clearPostponedCharData = do
  st <- get
  put $ st {postponedCharData = []}
  
savePostponedCharData :: LastElem  -- ^ Следующий элемент
                      -> Parsing ()
savePostponedCharData next = do
  st <- get
  let prev = lastElem st
      postponedCharData' = postponedCharData st
      skippedIdent' = skippedIdent st
      toPrint = case 1 of
        _
          | prev == LastElemOpenTag && next == LastElemCloseTag -> postponedCharData'
          | skippedIdent' /= SkippedNothing -> postponedCharData'
          | prev /= LastElemChars && next /= LastElemChars -> ""
          | otherwise -> postponedCharData'

  clearPostponedCharData
  
  unless (null toPrint) $ do    
    print' $ formatCharData $ xmlEscape' $ toPrint
    setLastElem LastElemChars
  

getOutputEncoding :: Parsing String
getOutputEncoding = do
  cfg <- ask
  return $ outputEncoding cfg

xmlEscape' :: String -> String
xmlEscape' s =
  concat $ map (\c -> case c of
                 '<' -> "&lt;"
                 c   -> [c]) s

formatCharData s = s --"<![CDATA[" ++ s ++ "]]>"

showElement :: SaxElement -> String
showElement (SaxProcessingInstruction (target, value)) =  "<?" ++ target ++ " " ++ value ++ "?>"
showElement (SaxElementOpen name attrs)                =  "<"  ++ name ++ showAttributes attrs ++ ">"
showElement (SaxElementClose name)                     =  "</" ++ name ++ ">"
showElement (SaxElementTag name attrs)                 =  "<"  ++ name ++ showAttributes attrs ++ "/>"
showElement (SaxCharData s)                            =  formatCharData s
showElement (SaxComment a)                             =  "<!--" ++ a ++ "-->"
showElement (SaxReference r)                           = case r of
                                                           RefEntity name -> "&" ++ name ++ ";"
                                                           RefChar   c    -> "&#" ++ show c ++ ";"
showElement _                                          =  ""

showSaxProcessingInstruction :: SaxElement -> String -> String
showSaxProcessingInstruction (SaxProcessingInstruction (target, value)) encodingName =
  let (pre, match, post) = value =~ "[ \t]+encoding=\"[^\"]+\"" :: (String, String, String)
  in "<?" ++ target ++ " " ++ pre ++ " encoding=\"" ++ encodingName ++ "\"" ++ post ++ "?>"

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
            showAttrValues (Left str : vs)  = concat (map (\c -> if c == '"' then "&quot;" else [c]) str) ++ showAttrValues vs
            showAttrValues (Right ref : vs) =
              case ref of
                RefEntity name -> "&" ++ name ++ ";"
                RefChar   c    -> "&#" ++ show c ++ ";"
              ++ showAttrValues vs


printElem :: SaxElement -> Parsing ()
printElem e = do
  st <- get
  cfg <- ask
  case e of
    x@(SaxProcessingInstruction ("xml", _)) -> do savePostponedCharData LastElemXmlHeader
                                                  lastElem' <- getLastElem
                                                  case lastElem' of
                                                    LastElemNothing  -> return ()
                                                    _                -> print' "\n"
                                                  
                                                  enc <- getOutputEncoding
                                                  printIdent $ showSaxProcessingInstruction x enc
                                                  setLastElem LastElemXmlHeader
      
    x@(SaxProcessingInstruction _) -> do savePostponedCharData LastElemProcessingInstruction
                                         conditionalSkipIdentSimply
                                         conditionalNewLine
                                         conditionalPrintIdent (showElement x) 
                                         setLastElem LastElemProcessingInstruction
      
    x@(SaxComment s)        -> do savePostponedCharData LastElemComment
                                  conditionalSkipIdentSimply
                                  let isEmacsInstructions s = s =~ " -\\*- +.+:.+ -\\*- " :: Bool
                                  lastElem' <- getLastElem
                                  case lastElem' of
                                    
                                    LastElemXmlHeader -> unless (isEmacsInstructions s) (print' "\n") -- Emacs instruction must be placed on the first line of file
                                    _ -> conditionalNewLine
                                  conditionalPrintIdent (showElement x)
                                  setLastElem LastElemComment
    
    x@(SaxElementTag _ _)   -> do savePostponedCharData LastElemOpenTag
                                  --getSkippedIdent >>= \c -> getIdent >>= \i -> print' $ "{close: skippedCnt=" ++ show c ++ ", ident="++ show i ++"}"
                                  conditionalSkipIdentSimply
                                  conditionalNewLine
                                  conditionalPrintIdent (showElement x) 
                                  --getSkippedIdent >>= \c -> getIdent >>= \i -> print' $ "{close: skippedCnt=" ++ show c ++ ", ident="++ show i ++"}"
                                  setLastElem LastElemCloseTag
                                  
    x@(SaxElementOpen _ _)  -> do savePostponedCharData LastElemOpenTag
                                  conditionalNewLine
                                  conditionalPrintIdent (showElement x) 
                                  conditionalIdentMore
                                  setLastElem LastElemOpenTag
                                  
    x@(SaxElementClose _ )  -> do savePostponedCharData LastElemCloseTag
                                  
                                  --getSkippedIdent >>= \c -> getIdent >>= \i -> print' $ "{close before: skippedCnt=" ++ show c ++ ", ident="++ show i ++"}"
                                  lastElem' <- getLastElem
                                  conditionalIdentLess
                                  case lastElem' of
                                    LastElemChars   -> return ()
                                    LastElemOpenTag -> return ()
                                    _               -> do conditionalNewLine
                                                          conditionalPrintIdent ""
                                  unskipIdent
                                  print' $ showElement x
                                  --getSkippedIdent >>= \c -> getIdent >>= \i -> print' $ "{close after: skippedCnt=" ++ show c ++ ", ident="++ show i ++"}"
                                  setLastElem LastElemCloseTag
    
    x@(SaxCharData s)       -> do
      lastElem' <- getLastElem
      case 1 of
        _
          | (all isSpace s) && lastElem' /= LastElemChars -> putPostponedCharData s
          | otherwise -> do savePostponedCharData LastElemChars
                            print' $ formatCharData $ xmlEscape' s
                            setLastElem LastElemChars
                                            
    x@(SaxReference _)      -> do print' $ showElement x
                                  setLastElem LastElemChars
    x                       -> print' $ showElement x

  where
    conditionalNewLine = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      when (lastElem' /= LastElemChars && skippedIdent' == SkippedNothing) $ print' "\n"
      
    conditionalPrintIdent x = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      if lastElem' /= LastElemChars && skippedIdent' == SkippedNothing then printIdent x
      else print' x

    conditionalIdentMore = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      if lastElem' /= LastElemChars && skippedIdent' == SkippedNothing then identMore
      else skipIdent
    
    conditionalIdentLess = do
      lastElem' <- getLastElem
      skippedIdent' <- getSkippedIdent
      when (skippedIdent' == SkippedNothing || skippedIdent' == SkippingJustEnded) identLess
      
                            
    conditionalSkipIdentSimply = do
      lastElem' <- getLastElem
      when (lastElem' == LastElemChars) skipIdentSimply

      

printTree  :: ParseConfig -> ParseState -> String
printTree cfg st =
  case elems st of
    [] -> case lastElem st of
                 LastElemCloseTag -> lastNewLine
                 LastElemComment  -> lastNewLine
                 LastElemProcessingInstruction -> lastNewLine
                 _                -> ""
                 where lastNewLine = "\n"
    e:ex -> case unwrapSaxElem e of
      Just e -> let st' = flip execState st{elems=ex, result = ""} $
                          flip runReaderT cfg $
                          printElem e
                in result st' ++ printTree cfg st'
      _ -> printTree cfg st{elems=ex}
    
      

processOneSource :: Flags -> FilePath -> IO ()
processOneSource opts inFileName = do
  tmpDir <- catch getTemporaryDirectory (\(e :: SomeException) -> return ".")
  let inFileP = inFileName
  
  stdout_isatty <- hIsTerminalDevice stdout
  let inPlace = inFileP /= "-" && stdout_isatty
  
  (inFileH, inFileDecoratedName) <- if inFileP == "-"
                                    then return (stdin, "stdin")
                                    else do x <- openFile inFileP ReadMode
                                            return (x, inFileP)
  (outFileP, outFileH) <-
    if inPlace
      then openTempFile tmpDir "xmlbeauty.xml" 
      else return ("-", stdout)
  
  hSetBinaryMode inFileH True
  hSetBinaryMode outFileH True
  
  parseDoc inFileH inFileDecoratedName outFileH inputEncoding outputEncoding identString
  
  when inPlace $
    do hClose inFileH
       hClose outFileH
       when (backup opts) (renameFile inFileP $ addExtension inFileP "bak")
       copyFile outFileP inFileP
       removeFile outFileP
            
  
  
  where
    inputEncoding = input_encoding opts
    outputEncoding = output_encoding opts
                     
    identString =
      case spaces opts of
        Just i -> replicate i ' '
        _               -> "\t"

main :: IO ()
main = do
  stdin_isatty  <- hIsTerminalDevice stdin
  stdout_isatty <- hIsTerminalDevice stdout
  let inPlace = stdin_isatty && stdout_isatty
  
  opts <- cmdArgs =<< opts'
  checkOptions opts
  let inFileSources = if stdin_isatty && null (inFileNames opts)
                      then error "No input data.\nUse '--help' command line flag to see the usage case."
                      else if null (inFileNames opts)
                           then ["-"]
                           else inFileNames opts

  mapM_ (processOneSource opts) inFileSources 
      
