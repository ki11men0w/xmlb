{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import System.Console.CmdArgs
--import System.Exit
--import Data.Maybe ( fromMaybe )
import System.IO
import System.Directory
import System.FilePath
--import System.IO.Error
import Data.List
--import Text.XML.HaXml.Pretty
import Text.XML.HaXml.SAX
import Text.XML.HaXml.Types
--import Text.XML.HaXml.Escape
--import Text.XML.HaXml.XmlContent.Parser
--import Control.Monad.Trans
--import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Maybe
--import System.IO.Error (catch)
--import Control.Exception (finally)
import Text.Regex.Posix

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import qualified Data.Text as TXT
import Data.Text.Encoding

import Control.Parallel.Strategies (rdeepseq, withStrategy)

programVersion = "2.0.0.2 (haskell)"

defaultInputEncoding  = "UTF-8"
defaultOutputEncoding = "ISO-8859-1"
defaultSpaceIdent = 3


-- | Флаги коммандной строки
data Flags = Flags
             { backup :: Bool,
               output_encoding :: Maybe String,
               input_encoding :: Maybe String,
               spaces :: Maybe Int,
               inFileNames :: [String]
             } deriving (Show, Data, Typeable)
                                
opts' = getProgName >>= \programName -> return $
        Flags { backup =
                   def
                   &= help "Backup original files",
                output_encoding =
                  def
                  &= help ("Encoding for OUTPUT documents (default is " ++ defaultOutputEncoding ++ ")")
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
        &= details ["Beautifies (makes human readable) xml file(s) inplace.",
                    "usage: " ++ programName ++ " OPTIONS XMLFILE1 [XMLFILE2 ...]",
                    "       " ++ programName ++ " OPTIONS < somefile.xml > somefile.xml"]


type EncodingName = String

data BomTestResult = FullyMatch EncodingName | SemiMatch EncodingName | NotMatch
                   deriving (Show)

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
                            --hPutStrLn stderr ((show bt) ++ ": '" ++ (concat $ intersperse "," (map (show . ord) (take 20 test_str))) ++ "'")
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
                           (_, _, _, enc) = test_str =~ "<\\?xml[ \t](.*encoding=\"(.+)\")?.*\\?>" :: (String, String, String, [String])
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
    "ISO88591" -> return latin1
    "UTF8"     -> return utf8
    "UTF16"    -> return utf16
    "UTF16LE"  -> return utf16le
    "UTF16BE"  -> return utf16be
    "UTF32LE"  -> return utf32le
    "UTF32BE"  -> return utf32be
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
    _         -> en
    
    where normalized = filter (\x -> not $ elem x "_- ") (map toUpper en)  


parseDoc :: Handle -> FilePath -> Handle ->Maybe EncodingName -> EncodingName -> String -> IO ()
parseDoc inH inFileName outH inputEncoding outputEncoding identString = do
  
  hSetBinaryMode inH True
  
  let getInputEncoding :: IO (Maybe EncodingName, BS.ByteString)
      getInputEncoding = case inputEncoding of
        Just e -> return (Just e, BS.empty)
        Nothing -> getXmlEncoding inH
          
  (inputEncodingName', inpt') <- getInputEncoding
  inpt <- case inputEncodingName' of
        Just inputEncodingName -> do
    
          inputEncoding <- mkTextEncoding' inputEncodingName
      
          (tmpName', tmpH') <- do
            tmpDir <- catch getTemporaryDirectory (\_ -> return ".")
            openTempFile tmpDir "xmlbeauty.header" 
          
          !inpt' <-
            if BS.null inpt' then
              do hClose tmpH'
                 return ""
            else
              do hSetBinaryMode tmpH' True
                 BS.hPutStr tmpH' inpt'
                 hSeek tmpH' AbsoluteSeek 0
                 hSetEncoding tmpH' inputEncoding
                 -- Все эти strict* для того что-бы можно было закрыть файл
                 -- tmpName' сразуже
                 (withStrategy rdeepseq) `liftM` hGetContents tmpH'
                     
          hClose tmpH'
          removeFile tmpName'
        
          hSetEncoding inH inputEncoding
          inpt'' <- hGetContents inH
          return $ inpt' ++ inpt''
        
        Nothing -> do
          -- Если кодировку определить не смогли, то считаем, что
          -- входной поток данных в UTF-8. Нам необходимо
          -- раскодировать из UTF-8 не только еще не прочитанную часть
          -- файла, но и небольшую зачитанную заголовачную
          -- часть. Т.к. механизм чтения (Prelude.hGetContents и
          -- hSetEncoding) не позволяет это сделать, то используем
          -- здесь функционал Data.Text.Encoding.
          inpt'' <- BS.hGetContents inH
          return $ TXT.unpack $ decodeUtf8 $ BS.append inpt' inpt''
  
  let (elms, xxx) = saxParse inFileName (inpt)
  
  (tmpName, tmpH) <- do
    tmpDir <- catch getTemporaryDirectory (\_ -> return ".")
    openTempFile tmpDir "xmlbeauty.xml" 
  
  hSetEncoding tmpH =<< mkTextEncoding' outputEncoding

  runStateT printTree SaxState {identLevel=0, elems=elms, lastElem = LastElemNothing, saveFunc = hPutStr tmpH, outputEncoding = getEncodingName4XmlHeader outputEncoding, identString = identString}
  hSeek tmpH AbsoluteSeek 0
  hSetBinaryMode tmpH True

  y <- hGetContents tmpH
  hPutStr outH y
  hClose tmpH
  removeFile tmpName
    
  case xxx of
    Just s -> error s
    _      -> return ()
       

showElement :: SaxElement -> String
showElement (SaxProcessingInstruction (target, value)) =  "<?" ++ target ++ " " ++ value ++ "?>"
showElement (SaxElementOpen name attrs)                =  "<"  ++ name ++ showAttributes attrs ++ ">"
showElement (SaxElementClose name)                     =  "</" ++ name ++ ">"
showElement (SaxElementTag name attrs)                 =  "<"  ++ name ++ showAttributes attrs ++ "/>"
showElement (SaxCharData s)                            =  s
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
            showAttrValues (Left str : vs)  = str ++ showAttrValues vs
            showAttrValues (Right ref : vs) =
              case ref of
                RefEntity name -> "&" ++ name ++ ";"
                RefChar   c    -> "&#" ++ show c ++ ";"
              ++ showAttrValues vs
  

data LastElem = LastElemNothing | LastElemXmlProcessingInstruction | LastElemOpenTag | LastElemCloseTag | LastElemChars | LastElemComment
data SaxState = SaxState { identLevel :: Int,
                           elems :: [SaxElement],
                           lastElem :: LastElem,
                           saveFunc :: String -> IO (),
                           outputEncoding :: String,
                           identString :: String
                         }

setIdent :: Int -> StateT SaxState IO ()
setIdent x = do
  z <- get
  put $ z { identLevel = x }

getIdent :: StateT SaxState IO Int
getIdent = do
  x <- get
  return $ identLevel x
  
identMore :: StateT SaxState IO ()
identMore = do
  x <- get
  put $ x { identLevel = identLevel x +1 }

identLess :: StateT SaxState IO ()
identLess = do
  x <- get
  put $ x { identLevel = identLevel x -1 }
  
justIO :: IO () -> StateT SaxState IO ()
justIO = liftIO

print' :: String -> StateT SaxState IO ()
print' s = do st <- get
              justIO $ saveFunc st s

printIdent :: String -> StateT SaxState IO ()
printIdent s = do x <- get
                  --print' $ replicate (identLevel x) '\t' ++ s
                  print' $ concat (take (identLevel x) (repeat (identString x))) ++ s

popElem :: StateT SaxState IO (Maybe SaxElement)
popElem = do
  x <- get
  case elems x of
    []     -> return Nothing
    (h:hs) -> do put x { elems = hs }
                 return $ Just h
                 
setLastElem :: LastElem -> StateT SaxState IO ()
setLastElem le = do
  st <- get
  put $ st {lastElem = le}
  
getOutputEncoding :: StateT SaxState IO String
getOutputEncoding = do
  st <- get
  return $ outputEncoding st

xmlEscape' :: String -> String
xmlEscape' s = s

printElem :: SaxElement -> StateT SaxState IO ()
printElem e = do
  st <- get
  case e of
    x@(SaxProcessingInstruction ("xml", _)) -> do case lastElem st of
                                                    LastElemNothing  -> return ()
                                                    _                -> print' "\n"
                                                  
                                                  enc <- getOutputEncoding
                                                  printIdent $ showSaxProcessingInstruction x enc
                                                  setLastElem LastElemXmlProcessingInstruction
      
    x@(SaxElementOpen _ _)  -> do print' "\n"
                                  printIdent (showElement x) 
                                  identMore
                                  setLastElem LastElemOpenTag
                                  
    x@(SaxElementClose _ )  -> do identLess
                                  case lastElem st of
                                    LastElemChars   -> return ()
                                    LastElemOpenTag -> return ()
                                    _               -> do print' "\n"
                                                          printIdent ""
                                  print' $ showElement x
                                  setLastElem LastElemCloseTag
    
    x@(SaxCharData s)       -> unless (all isSpace s && lastElemIsNotChar) $
                                 do print' $ xmlEscape' s
                                    setLastElem LastElemChars
                                 where 
                                   lastElemIsNotChar = case lastElem st of
                                                         LastElemChars -> False
                                                         _             -> True
                                            
    x@(SaxElementTag _ _)   -> do print' "\n"
                                  printIdent (showElement x) 
                                  setLastElem LastElemCloseTag
                                  
    x@(SaxComment s)        -> do case lastElem st of
                                    LastElemXmlProcessingInstruction -> unless (isEmacsInstructions s) (print' "\n")
                                    _ -> print' "\n"
                                  printIdent (showElement x)
                                  setLastElem LastElemComment
    x@(SaxReference r)      -> do print' $ showElement x
                                  setLastElem LastElemChars
    x                       -> print' $ showElement x
        
    where isEmacsInstructions s = s =~ " -\\*- +.+:.+ -\\*- " :: Bool
    


printTree  :: StateT SaxState IO ()
printTree  = do
  x <- popElem
  case x of
    Nothing -> do st <- get
                  case lastElem st of
                    LastElemCloseTag -> lastNewLine
                    LastElemComment  -> lastNewLine
                    _                -> return ()
                    where lastNewLine = print' "\n"
                  
    Just e  -> do printElem e
                  printTree
      

processOneSource :: Flags -> FilePath -> IO ()
processOneSource opts inFileName = do
  tmpDir <- catch getTemporaryDirectory (\_ -> return ".")
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
    outputEncoding = fromMaybe defaultOutputEncoding (output_encoding opts)
    inputEncoding = input_encoding opts
                     
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
  let inFileSources = if stdin_isatty && null (inFileNames opts)
                      then error "No input data.\nUse '--help' command line flag to see the usage case."
                      else if null (inFileNames opts)
                           then ["-"]
                           else inFileNames opts

  mapM_ (processOneSource opts) inFileSources 
      
