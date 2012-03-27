{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import System.Console.CmdArgs
--import System.Exit
--import Data.Maybe ( fromMaybe )
import System.IO
import System.IO.Temp
import System.Directory
import System.FilePath
--import System.IO.Error
import Data.List
--import Text.XML.HaXml.Pretty
--import Text.XML.HaXml.SAX
--import Text.XML.HaXml.Types
--import Text.XML.HaXml.Escape
--import Text.XML.HaXml.XmlContent.Parser
--import Control.Monad.Trans
--import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.Char
import Data.Maybe
--import System.IO.Error (catch)
--import Control.Exception (finally)
import Text.Regex.Posix

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as TXT
import Data.Text.Encoding

import Control.Parallel.Strategies (rdeepseq, withStrategy)

import Text.XML.Expat.SAX


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

-- getXmlEncoding :: Handle -> IO (Maybe EncodingName, BS.ByteString)
-- getXmlEncoding inH = do
--   t <- bomTest'
--   case t of
--     (Just e, s)  -> return (Just e, s)
--     (Nothing, s) -> xmlDeclTest s
--   where
--     bomTest' :: IO (Maybe EncodingName, BS.ByteString)
--     bomTest' = do
--       cr <- checkNextByte $ BS.empty
--       case cr of
--         (FullyMatch enc, s) -> return (Just enc, s)
--         (NotMatch, s)       -> return (Nothing, s)
--       where
--         checkNextByte :: BS.ByteString -> IO (BomTestResult, BS.ByteString)
--         checkNextByte s = hIsEOF inH >>= \eof ->
--                           if eof then return (NotMatch, s)
--                           else do
--                             test_str <- BS.hGet inH 1 >>= \c -> return $ BS.concat [s, c]
--                             let bt = bomTest test_str
--                             --hPutStrLn stderr ((show bt) ++ ": '" ++ (concat $ intersperse "," (map (show . ord) (take 20 test_str))) ++ "'")
--                             case bomTest test_str of
--                               x@(NotMatch)     -> return (x, test_str)
--                               x@(FullyMatch e) -> return (x, BS.empty)
--                               x@(SemiMatch _)  -> checkNextByte test_str
          
--           where bomTest :: BS.ByteString -> BomTestResult
--                 bomTest xml =
--                   let utf8'BOM       = BS.pack [0xef, 0xbb, 0xbf]
--                       utf16be'BOM    = BS.pack [0xFE, 0xFF]
--                       utf16le'BOM    = BS.pack [0xFF, 0xFE]
--                       utf32be'BOM    = BS.pack [0x00, 0x00, 0xFE, 0xFF]
--                       utf32le'BOM    = BS.pack [0xFF, 0xFE, 0x00, 0x00]
--                       utf7'BOMstart  = BS.pack [0x2B, 0x2F, 0x76]
--                       utf1'BOM       = BS.pack [0xF7, 0x64, 0x4C]
--                       utfEBCDIC'BOM  = BS.pack [0xDD, 0x73, 0x66, 0x73]
--                       scsu'BOM       = BS.pack [0x0E, 0xFE, 0xFF]
--                       bocu1'BOM      = BS.pack [0xFB, 0xEE, 0x28]
--                       gb18030'BOM    = BS.pack [0x84, 0x31, 0x95, 0x33]
                      
--                       tests = [checkBOM utf8'BOM "UTF-8",
--                                checkBOM utf16be'BOM "UTF-16BE",
--                                checkBOM utf16le'BOM  "UTF-16LE",
--                                checkBOM utf32be'BOM  "UTF-32BE",
--                                checkBOM utf32le'BOM  "UTF-32LE",
--                                check_utf7'BOM,
--                                checkBOM utf1'BOM "UTF-1",
--                                checkBOM utfEBCDIC'BOM "UTF-EBCDIC",
--                                checkBOM scsu'BOM "SCSU",
--                                checkBOM bocu1'BOM "BOCU-1",
--                                checkBOM gb18030'BOM "GB18030"]
                     
--                       checkBOM bom enc = case 1 of
--                             _
--                               | BS.isPrefixOf bom xml -> FullyMatch enc
--                               | BS.isPrefixOf xml bom -> SemiMatch enc
--                               | otherwise -> NotMatch
                          
--                       check_utf7'BOM =
--                             if BS.isPrefixOf xml utf7'BOMstart then
--                               -- Для UTF-7 последний символ BOM может содержать 
--                               -- любой из четырех символов.
--                               let xml' = BS.drop (BS.length utf7'BOMstart) xml
--                               in case 1 of
--                                 _
--                                   -- Проверим что что строка не кончилась на первой части BOM
--                                   | BS.null xml'  -> SemiMatch "UTF-7"
--                                   -- Проверим входит ли наш символ в группу допустимых концов BOM
--                                   | BS.elem (BS.head xml') (BS.pack [0x38, 0x39, 0x2B, 0x2F]) -> FullyMatch "UTF-7"
--                                   | otherwise -> NotMatch
--                             else NotMatch
--                       fully_matched = find (\x -> case x of; FullyMatch enc -> True; otherwithe -> False) tests
--                       semi_matched  = find (\x -> case x of; SemiMatch enc -> True; otherwithe -> False) tests
                      
--                   in case fully_matched of
--                     Just x -> x
--                     Nothing -> case semi_matched of
--                       Just x -> x
--                       Nothing -> NotMatch

--     xmlDeclTest :: BS.ByteString -> IO (Maybe EncodingName, BS.ByteString)
--     xmlDeclTest already_read_str = do
--       -- Считаем что по крайней мере до конца xml-заголовока идут только однобайтовые символы
--       eof <- hIsEOF inH
--       case 1 of
--         _
--           | eof -> return (Nothing, already_read_str)
--           | BS.length already_read_str > 1000 -> return (Nothing, already_read_str)
--           | True -> do new_str <- BS.hGet inH howMatchRead >>= \c -> return $ BS.concat [already_read_str, c]
--                        let test_str = dropWhile isSpace $ C8.unpack new_str
--                            (_, _, _, enc) = test_str =~ "<\\?xml[ \t](.*encoding=\"(.+)\")?.*\\?>" :: (String, String, String, [String])
--                        case enc of
--                          _:"":_     -> return (Nothing, new_str)
--                          _:e:_      -> return (Just e, new_str)
--                          otherwithe -> xmlDeclTest new_str

--             where howMatchRead = if already_read_str_length < min_length
--                                  then min_length - already_read_str_length
--                                  else 1
--                                    where min_length = length "<?xml encoding=\".\"?>"
--                                          already_read_str_length = BS.length already_read_str
                                        


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


type Parsing a = ReaderT ParseConfig (State ParseState) a

data LastElem = LastElemNothing | LastElemXmlProcessingInstruction | LastElemOpenTag | LastElemCloseTag | LastElemChars | LastElemComment

data ParseConfig = ParseConfig { 
                                 outputEncoding :: String,
                                 identString :: String
                               }

type SaxElement = SAXEvent String String

data ParseState = ParseState { identLevel :: Int,
                               elems :: [SaxElement],
                               lastElem :: LastElem,
                               result :: String
                             }

data SaxElementWrapper = SaxElement' SaxElement | SaxError' (Maybe String)

parseDoc :: Handle -> FilePath -> Handle ->Maybe EncodingName -> EncodingName -> String -> IO ()
parseDoc inH inFileName outH inputEncoding outputEncoding identString = do
  
  hSetBinaryMode inH True
  
  inpt' <- LBS.hGetContents inH
  let elms = parseThrowing defaultParseOptions inpt'
  
  withSystemTempFile "xmlbeauty.xml" $ \_ -> \tmpH ->
    do hSetEncoding tmpH =<< mkTextEncoding' outputEncoding
   
       let c = ParseConfig {outputEncoding = getEncodingName4XmlHeader outputEncoding, identString = identString}
           s = ParseState {identLevel=0, elems=elms, lastElem = LastElemNothing, result = ""}
           !x = printTree c s
         
       hPutStr tmpH x
       
       hSeek tmpH AbsoluteSeek 0
       hSetBinaryMode tmpH True
       
       y <- hGetContents tmpH
       hPutStr outH y
       

showElement :: SaxElement -> String
showElement (ProcessingInstruction target  value)      =  "<?" ++ target ++ " " ++ value ++ "?>"
showElement (StartElement name attrs)                  =  "<"  ++ name ++ showAttributes attrs ++ ">"
showElement (EndElement name)                          =  "</" ++ name ++ ">"
showElement (CharacterData s)                          =  s
showElement (Comment a)                                =  "<!--" ++ a ++ "-->"
showElement _                                          =  ""

showXMLDeclaration :: SaxElement -> String -> String
showXMLDeclaration (XMLDeclaration version _ _) encodingName =
  "<?xml version=\"" ++ version ++ "\" encoding=\"" ++ encodingName ++ "\"?>"

type Attribute = (String, String)
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
        showAttr (attrName, attrValue) =
          attrName ++ "=\"" ++ attrValue ++ "\""


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

setLastElem :: LastElem -> Parsing ()
setLastElem le = do
  st <- get
  put $ st {lastElem = le}
  
getOutputEncoding :: Parsing String
getOutputEncoding = do
  cfg <- ask
  return $ outputEncoding cfg

xmlEscape' :: String -> String
xmlEscape' s = s

printElem :: SaxElement -> Parsing ()
printElem e = do
  st <- get
  case e of
    x@(XMLDeclaration name content _) -> do 
      case lastElem st of
        LastElemNothing  -> return ()
        _                -> print' "\n"
                            
      enc <- getOutputEncoding
      printIdent $ showXMLDeclaration x enc
      setLastElem LastElemXmlProcessingInstruction
      
    x@(StartElement _ _)  -> do
      print' "\n"
      printIdent (showElement x) 
      identMore
      setLastElem LastElemOpenTag
                                  
    x@(EndElement _ )  -> do 
      identLess
      case lastElem st of
        LastElemChars   -> return ()
        LastElemOpenTag -> return ()
        _               -> do print' "\n"
                              printIdent ""
      print' $ showElement x
      setLastElem LastElemCloseTag
    
    x@(CharacterData s) -> 
      unless (all isSpace s && lastElemIsNotChar) $
        do print' $ xmlEscape' s
           setLastElem LastElemChars
        where 
          lastElemIsNotChar = case lastElem st of
                                LastElemChars -> False
                                _             -> True
                                            
    x@(Comment s) -> do
      case lastElem st of
        LastElemXmlProcessingInstruction -> unless (isEmacsInstructions s) (print' "\n")
        _ -> print' "\n"
      printIdent (showElement x)
      setLastElem LastElemComment
      
    x                       -> print' $ showElement x
        
    where isEmacsInstructions s = s =~ " -\\*- +.+:.+ -\\*- " :: Bool
    


printTree  :: ParseConfig -> ParseState -> String
printTree cfg st =
  case elems st of
    [] -> case lastElem st of
                 LastElemCloseTag -> lastNewLine
                 LastElemComment  -> lastNewLine
                 _                -> ""
                 where lastNewLine = "\n"
    e:ex -> let st' = flip execState st{elems=ex, result = ""} $
                      flip runReaderT cfg $
                      printElem e
            in result st' ++ printTree cfg st'
    
      

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
      
