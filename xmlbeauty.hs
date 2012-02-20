{-# LANGUAGE DeriveDataTypeable #-}
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
                  &= help "Encoding for INPUT documents. If not specified then try to realize it from document"
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

getXmlEncoding' :: Handle -> IO (EncodingName, String)
getXmlEncoding' inH = do
  t <- bomTest'
  case t of
    (Just e, s)  -> return (e, s)
    (Nothing, s) -> xmlDeclTest s >>= \t ->
                    case t of
                      (Just e, s)  -> return (e, s)
                      (Nothing, s) -> return ("ISO-8859-1", s) -- т.к. мы читали побайтно, то необходимо вернуть однобайтную кодировку
  where
    bomTest' :: IO (Maybe EncodingName, String)
    bomTest' = do
      cr <- checkNextByte ""
      case cr of
        (FullyMatch enc, s) -> return (Just enc, s)
        (NotMatch, s)       -> return (Nothing, s)
      where
        checkNextByte :: String -> IO (BomTestResult, String)
        checkNextByte s = hIsEOF inH >>= \eof ->
                          if eof then return (NotMatch, s)
                          else do
                            test_str <- hGetChar inH >>= \c -> return $ s ++ [c]
                            let bt = bomTest test_str
                            --hPutStrLn stderr ((show bt) ++ ": '" ++ (concat $ intersperse "," (map (show . ord) (take 20 test_str))) ++ "'")
                            case bomTest test_str of
                              x@(NotMatch)     -> return (x, test_str)
                              x@(FullyMatch e) -> return (x, "")
                              x@(SemiMatch _)  -> checkNextByte test_str
          
          where bomTest :: String -> BomTestResult
                bomTest xml =
                  let utf8'BOM       = map chr [0xef, 0xbb, 0xbf]
                      utf16be'BOM    = map chr [0xFE, 0xFF]
                      utf16le'BOM    = map chr [0xFF, 0xFE]
                      utf32be'BOM    = map chr [0x00, 0x00, 0xFE, 0xFF]
                      utf32le'BOM    = map chr [0xFF, 0xFE, 0x00, 0x00]
                      utf7'BOMstart  = map chr [0x2B, 0x2F, 0x76]
                      utf1'BOM       = map chr [0xF7, 0x64, 0x4C]
                      utfEBCDIC'BOM  = map chr [0xDD, 0x73, 0x66, 0x73]
                      scsu'BOM       = map chr [0x0E, 0xFE, 0xFF]
                      bocu1'BOM      = map chr [0xFB, 0xEE, 0x28]
                      gb18030'BOM    = map chr [0x84, 0x31, 0x95, 0x33]
                      
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
                              | bom `isPrefixOf` xml -> FullyMatch enc
                              | xml `isPrefixOf` bom -> SemiMatch enc
                              | otherwise -> NotMatch
                          
                      check_utf7'BOM =
                            if xml `isPrefixOf` utf7'BOMstart then
                              -- Для UTF-7 последний символ BOM может содержать 
                              -- любой из четырех символов.
                              let xml' = drop (length utf7'BOMstart) xml
                              in case 1 of
                                _
                                  -- Проверим что что строка не кончилась на первой части BOM
                                  | null xml'  -> SemiMatch "UTF-7"
                                  -- Проверим входит ли наш символ в группу допустимых концов BOM
                                  | head xml' `elem` map chr [0x38, 0x39, 0x2B, 0x2F] -> FullyMatch "UTF-7"
                                  | otherwise -> NotMatch
                            else NotMatch
                      fully_matched = find (\x -> case x of; FullyMatch enc -> True; otherwithe -> False) tests
                      semi_matched  = find (\x -> case x of; SemiMatch enc -> True; otherwithe -> False) tests
                      
                  in case fully_matched of
                    Just x -> x
                    Nothing -> case semi_matched of
                      Just x -> x
                      Nothing -> NotMatch

    xmlDeclTest :: String -> IO (Maybe EncodingName, String)
    xmlDeclTest already_read_str = do
      -- Считаем что по крайней мере до конца xml-заголовока идут только однобайтовые символы
      eof <- hIsEOF inH
      case 1 of
        _
          | eof -> return (Nothing, already_read_str)
          | length already_read_str > 1000 -> return (Nothing, already_read_str)
          | True -> do new_str <- hGetChar inH >>= \c -> return $ already_read_str ++ [c]
                       let test_str = dropWhile isSpace new_str
                           (_, _, _, enc) = test_str =~ "<\\?xml .*encoding=\"(.+)\".*\\?>" :: (String, String, String, [String])
                       case enc of
                         e:es       -> return (Just e, new_str)
                         otherwithe -> xmlDeclTest new_str
                                        
              

getXmlEncoding :: String -> EncodingName
getXmlEncoding xml =
  case bomTest of
    Just s -> s
    _      -> case xmlDeclTest of
                Just s -> s
                _      -> defaultInputEncoding
                
    where bomTest :: Maybe String
          bomTest =
            let utf8'BOM       = map chr [0xef, 0xbb, 0xbf]
                utf16be'BOM    = map chr [0xFE, 0xFF]
                utf16le'BOM    = map chr [0xFF, 0xFE]
                utf32be'BOM    = map chr [0x00, 0x00, 0xFE, 0xFF]
                utf32le'BOM    = map chr [0xFF, 0xFE, 0x00, 0x00]
                utf7'BOMstart  = map chr [0x2B, 0x2F, 0x76]
                utf1'BOM       = map chr [0xF7, 0x64, 0x4C]
                utfEBCDIC'BOM  = map chr [0xDD, 0x73, 0x66, 0x73]
                scsu'BOM       = map chr [0x0E, 0xFE, 0xFF]
                bocu1'BOM      = map chr [0xFB, 0xEE, 0x28]
                gb18030'BOM    = map chr [0x84, 0x31, 0x95, 0x33]
                
            in case 1 of
              _ 
                | checkBOM utf8'BOM      -> Just "UTF-8"
                | checkBOM utf16be'BOM   -> Just "UTF-16BE"
                | checkBOM utf16le'BOM   -> Just "UTF-16LE"
                | checkBOM utf32be'BOM   -> Just "UTF-32BE"
                | checkBOM utf32le'BOM   -> Just "UTF-32LE"
                | checkBOM utf7'BOMstart ->
                        -- Для UTF-7 последний символ BOM может содержать 
                        -- любой из четырех символов.
                        let xml' = drop (length utf7'BOMstart) xml
                        in case 1 of
                          _
                            -- Проверим что что строка не кончилась на первой части BOM
                            | null xml'  -> Nothing
                            -- Проверим входит ли наш символ в группу допустимых концов BOM
                            | head xml' `elem` map chr [0x38, 0x39, 0x2B, 0x2F]    -> Just "UTF-7"
                            | otherwise                                    -> Nothing
                
                | checkBOM utf1'BOM      -> Just "UTF-1"
                | checkBOM utfEBCDIC'BOM -> Just "UTF-EBCDIC"
                | checkBOM scsu'BOM      -> Just "SCSU"
                | checkBOM bocu1'BOM     -> Just "BOCU-1"
                | checkBOM gb18030'BOM   -> Just "GB18030"

                | otherwise -> Nothing
              
              where checkBOM bom = bom `isPrefixOf` xml
                  
          
          xmlDeclTest :: Maybe String
          xmlDeclTest =
            -- Считаем что по крайней мере до конца xml-заголовока идут только однобайтовые символы
            let xml'' = dropWhile isSpace $ take 1000 xml
                (_, _, _, enc) = xml'' =~ "<\\?xml .*encoding=\"(.+)\".*\\?>" :: (String, String, String, [String])
            in
             case enc of
               e:es      -> Just e
               otherwise -> Nothing
               


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
  
  let getInputEncoding :: IO (EncodingName, String)
      getInputEncoding = case inputEncoding of
        Just e -> return (e, "")
        Nothing -> do --inpt <- foldl (>>=) (return "") (take 1000 (repeat (\x -> hGetChar inH >>= \y -> return $ x ++ [y])))
                      (e,s) <- getXmlEncoding' inH
                      -- error $ "enc:" ++ e ++ " str:'" ++ s ++ "'"
                      return (e,s)
          
  (inputEncodingName, inpt') <- getInputEncoding
  inputEncoding <- mkTextEncoding' inputEncodingName
  
  (tmpName', tmpH') <- do
    tmpDir <- catch getTemporaryDirectory (\_ -> return ".")
    openTempFile tmpDir "xmlbeauty.header" 
  
  inpt'' <-
    if null inpt' then
      do hClose tmpH'
         return ""
    else
      do hSetBinaryMode tmpH' True
         hPutStr tmpH' inpt'
         hSeek tmpH' AbsoluteSeek 0
         hSetEncoding tmpH' inputEncoding
         hGetContents tmpH'
  

  hSetEncoding inH inputEncoding
  inpt <- hGetContents inH
  
  --hPutStrLn stderr (concat $ intersperse "," (map (show . ord) (take 20 inpt'')))
  
  let (elms, xxx) = saxParse inFileName (inpt'' ++ inpt)
  
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
  hClose tmpH'
  removeFile tmpName'
    
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
  let (pre, match, post) = value =~ "encoding=\"[^\"]+\"" :: (String, String, String)
  in "<?" ++ target ++ " " ++ pre ++ "encoding=\"" ++ encodingName ++ "\"" ++ post ++ "?>"

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
                      then error "No input data"
                      else if null (inFileNames opts)
                           then ["-"]
                           else inFileNames opts

  mapM_ (processOneSource opts) inFileSources 
      
