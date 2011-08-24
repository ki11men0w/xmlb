module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
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
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Maybe
--import System.IO.Error (catch)
--import Control.Exception (finally)
import Text.Regex.Posix

import Data.Encoding
import Codec.Text.IConv as IConv
--import qualified Data.Encoding.ISO88591 as ISO88591
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

version = "2.0.0.1 (haskell)"

-- | Флаги коммандной строки
data Flag = Backup | Encoding String| Quiet | Help | Version
          deriving (Show, Eq)

defaultInputEncoding  = "UTF-8"
defaultOutputEncoding = "ISO-8859-1"

programOptions :: [OptDescr Flag]
programOptions = [
  Option "b" ["backup"]   (NoArg Backup)   "Backup original files.",
  Option "e" ["encoding"] (ReqArg Encoding "ENCODING") ("Encoding for output documents (default is " ++ defaultOutputEncoding ++")."),
  Option "q" ["quiet"]    (NoArg Quiet)    "Be quiet. Do not print warnings.",
  Option "h" ["help"]     (NoArg Help)     "Show this help message and exit.",
  Option "v" ["version"]  (NoArg Version)  "Print version information and exit."
  ]

programUsageInfo = do
  header <- getHeader
  return $ usageInfo header programOptions
    where
      getHeader = do
        progName <- getProgName
        return $ "Beautifies (makes human readable) xml file(s) inplace.\n" ++
                 "usage: " ++ progName ++ " OPTIONS XMLFILE1 [XMLFILE2, ...]\n" ++
                 "       " ++ progName ++ " OPTIONS < somefile.xml > somefile.xml"
  
getOptions :: IO ([Flag], [FilePath])
getOptions =
  do argv <- getArgs
     prog <- getProgName
     usi <- programUsageInfo
     (opts, inFileNames) <- parseOptions argv usi
     
     case 1 of
       _ 
         | Help    `elem` opts -> do putStrLn usi
                                     exitWith ExitSuccess
         | Version `elem` opts -> do putStrLn (prog ++ " version " ++ version)
                                     exitWith ExitSuccess
         | True                -> return ()
     
     return (opts, inFileNames)
     
  where
    parseOptions argv usi =
           case getOpt Permute programOptions argv of
             (opt,fileNames,[]) -> return (opt, fileNames)
             (_,_,errs)         -> error $ concat (intersperse "\n" errs) ++ "\n" ++ usi



getXmlEncoding :: LBS.ByteString -> String
getXmlEncoding xml =
  case bomTest of
    Just s -> s
    _      -> case xmlDeclTest of
                Just s -> s
                _      -> defaultInputEncoding
                
    where bomTest :: Maybe String
          bomTest =
            let utf8'BOM       = LBS.pack [0xef, 0xbb, 0xbf]
                utf16be'BOM    = LBS.pack [0xFE, 0xFF]
                utf16le'BOM    = LBS.pack [0xFF, 0xFE]
                utf32be'BOM    = LBS.pack [0x00, 0x00, 0xFE, 0xFF]
                utf32le'BOM    = LBS.pack [0xFF, 0xFE, 0x00, 0x00]
                utf7'BOMstart  = LBS.pack [0x2B, 0x2F, 0x76]
                utf1'BOM       = LBS.pack [0xF7, 0x64, 0x4C]
                utfEBCDIC'BOM  = LBS.pack [0xDD, 0x73, 0x66, 0x73]
                scsu'BOM       = LBS.pack [0x0E, 0xFE, 0xFF]
                bocu1'BOM      = LBS.pack [0xFB, 0xEE, 0x28]
                gb18030'BOM    = LBS.pack [0x84, 0x31, 0x95, 0x33]
                
            in case 1 of
              _ 
                | checkBOM utf8'BOM      -> Just "UTF-8"
                | checkBOM utf16be'BOM   -> Just "UTF-16"
                | checkBOM utf16le'BOM   -> Just "UTF-16"
                | checkBOM utf32be'BOM   -> Just "UTF-32"
                | checkBOM utf32le'BOM   -> Just "UTF-32"
                | checkBOM utf7'BOMstart ->
                        -- Для UTF-7 последний символ BOM может содержать 
                        -- любой из четырех символов.
                        let xml' = LBS.drop (LBS.length utf7'BOMstart) xml
                        in case 1 of
                          _
                            -- Проверим что что строка не кончилась на первой части BOM
                            | LBS.null xml'  -> Nothing
                            -- Проверим входит ли наш символ в группу допустимых концов BOM
                            | LBS.head xml' `elem` [0x38, 0x39, 0x2B, 0x2F]    -> Just "UTF-7"
                            | otherwise                                       -> Nothing
                
                | checkBOM utf1'BOM      -> Just "UTF-1"
                | checkBOM utfEBCDIC'BOM -> Just "UTF-EBCDIC"
                | checkBOM scsu'BOM      -> Just "SCSU"
                | checkBOM bocu1'BOM     -> Just "BOCU-1"
                | checkBOM gb18030'BOM   -> Just "GB18030"

                | otherwise -> Nothing
              
              where checkBOM bom = bom `LBS.isPrefixOf` xml
                  
          
          xmlDeclTest :: Maybe String
          xmlDeclTest =
            -- Считаем что по крайней мере до конца xml-заголовока идут только однобайтовые символы
            let xml'  = decodeLazyByteString (encodingFromString "ISO-8859-1") (LBS.take 1000 xml)
                xml'' = dropWhile isSpace xml'
                (_, _, _, enc) = xml'' =~ "<\\?xml .*encoding=\"(.+)\".*\\?>" :: (String, String, String, [String])
            in
             case enc of
               e:es      -> Just e
               otherwise -> Nothing
               

decode_ :: IConv.EncodingName -> LBS.ByteString -> String
decode_ fromEncoding input =
  ucs2_to_string input'
  where input' = IConv.convert fromEncoding "UCS-2LE" input
        ccc = LBS.cons
        ucs2_to_string :: LBS.ByteString -> String
        ucs2_to_string null = []
        ucs2_to_string input = let f = LBS.head input
                                   s = LBS.head $ LBS.tail input
                                   x = (fromIntegral(f) + (fromIntegral(s) * 256))::Int
                               in toEnum(x):ucs2_to_string(LBS.drop 2 input)
          

  
  

parseDoc :: Handle -> FilePath -> Handle -> String -> IO ()
parseDoc inH inFileName outH outputEncoding = do
  x <- LBS.hGetContents inH
  parseDoc' x
    where
      parseDoc' :: LBS.ByteString -> IO ()
      parseDoc' inpt = do
        
--        let (elms, xxx) = saxParse inFileName (decodeStrictByteString (encodingFromString $ getXmlEncoding inpt) inpt)
        let (elms, xxx) = saxParse inFileName (decode_ (getXmlEncoding inpt) inpt)
        
        (tmpName, tmpH) <- do
          tmpDir <- catch getTemporaryDirectory (\_ -> return ".")
          openBinaryTempFile tmpDir "xmlbeauty.xml" 
        
        runStateT printTree SaxState {ident=0, elems=elms, lastElem = LastElemNothing, saveFunc = hPutStr tmpH, outputEncoding = outputEncoding}
        hSeek tmpH AbsoluteSeek 0
        y <- hGetContents tmpH
        saveFuncEnc y
        hClose tmpH
        removeFile tmpName
          
        case xxx of
          Just s -> error s
          _      -> return ()
             
        
          where
            saveFuncEnc = LBS.hPutStr outH . encodeLazyByteString (encodingFromString outputEncoding)




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
data SaxState = SaxState { ident :: Int,
                           elems :: [SaxElement],
                           lastElem :: LastElem,
                           saveFunc :: String -> IO (),
                           outputEncoding :: String
                         }

setIdent :: Int -> StateT SaxState IO ()
setIdent x = do
  z <- get
  put $ z { ident = x }

getIdent :: StateT SaxState IO Int
getIdent = do
  x <- get
  return $ ident x
  
identMore :: StateT SaxState IO ()
identMore = do
  x <- get
  put $ x { ident = ident x +1 }

identLess :: StateT SaxState IO ()
identLess = do
  x <- get
  put $ x { ident = ident x -1 }
  
justIO :: IO () -> StateT SaxState IO ()
justIO = liftIO

print' :: String -> StateT SaxState IO ()
print' s = do st <- get
              justIO $ saveFunc st s

printIdent :: String -> StateT SaxState IO ()
printIdent s = do i <- getIdent
                  print' $ replicate i '\t' ++ s

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
      

processOneSource :: [Flag] -> FilePath -> IO ()
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
  
  parseDoc inFileH inFileDecoratedName outFileH outputEncoding
  
  when inPlace $
    do hClose inFileH
       hClose outFileH
       when (Backup `elem` opts) (renameFile inFileP $ addExtension inFileP "bak")
       copyFile outFileP inFileP
       removeFile outFileP
            
  
  
  where
    outputEncoding = case find isEncoding opts of
      Just (Encoding enc) -> enc
      _                   -> defaultOutputEncoding
    isEncoding x = case x of
      Encoding _ -> True
      _          -> False
        

main :: IO ()
main = do
  
  usi <- programUsageInfo
  
  stdin_isatty  <- hIsTerminalDevice stdin
  stdout_isatty <- hIsTerminalDevice stdout
  let inPlace = stdin_isatty && stdout_isatty
  
  (opts, inFileNames) <- getOptions
  let inFileSources = if stdin_isatty && null inFileNames 
                      then error $ "No input data\n" ++ usi
                      else if null inFileNames
                           then ["-"]
                           else inFileNames

  mapM_ (processOneSource opts) inFileSources 
      
