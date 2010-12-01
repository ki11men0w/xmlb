module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe ( fromMaybe )
import System.IO
import System.Directory
import System.FilePath
--import System.IO.Error
import Data.List
--import Text.XML.HaXml.Pretty
import Text.XML.HaXml.SAX
import Text.XML.HaXml.Types
import Text.XML.HaXml.Escape
--import Text.XML.HaXml.XmlContent.Parser
--import Control.Monad.Trans
--import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Maybe
import System.IO.Error (catch)
--import Control.Exception (finally)
import Text.Regex.Posix

import Data.Encoding
import qualified Data.ByteString.Lazy as LBS

version = "2.0.0.1 (haskell)"

-- | Флаги коммандной строки
data Flag = Backup | Encoding String| Quiet | Help | Version
          deriving (Show, Eq)

defaultOutputEncoding = "ISO-8859-1"

programOptions :: [OptDescr Flag]
programOptions = [
  Option ['b'] ["backup"]   (NoArg Backup)   "Backup original files.",
  Option ['e'] ["encoding"] (ReqArg Encoding "ENCODING") ("Encoding for output documents (default is " ++ defaultOutputEncoding ++")."),
  Option ['q'] ["quiet"]    (NoArg Quiet)    "Be quiet. Do not print warnings.",
  Option ['h'] ["help"]     (NoArg Help)     "Show this help message and exit.",
  Option ['v'] ["version"]  (NoArg Version)  "Print version information and exit."
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
         | True                -> do return ()
     
     return (opts, inFileNames)
     
  where
    parseOptions argv usi = do
           case getOpt Permute programOptions argv of
             (opt,fileNames,[]) -> return (opt, fileNames)
             (_,_,errs)         -> error $ (concat $ intersperse "\n" errs) ++ "\n" ++ usi




parseDoc :: Handle -> FilePath -> Handle -> String -> IO ()
parseDoc inH inFileName outH outputEncoding = do
  x <- hGetContents inH
  parseDoc' x
  return ()
    where
      parseDoc' :: String -> IO ()
      parseDoc' inpt = do
        let (elms, xxx) = saxParse inFileName inpt
        
        runStateT printTree (SaxState {ident=0, elems=elms, lastElem = LastElemNothing, saveFunc = saveFuncEnc, outputEncoding = outputEncoding})
        
        case xxx of
          Just s -> error s
          _      -> return ()
             
        
        return ()
          where
            saveFuncEnc = (LBS.hPutStr outH) . encodeLazyByteString (encodingFromString outputEncoding)




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
showAttributes [] = []
showAttributes attrs =
  " " ++ (concat $ intersperse " " (showAttributes' attrs))
  where
    showAttributes' :: [Attribute] -> [String]
    showAttributes' [] = []
    showAttributes' (a:as) = ((showAttr a):showAttributes' as)
      where
        showAttr :: Attribute -> String
        showAttr (name, AttValue attrvs) = name ++ "=\"" ++ showAttrValues attrvs ++ "\""
          where
            showAttrValues :: [Either String Reference] -> String
            showAttrValues [] = []
            showAttrValues ((Left value):vs) = value ++ showAttrValues vs
            showAttrValues ((Right value):vs) =
              case value of
                RefEntity name -> "&" ++ name ++ ";"
                RefChar   c    -> "&#" ++ show c ++ ";"
              ++ showAttrValues vs
  

data LastElem = LastElemNothing | LastElemOpenTag | LastElemCloseTag | LastElemChars | LastElemComment
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
  put $ x { ident = (ident x) +1 }

identLess :: StateT SaxState IO ()
identLess = do
  x <- get
  put $ x { ident = (ident x) -1 }
  
justIO :: IO () -> StateT SaxState IO ()
justIO action = do liftIO action

print' :: String -> StateT SaxState IO ()
print' s = do st <- get
              justIO $ (saveFunc st) s

printIdent :: String -> StateT SaxState IO (String)
printIdent s = do i <- getIdent
                  return $ replicate i '\t' ++ s

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

printElem :: SaxElement -> StateT SaxState IO (String)
printElem e = do
  st <- get
  case e of
    x@(SaxProcessingInstruction ("xml", _)) -> do enc <- getOutputEncoding
                                                  let s = case lastElem st of
                                                        LastElemNothing  -> ""
                                                        _                -> "\n"
                                                    
                                                  s' <- printIdent (showSaxProcessingInstruction x enc)
                                                  return $ s ++ s' ++ "\n"
      
    x@(SaxElementOpen _ _)  -> do s' <- printIdent (showElement x)
                                  let s = "\n" ++ s'
                                  identMore
                                  setLastElem LastElemOpenTag
                                  return s
                                  
    x@(SaxElementClose _ )  -> do identLess
                                  s <- case lastElem st of
                                        LastElemChars   -> return ""
                                        LastElemOpenTag -> return ""
                                        _               -> do s <- printIdent ""
                                                              return $ "\n" ++ s
                                  setLastElem LastElemCloseTag
                                  return $ s ++ (showElement x)
    
    x@(SaxCharData s)       -> do if (all isSpace s) && lastElemIsNotChar
                                    then return ""
                                    else do setLastElem LastElemChars
                                            return $ xmlEscape' s
                                            
                                            
                                    where 
                                      lastElemIsNotChar = case lastElem st of
                                                            LastElemChars -> False
                                                            _             -> True
                                            
    x@(SaxElementTag _ _)   -> do s <- printIdent (showElement x) 
                                  setLastElem LastElemCloseTag
                                  return $ "\n" ++ s
                                  
    x@(SaxComment s)        -> do s <- printIdent (showElement x)
                                  setLastElem LastElemComment
                                  return $ "\n" ++ s ++ "\n"
    x@(SaxReference r)      -> do setLastElem LastElemChars
                                  return $ showElement x
    x                       -> do return $ showElement x
        


printTree  :: StateT SaxState IO ()
printTree  = do
  st <- get
  s <- printTree'
  liftIO $ (saveFunc st) s
  
  where
    printTree'  :: StateT SaxState IO (String)
    printTree' = do
      x <- popElem
      case x of
        Nothing -> do st <- get
                      case lastElem st of
                        LastElemCloseTag -> return lastNewLine
                        LastElemComment  -> return lastNewLine
                        _                -> return ""
                        where lastNewLine = "\n"
                      
        Just e  -> do el <- printElem e
                      els <- printTree'
                      return $ el ++ els
          
      

processSources :: [FilePath] -> [Flag] -> IO ()
processSources [] _ = return ()
processSources inFileNames opts = do
  tmpDir <- catch (getTemporaryDirectory) (\_ -> return ".")
  let inFileP = head inFileNames
  let inPlace = inFileP /= "-"
  
  (inFileH, inFileDecoratedName) <- if inFileP == "-"
                                    then return (stdin, "stdin")
                                    else do x <- openFile inFileP ReadMode
                                            return (x, inFileP)
  (outFileP, outFileH) <- do
    if inPlace
      then do x <- openTempFile tmpDir "xmlbeauty.xml" 
              return x  
      else return ("-", stdout)
  
  hSetBinaryMode inFileH True
  hSetBinaryMode outFileH True
  
  parseDoc inFileH inFileDecoratedName outFileH outputEncoding
  
  if inPlace
    then do hClose inFileH
            hClose outFileH
            if Backup `elem` opts
              then do renameFile inFileP $ addExtension inFileP "bak"
              else return ()
            copyFile outFileP inFileP
            removeFile outFileP
            
    else return ()
  
  processSources (tail inFileNames) opts
  
  
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

  processSources inFileSources opts
      
