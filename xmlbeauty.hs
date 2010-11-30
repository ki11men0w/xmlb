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

version = "2.0.0.1 (haskell)"

-- | Флаги коммандной строки
data Flag = Backup | Encoding String| Quiet | Help | Version
          deriving (Show, Eq)


programOptions = [
  Option ['b'] ["backup"]   (NoArg Backup)   "Backup original files",
  Option ['e'] ["encoding"] (OptArg (Encoding . fromMaybe "ISO-8859-1") "ENCODING") "Encoding for output documents (default is iso-8859-1)",
  Option ['q'] ["quiet"]    (NoArg Quiet)    "Be quiet. Do not print warnings",
  Option ['h'] ["help"]     (NoArg Help)     "Show this help message and exit",
  Option ['v'] ["version"]  (NoArg Version)  "Print version information and exit"
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




parseDoc :: Handle -> FilePath -> Handle -> IO ()
parseDoc inH inFileName outH = do
  x <- hGetContents inH
  parseDoc' x
  return ()
    where
      parseDoc' :: String -> IO ()
      parseDoc' inpt = do
        let (elms, xxx) = saxParse inFileName inpt
        runStateT printTree (SaxState {ident=0, elems=elms, lastElem = LastElemNothing, saveFunc = saveFunc})
        return ()
          where
            saveFunc :: String -> IO ()
            saveFunc = hPutStr outH




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
                           saveFunc :: String -> IO ()
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

xmlEscape' :: String -> String
xmlEscape' s = s

printElem :: SaxElement -> StateT SaxState IO ()
printElem e = do
  st <- get
  case e of
    x@(SaxProcessingInstruction ("xml", _)) -> do case lastElem st of
                                                    LastElemNothing  -> return ()
                                                    _                -> print' "\n"
                                                  
                                                  printIdent $ showElement x
                                                  print' "\n"
      
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
    
    x@(SaxCharData s)       -> do if (all isSpace s) && lastElemIsNotChar
                                    then return ()
                                    else do print' $ xmlEscape' s
                                            setLastElem LastElemChars
                                    where 
                                      lastElemIsNotChar = case lastElem st of
                                                            LastElemChars -> False
                                                            _             -> True
                                            
    x@(SaxElementTag _ _)   -> do print' "\n"
                                  printIdent (showElement x) 
                                  setLastElem LastElemCloseTag
                                  
    x@(SaxComment s)        -> do print' "\n"
                                  printIdent (showElement x)
                                  print' "\n"
                                  setLastElem LastElemComment
    x@(SaxReference r)      -> do print' $ showElement x
                                  setLastElem LastElemChars
    x                       -> do print' $ showElement x
        


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
  
  parseDoc inFileH inFileDecoratedName outFileH
  
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
      
