module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe ( fromMaybe )
import System.IO
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

version = "2.0 (haskell)"

-- | Флаги коммандной строки
data Flag = Backup | Encoding String| Quiet | Help | Version
          deriving (Show, Eq)


getOptions :: IO ([Flag], Maybe FilePath)
getOptions =
  do argv <- getArgs
     prog <- getProgName
     (opts, inFileName) <- parseOptions argv prog
     
     if Help `elem` opts
       then do let usi = usageInfo (header prog) options
               putStrLn usi
               exitWith ExitSuccess
       else if Version `elem` opts
              then do putStrLn (prog ++ " version " ++ version)
                      exitWith ExitSuccess
              else return ()
     
     return (opts, inFileName)
     
  where
    options = [
      Option ['b'] ["backup"]   (NoArg Backup)   "Backup original files",
      Option ['e'] ["encoding"] (OptArg (Encoding . fromMaybe "ISO-8859-1") "ENCODING") "Encoding for output documents (default is iso-8859-1)",
      Option ['q'] ["quiet"]    (NoArg Quiet)    "Be quiet. Do not print warnings",
      Option ['h'] ["help"]     (NoArg Help)     "Show this help message and exit",
      Option ['v'] ["version"]  (NoArg Version)  "Print version information and exit"
      ]
    header progName =
      "Beautifies (makes human readable) xml file(s) inplace.\n" ++
      "usage: " ++ progName ++ " OPTIONS XMLFILE1 [XMLFILE2, ...]\n" ++
      "       " ++ progName ++ " OPTIONS < somefile.xml > somefile.xml"
    parseOptions argv prog = do
           case getOpt Permute options argv of
             (opt,[],[]) -> return (opt, Nothing)
             (opt,x@(inFileName:[]),[]) -> return (opt, Just inFileName)
             (_,extra,errs) -> error $ "unexpected extra arguments: " ++ (concat $ intersperse " " (tail extra)) ++ "\n" ++
                                       (if null errs then "" else (concat $ intersperse "\n" errs) ++ "\n") ++
                                       usageInfo (header prog) options




parseDoc :: Handle -> Handle -> IO ()
parseDoc inH outH = do
  x <- hGetContents inH
  parseDoc' x
  return ()
    where
      parseDoc' :: String -> IO ()
      parseDoc' inpt = do
        let (elms, xxx) = saxParse "unknown.xml" inpt
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
      
  

main :: IO ()
main = do
  (opts, inFileName) <- getOptions
  inFileH <- case inFileName of
                  Nothing -> return stdin
                  Just fn -> openFile fn ReadMode
      
  hSetBinaryMode inFileH True
  hSetBinaryMode stdout True
  
  parseDoc inFileH stdout
