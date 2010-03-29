module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe ( fromMaybe )
import System.IO
import System.IO.Error
import Data.List
--import Text.XML.HaXml.Pretty
import Text.XML.HaXml.SAX
--import Text.XML.HaXml.Types
--import Text.XML.HaXml.XmlContent.Parser
import Control.Monad.Trans
import Control.Monad

import Data.Char
import Data.Maybe

version = "2.0"

data Flag = Backup | Encoding String| Quiet | Help | Version
          deriving (Show, Eq)

type FileName = Maybe FilePath

getOptions :: IO ([Flag], FileName)
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



type ParseState = Int


copyFile inH outH = do
  x <- hGetContents inH
  hPutStr outH x
  

parseDoc :: Handle -> Handle -> IO ()
parseDoc inH outH = do
  x <- hGetContents inH
  parseDoc' x saveFunc
    where
      saveFunc :: String -> IO ()
      saveFunc = hPutStr outH
    
parseDoc' :: String -> (String -> IO ()) -> IO ()
parseDoc' src saveF =
  let (elems, xs) = saxParse "xxx.xml" src
  in
    printElems elems 0
    where
      printElems [] _ = return ()
      printElems (x:xs) state = do st <- printElem x state
                                   printElems xs st
                                   return ()
        where
          printElem :: SaxElement -> Int -> IO Int
          printElem (SaxProcessingInstruction (target, value)) st = do
            saveF $ "<?" ++ target ++ " " ++ value ++ "?>\n"
            return st
          printElem (SaxElementOpen name attrs) st = do
            saveF $ "<" ++ name ++ ">"
            return st
          printElem (SaxElementClose name) st = do
            saveF $ "</" ++ name ++ ">"
            return st
          printElem (SaxElementTag name attrs) st = do
            saveF $ "<" ++ name ++ "/>"
            return st
          printElem (SaxCharData s) st = do
            saveF $ s
            return st
          printElem (SaxComment a) st = do
            saveF $ "<!--" ++ a ++ "-->"
            return st
          printElem _ st =
            return st





newtype IdentIO a = IdentIO { runWithIdent :: Int -> IO (Int, a) } 
instance Monad IdentIO where
  return a            = IdentIO $ \i -> return (i, a)
  (IdentIO r) >>= f   = IdentIO $ \i -> do (i', a) <- r i
                                           runWithIdent (f a) i'
                                        

newtype (Monad m) => IdentIO_T m a = IdentIO_T { runIdentIO_T :: m (IdentIO a)}
instance Monad m => Monad (IdentIO_T m)  where
  return   = IdentIO_T . return . return
  x >>= f  = IdentIO_T $ do (IdentIO r) <- runIdentIO_T x
                            return $ IdentIO $ \i -> do (i', a) <- r i
                                                        x <- runIdentIO_T (f a)
                                                        runWithIdent x i'

                            
-- instance MonadTrans IdentIO where
--   lift = IdentIO_T . (liftM Just)


setIdent   new_i = IdentIO $ \i -> return (new_i, ())
justIO    action = IdentIO $ \i -> do action; return (i, ())
identMore        = IdentIO $ \i -> return (i + 1, ())
identLess        = IdentIO $ \i -> return (i - 1, ())
getIdent         = IdentIO $ \i -> return (i, i)
printIdent s     = do i <- getIdent
                      justIO $ putStr $ replicate i '\t' ++ s
                      

walk :: (Monad m) => (SaxElement -> m a) -> [SaxElement] -> m ()
walk f [] = return () 
walk f (x:xs) = do 
  f x
  walk f xs
  return ()
       

showElement :: SaxElement -> String
showElement (SaxProcessingInstruction (target, value)) =  "<?" ++ target ++ " " ++ value ++ "?>\n"
showElement (SaxElementOpen name attrs)                =  "<"  ++ name ++ ">"
showElement (SaxElementClose name)                     =  "</" ++ name ++ ">\n"
showElement (SaxElementTag name attrs)                 =  "<"  ++ name ++ "/>\n"
showElement (SaxCharData s)                            =  s
showElement (SaxComment a)                             =  "<!--" ++ a ++ "-->\n"
showElement _                                          =  []

printTree :: [SaxElement] -> IdentIO ()  
printTree = do walk p
  where 
    p x@(SaxProcessingInstruction ("xml", _)) = do setIdent 0
                                                   printIdent (showElement x)
    p x@(SaxElementOpen _ _)  = do printIdent (showElement x)
                                   identMore
    p x@(SaxElementClose _ )  = do printIdent (showElement x)
                                   identLess
    p x                       = do printIdent (showElement x)
          


--parseDoc2 :: Handle -> Handle -> IO ()
parseDoc2 inH outH = do
  x <- hGetContents inH
  parseDoc2' x
  return ()
    where
      parseDoc2' :: String -> IO ()
      parseDoc2' inpt = do
        let (elems, xxx) = saxParse "xxx.xml" inpt
        --sequence $ [printTree elems]
        return ()
          where
            --saveFunc :: String -> IO ()
            saveFunc = hPutStr outH



getPassword :: IO (Maybe String)
getPassword = do s <- getLine
                 if isValid s
                   then return $ Just s
                   else return Nothing
                        
isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s


askPassword :: IO ()
askPassword = do putStrLn "Enter password: "
                 x <- getPassword
                 if isJust x
                   then putStrLn "Ok"
                   else return ()



main = do
  (opts, inFileName) <- getOptions
  inFileH <- case inFileName of
                  Nothing -> return stdin
                  Just fn -> openFile fn ReadMode
      
  hSetBinaryMode inFileH True
  hSetBinaryMode stdout True
  
  parseDoc2 inFileH stdout
  --Just x <- getPassword
  --putStr x
  return ()
       
       