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




copyFile inH outH = do
  x <- hGetContents inH
  hPutStr outH x
  

parseDoc inH outH = do
  x <- hGetContents inH
  hPutStr outH $ parseDoc' x saveFunc
    where
      saveFunc :: String -> IO ()
      saveFunc = do hPutStr outH
    
parseDoc' src saveF =
  let (elems, xs) = saxParse "xxx.xml" src
  in
    printElems elems
    --parseDoc' xs saveF
    where
      printElems ((SaxComment a):xs) = do
        saveF a
        x <- printElems xs
        return x
          
       
  

main = do
  (opts, inFileName) <- getOptions
  inFileH <- case inFileName of
                  Nothing -> return stdin
                  Just fn -> openFile fn ReadMode
      
  hSetBinaryMode inFileH True
  hSetBinaryMode stdout True
  
  copyFile inFileH stdout
       
       