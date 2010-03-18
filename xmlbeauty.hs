module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Maybe ( fromMaybe )
import System.IO

version = "2.0"

data Flag = Backup | Encoding String| Quiet | Help | Version
          deriving (Show, Eq)

data ExitReason = Success | ShowHelpOrVersion | UnknownEncoding | UnknownError

exitFor Success           = exitWith $ ExitSuccess
exitFor ShowHelpOrVersion = exitWith $ ExitSuccess
exitFor UnknownEncoding   = exitWith $ ExitFailure (-2)
exitFor UnknownError      = exitWith $ ExitFailure (-1)

getOptions :: IO [Flag]
getOptions =
  do argv <- getArgs
     prog <- getProgName
     opts <- parseOptions argv prog
     
     if Help `elem` opts
       then do let usi = usageInfo (header prog) options
               putStrLn usi
               exitFor ShowHelpOrVersion
       else if Version `elem` opts
              then do putStrLn (prog ++ " version " ++ version)
                      exitFor ShowHelpOrVersion
              else return ()
     
     return opts
     
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
             (opt,[],[]) -> return (opt)
             (_,_,errs@(x:xs))  -> ioError (error ((concat errs) ++ "\n" ++ (usageInfo (header prog) options)))
             (_,tail,[]) -> ioError ( error ("unknown extra parameters: " ++ (concat tail) ++ "\n" ++ usageInfo (header prog) options))


main = do
  catch (
    do opts <- getOptions
       print ">>>>>>>>>>"
       print opts
       print "<<<<<<<<<<"
       exitFor Success
    )
    errorHangler
   where
     errorHangler err = do hPutStrLn stderr (show err)
                           exitFor UnknownError
                          