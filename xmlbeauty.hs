{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment
import System.Console.CmdArgs
import System.IO
import System.IO.Temp
import System.Directory
import System.FilePath
import Control.Monad

import FormatXml

programVersion = "2.1.0.0 (haskell)"

defaultSpaceIdent = 3


-- | Флаги коммандной строки
data Flags = Flags
             { backup :: Bool,
               output_encoding :: Maybe String,
               input_encoding :: Maybe String,
               spaces :: Maybe Int,
               inFileNames :: [String]
             } deriving (Data, Typeable)
                                
opts' = getProgName >>= \programName -> return $
        Flags { backup =
                   def
                   &= help "Backup original files",
                output_encoding =
                  def
                  &= help "Encoding for OUTPUT documents (default is encoding of input document)"
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
        &= details ["Beautifies (makes human readable) xml file(s). If input data is/are common file(s) and no redirection of STDOUT is specified than input files will be changed inplace. If STDIN is not a terminal (e.g. because of redirection) than result will be printed to STDOUT.",
                    "usage: " ++ programName ++ " OPTIONS XMLFILE1 [XMLFILE2 ...]",
                    "       " ++ programName ++ " OPTIONS < somefile.xml > somefile.xml"]


checkOptions opts = do
  hIsTerminalDevice stdin >>= \t ->
    when (not t && (not $ null $ inFileNames opts)) $
      error "As a data source, you must specify either STDIN or file(s) listed in the command line, but not both."
  
  when (backup opts && null (inFileNames opts)) $
    error "--backup option makes sence only when data source is a plain file(s) listed in the command line, not STDIN."
  where
    showOption (x:[]) = '-' : [x]
    showOption x      = "--" ++ x

  

processOneSource :: Flags -> FilePath -> IO ()
processOneSource opts inFileName = do
  let inFileP = inFileName
  
  stdout_isatty <- hIsTerminalDevice stdout
  let inPlace = inFileP /= "-" && stdout_isatty
  
  (inFileH, inFileDecoratedName) <- if inFileP == "-"
                                    then return (stdin, "stdin")
                                    else do x <- openFile inFileP ReadMode
                                            return (x, inFileP)
  
  let parseDoc' outFileH = processFile inFileH inFileDecoratedName outFileH inputEncoding outputEncoding identString
  
  if inPlace
  then
    withSystemTempFile "xmlbeauty.xml" $ \outFileP outFileH -> do
      parseDoc' outFileH
      hClose inFileH
      hClose outFileH
      when (backup opts) (renameFile inFileP $ addExtension inFileP "bak")
      copyFile outFileP inFileP
  else
    parseDoc' stdout
    
  where
    inputEncoding = input_encoding opts
    outputEncoding = output_encoding opts
                     
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
  checkOptions opts
  let inFileSources = case (stdin_isatty, inFileNames opts) of
        (True, []) -> error "No input data.\nUse '--help' command line flag to see the usage case."
        (_, [])    -> ["-"]
        (_, x)     -> x

  mapM_ (processOneSource opts) inFileSources
      
