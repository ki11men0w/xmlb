{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment
import System.Console.CmdArgs
import System.IO
import System.IO.Temp
import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe
import System.FilePath.Glob

import FormatXml

import Paths_xmlb (version)
import Data.Version (showVersion)

programVersion :: String
programVersion =
  showVersion version ++ " (haskell)"

defaultSpaceIndent :: Int
defaultSpaceIndent = 3


-- | Флаги коммандной строки
data Flags = Flags
             { backup :: Bool,
               output_encoding :: Maybe EncodingName,
               input_encoding :: Maybe EncodingName,
               spaces :: Maybe Int,
               strip :: Bool,
               legacy :: Bool,
               quiet :: Bool,
               inFileNames :: [FilePath]
             } deriving (Data, Typeable)
                                
opts' :: IO Flags
opts' = getProgName >>= \programName -> return $
        Flags { backup =
                   def
                   &= help "Backup original files",
                output_encoding =
                  def
                  &= help "Encoding for OUTPUT documents (default is encoding of an input document)"
                  &= explicit &= name "e" &= name "encoding" &= name "o"
                  &= typ  "ENC",
                input_encoding =
                  def
                  &= help "Encoding for INPUT documents. If not specified then the content of an input XML-document will be used to realize its encoding. If all failed then UTF-8 will be used"
                  &= typ  "ENC",
                spaces =
                  def
                  &= help ("Use this number of spaces instead of tabs for indentation (default is " ++ show defaultSpaceIndent ++ ")")
                  &= opt (show defaultSpaceIndent),
                strip =
                  def
                  &= help "Strip all insignificant spaces instead of making XML human readable",
                legacy =
                  def
                  &= explicit &= name "legacy"
                  &= help "Legacy mode",
                quiet =
                  def
                  &= help "Be quiet. Do not print warnings",
                
                inFileNames =
                  def &= args &= typ "XMLFILE1 [XMLFILE2 ...]"
              }
        &= program programName
        &= summary ("XML Beautifier version " ++ programVersion)
        &= details ["Format xml file(s). If input data is/are common file(s) and no redirection of STDOUT is specified than input files will be changed inplace. If STDIN is not a terminal (e.g. because of redirection) then result will be printed to STDOUT.",
                    "usage: " ++ programName ++ " OPTIONS XMLFILE1 [XMLFILE2 ...]",
                    "       " ++ programName ++ " OPTIONS < somefile.xml > somefile.xml"]


checkOptions :: Flags -> IO ()
checkOptions opts = do
  hIsTerminalDevice stdin >>= \t ->
    when (not t && (not $ null $ inFileNames opts)) $
      error "As a data source, you must specify either STDIN or file(s) listed in the command line, but not both."
  
  hIsTerminalDevice stdout >>= \t ->
    when (backup opts && not t) $
      error "--backup option makes sence only when STDOUT is not redirected."
  
  when (backup opts && null (inFileNames opts)) $
    error "--backup option makes sence only when data source is a plain file(s) listed in the command line, not STDIN."
  when (strip opts && (isJust . spaces) opts) $
    error "--strip and --spaces options are mutually exclusive."
  when (legacy opts && (isJust . spaces) opts) $
    error "--legacy and --spaces options are mutually exclusive."
  when (legacy opts && strip opts) $
    error "--legacy and --strip options are mutually exclusive."
  when (legacy opts && (isJust . input_encoding) opts) $
    error "--legacy and --input-encoding options are mutually exclusive."

  

processOneSource :: Flags -> FilePath -> IO ()
processOneSource opts inFileName = do
  let inFileP = inFileName
  
  stdout_isatty <- hIsTerminalDevice stdout
  let inPlace = inFileP /= "-" && stdout_isatty
  
  (inFileH, inFileDecoratedName) <- if inFileP == "-"
                                    then return (stdin, "stdin")
                                    else do x <- openFile inFileP ReadMode
                                            return (x, inFileP)
  
  let parseDoc' outFileH = processFile inFileH inFileDecoratedName outFileH inputEncoding outputEncoding getMode
  
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

                 
    getMode =
      case () of
        _
         | legacy opts -> ModeLegacy
         | strip opts  -> ModeStrip
         | otherwise   -> ModeBeautify {indentString = indentString'}
      where
        indentString' =
          case spaces opts of
            Just i -> replicate i ' '
            _      -> "\t"


main :: IO ()
main = do
  -- stdin_isatty  <- hIsTerminalDevice stdin
  -- stdout_isatty <- hIsTerminalDevice stdout
  -- let inPlace = stdin_isatty && stdout_isatty
  
  opts'' <- cmdArgs =<< opts'
  checkOptions opts''
  let opts = if legacy opts''
             then opts'' { spaces = Nothing,
                           input_encoding = Nothing,
                           strip = False,
                           output_encoding = if (isNothing . output_encoding) opts''
                                             then Just "ISO-8859-1" -- в режиме совместимости
                                                                    -- кодировкой по умолчанию для
                                                                    -- выходного документа является
                                                                    -- iso-8859-1
                                             else output_encoding opts''
                         }
             else opts''
  
  
  globed_inFileNames <- liftM concat (mapM glob $ inFileNames opts)
  
  stdin_isatty <- hIsTerminalDevice stdin
  let inFileSources = case (stdin_isatty, inFileNames opts) of
        (True, []) -> error "No input data.\nUse '--help' command line flag to see the usage case."
        (_, [])    -> ["-"]
        _          -> globed_inFileNames

  case inFileSources of
    [] -> unless (quiet opts) $ hPutStrLn stderr "Specified file pattern does not match any file. No one file was processed."
    _  -> mapM_ (processOneSource opts) inFileSources
