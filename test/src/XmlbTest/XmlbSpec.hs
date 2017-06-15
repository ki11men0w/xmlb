{-# LANGUAGE DeriveDataTypeable #-}
module XmlbTest.XmlbSpec where

import Test.Hspec
import TestUtils
import System.IO (hClose, hGetContents, withBinaryFile, IOMode(..), hIsTerminalDevice, stdin, stdout)
import System.FilePath ((</>), (<.>))
import System.Process (proc, shell, createProcess, waitForProcess, CreateProcess(..), StdStream(..), showCommandForUser)
import System.Exit (ExitCode(..))
import Control.Exception (Exception(), throw)
import Data.Typeable (Typeable())
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (copyFile)
import Data.Maybe (fromMaybe)
import Paths_xmlb (getBinDir)


data ExitFailureException = ExitFailureException {command:: String, exitCode :: Int, message :: String} deriving (Typeable)
instance Exception ExitFailureException
instance Show ExitFailureException where
  show (ExitFailureException command exitCode message) = "Command (" ++ command ++ ") terminated with exitCode=" ++show exitCode ++ ": " ++ trim message

getExeFileName :: IO String
getExeFileName = getBinDir >>= return . (</> "xmlb")

stderrFileName = "stderr.txt"


exitFailureException :: Selector ExitFailureException
exitFailureException = const True

exitFailureExceptionWith :: String -> Selector ExitFailureException
exitFailureExceptionWith s (ExitFailureException _ _ message) = s == trim message

exitFailureExceptionContaining :: String -> Selector ExitFailureException
exitFailureExceptionContaining s (ExitFailureException _ _ message) = s `isInfixOf` trim message

return' s = length s `seq` return s


type CommandArgs = [String]
action :: CommandArgs -> FilePath -> FilePath -> IO ()
action options inFileName outFileName = do

  withBinaryFile inFileName ReadMode $ \inH -> do
    withBinaryFile outFileName WriteMode $ \outH -> do
      action' options (UseHandle inH) (UseHandle outH)

action' :: CommandArgs -> StdStream -> StdStream -> IO ()
action' options inH outH = do
  tempFileWithErrorOutput <- getDirWithTempData >>= return . (</> stderrFileName)
  (command, res) <-
    withBinaryFile tempFileWithErrorOutput WriteMode $ \errH -> do
      exeFileName <- getExeFileName 
      let procParams = proc exeFileName options
      (_, _, _, ph) <- createProcess $ procParams {std_in=inH, std_out=outH, std_err=UseHandle errH}
      exitCode' <- waitForProcess ph
      return (showCommandForUser exeFileName options, exitCode')


  case res of
    ExitSuccess -> return ()
    ExitFailure i -> do
      errMsg <- readFile tempFileWithErrorOutput >>= return'
      throw $ ExitFailureException command i errMsg


action'' :: CommandArgs -> IO ()
action'' options = action' options Inherit Inherit


actionConvertInPlace :: CommandArgs -> FilePath -> FilePath -> IO ()
actionConvertInPlace options inFileName outFileName = do
  tempDir <- getDirWithTempData
  let
    stderrFileName' = tempDir </> stderrFileName

  (command, res) <-
    withBinaryFile stderrFileName' WriteMode $ \errH -> do
      exeFileName <- getExeFileName
      let options' = options ++ [inFileName]
      let procParams = proc exeFileName options'
      (_, _, _, ph) <- createProcess $ procParams {std_err=UseHandle errH}
      exitCode' <- waitForProcess ph
      return (showCommandForUser exeFileName options', exitCode')

  errMsg <- readFile stderrFileName' >>= return'
  case res of
    ExitSuccess -> do
      (return errMsg) `shouldReturn` []
      copyFile inFileName outFileName
    ExitFailure i -> do
      throw $ ExitFailureException command i errMsg
  
  

spec :: Spec
spec = do
  describe "Running xmlb executable" $ do

    context "Converting xml data" $ do

      context ("with file " ++ resourceFile_test) $ do

        describe "Test code corectness" $ do
          it ("with default result must differ from " ++ resourceFile_garbage) $ do
            assumeConversionIncorrect resourceFile_test resourceFile_garbage $
              action []
    
          it ("with default result must differ from " ++ resourceFile_empty) $ do
            assumeConversionIncorrect resourceFile_test resourceFile_empty $
              action []
          
          it ("with default parameters must differ from " ++ resourceFile_test_strip) $ do
            assumeConversionIncorrect resourceFile_test resourceFile_test_strip $
              action []
        
        it "with default parameters" $ do
          assumeConversionCorrect resourceFile_test resourceFile_test $
            action []
  
        it "with --spaces" $
          assumeConversionCorrect resourceFile_test resourceFile_test_spaces $
            action ["--spaces"]
  
        it "with --spaces=2" $
          assumeConversionCorrect resourceFile_test resourceFile_test_spaces2 $
            action ["--spaces=2"]
  
        it "with --strip" $
          assumeConversionCorrect resourceFile_test resourceFile_test_strip $
            action ["--strip"]
   

      context ("with file " ++ resourceFile_test_strip) $ do
        it "with default parameters" $
          assumeConversionCorrect resourceFile_test_strip resourceFile_test $
            action []

    context ("Checking xml header rendering") $ do
      context("with file " ++ resourceFile_header_no_attr) $ do
        it ("without strip") $
          assumeConversionCorrect resourceFile_header_no_attr resourceFile_header $
            action []
        it ("with strip") $
          assumeConversionCorrect resourceFile_header_no_attr resourceFile_header_strip $
            action ["--strip"]
      context("with file " ++ resourceFile_header_bencoding) $ do
        it ("without strip") $
          assumeConversionCorrect resourceFile_header_bencoding resourceFile_header_bencoding_beauty $
            action []
        it ("with strip") $
          assumeConversionCorrect resourceFile_header_bencoding resourceFile_header_bencoding_strip $
            action ["--strip"]


    context "Encoding conversions" $ do
      it "without specifying input or output encodings original encoding must be used and printed as is" $ 
        assumeConversionCorrect resourceFile_enc1 resourceFile_enc1 $
          action []

      it "realizing encoding of input document only by BOM" $ 
        assumeConversionCorrect resourceFile_enc_utf16_withBOM_woEnc resourceFile_enc $
          action ["--encoding=UTF-8"]

      it "realizing encoding of input document only by BOM and print output encoding literally as in --encoding parameter" $ 
        assumeConversionCorrect resourceFile_enc_utf16_withBOM_woEnc resourceFile_enc1 $
          action ["--encoding=Utf-8"]

      context "with file enc.xml" $ do

        it "with default" $ 
          assumeConversionCorrect resourceFile_enc resourceFile_enc $
            action []

        it "with --encoding=utf16le" $ 
          assumeConversionCorrect resourceFile_enc resourceFile_enc_utf16le $
            action ["--encoding=utf16le"]

        it "with --encoding=utf16be" $ 
          assumeConversionCorrect resourceFile_enc resourceFile_enc_utf16be $
            action ["--encoding=utf16be"]





specOnlyOnTerminal :: Spec
specOnlyOnTerminal = do
  stdin_isatty  <- runIO $ hIsTerminalDevice stdin
  stdout_isatty <- runIO $ hIsTerminalDevice stdout

  let
    with_stdin_as_terminal  = \action -> do
      if stdin_isatty
        then action
        else pendingWith "STDIN must be terminal device for running this test"
    with_stdout_as_terminal = \action -> do
      if stdout_isatty
        then action
        else pendingWith "STDOUT must be terminal device for running this test"

  describe "Running xmlb executable with STDIN/STDOUT as terminal device" $ do
    context "Exits with failure" $ do
      it "must not mix file(s) and stdin as data source" $
        with_stdout_as_terminal $
        assumeConversionIncorrect resourceFile_test resourceFile_garbage (action ["someFileName"]) 
          `shouldThrow` exitFailureExceptionContaining "As a data source, you must specify either STDIN or file(s) listed in the command line, but not both."

      it "with bad command line argument --qq" $
        with_stdout_as_terminal $
        action'' ["--qq"] `shouldThrow` exitFailureExceptionWith "Unknown flag: --qq"
     
      it "with several bad command line arguments must tell about first" $
        with_stdout_as_terminal $
        action'' ["--zz", "--qq", "--aa"] `shouldThrow` exitFailureExceptionWith "Unknown flag: --zz"

      it "if no source data specified then must tell about it" $
        with_stdout_as_terminal $
        with_stdin_as_terminal $
        action'' [] `shouldThrow` exitFailureExceptionContaining "No input data.\nUse '--help' command line flag to see the usage case."

    
    context "Converting xml data" $ do

      context "Converting inplace" $ do
        it "with --spaces" $
          (with_stdout_as_terminal . with_stdin_as_terminal) $
          assumeConversionCorrect resourceFile_test resourceFile_test_spaces $
            actionConvertInPlace ["--spaces"]

        it "if can not convert original file then must not touch it" $
          with_stdout_as_terminal $
          (assumeConversionCorrect resourceFile_invalid resourceFile_invalid $ actionConvertInPlace []) `shouldThrow` exitFailureException

        it "must not touch source file if unknown option specified" $
          with_stdout_as_terminal $
          (assumeConversionCorrect resourceFile_test resourceFile_test $ actionConvertInPlace ["--qq"]) `shouldThrow` exitFailureException
