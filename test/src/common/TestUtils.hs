module TestUtils where

import System.Directory (createDirectoryIfMissing, createDirectory, copyFile)
import System.IO.Error (tryIOError)
import System.FilePath ((</>), (<.>), takeFileName)
import System.IO (withBinaryFile, hGetContents, IOMode(..))
import Control.Monad (unless, when)
import Test.Hspec (expectationFailure)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace)
import Paths_xmlb (getDataFileName)

resourceFile_invalid = "invalid.xml.bad"
resourceFile_empty = "empty.txt"
resourceFile_garbage = "garbage.bin"
resourceFile_test = "test.xml"
resourceFile_test_strip = "test.strip.xml"
resourceFile_test_spaces = "test.spaces.xml"
resourceFile_test_spaces2 = "test.spaces.2.xml"

resourceFile_enc = "enc.xml"
resourceFile_enc_utf16_withBOM_woEnc = "enc.utf16.withBOM.woEnc.xml"
resourceFile_enc_utf16be = "enc.utf16be.xml"
resourceFile_enc_utf16le = "enc.utf16le.xml"
resourceFile_enc1 = "enc1.xml"

resourceFile_header = "header.xml"
resourceFile_header_no_attr = "header.no.attr.xml"
resourceFile_header_strip = "header.strip.xml"
resourceFile_header_bencoding = "header.bencoding.xml"
resourceFile_header_bencoding_beauty = "header.bencoding.beauty.xml"
resourceFile_header_bencoding_strip = "header.bencoding.strip.xml"

resourceFile_test_significant_whitespaces = "test.significant-witespaces.xml"
resourceFile_test_significant_whitespaces_b = "test.significant-witespaces.beauty.xml"
resourceFile_test_significant_whitespaces_s = "test.significant-witespaces.strip.xml"

trim :: String -> String 
trim = dropWhile isSpace . dropWhileEnd isSpace
  where
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []


getDirWithTempData :: IO FilePath
getDirWithTempData = do
  let dir = ".tests-data.tmp"
  -- something wrong with createDirectoryIfMissing on Windows so use plain createDirectory
  --createDirectoryIfMissing True dir
  tryIOError $ createDirectory dir
  return dir

findOriginalFile :: FilePath -> IO FilePath
findOriginalFile fileName = do
  getDataFileName $ "test" </> "resources" </> fileName

type Action = FilePath -> FilePath -> IO ()

applyConversion :: FilePath -> Action -> IO FilePath
applyConversion inFileName action = do
  dirWithTempData <- getDirWithTempData
  originalFileName <- findOriginalFile inFileName

  let
    sourceFileName = dirWithTempData </> inFileName
    resultFileName = dirWithTempData </> inFileName <.> "new"
    
  copyFile originalFileName sourceFileName

  action sourceFileName resultFileName

  return resultFileName

compareFiles :: FilePath -> FilePath -> IO Bool  
compareFiles fileName1 fullFileName2 = do
  fullFileName1 <- findOriginalFile fileName1
  simpleCompareFiles fullFileName1 fullFileName2
    where
      simpleCompareFiles :: FilePath -> FilePath -> IO Bool
      simpleCompareFiles fileName1 fileName2 =
        withBinaryFile fileName1 ReadMode $ \fileHandle1 ->
          withBinaryFile fileName2 ReadMode $ \fileHandle2 -> do
            x1 <- hGetContents fileHandle1
            x2 <- hGetContents fileHandle2
            return $! x1 == x2


assumeFilesEqual :: FilePath -> FilePath -> IO ()
assumeFilesEqual resultFileName etalonFileName = do
  equal <- compareFiles etalonFileName resultFileName
  unless equal $
    expectationFailure $ "Result file differs from etalon result file \"" ++ takeFileName etalonFileName ++ "\""

assumeFilesDiffer :: FilePath -> FilePath -> IO ()
assumeFilesDiffer resultFileName etalonFileName = do
  equal <- compareFiles etalonFileName resultFileName
  when equal $
    expectationFailure $ "Result file equal to etalon result file \"" ++ takeFileName etalonFileName ++ "\" but must differ"
  

assumeConversionCorrect :: FilePath -> FilePath -> Action -> IO ()
assumeConversionCorrect origFileName etalonFileName action = do
  resultFileName <- applyConversion origFileName action
  --compareFiles etalonFileName resultFileName `shouldReturn` True
  assumeFilesEqual resultFileName etalonFileName

assumeConversionIncorrect :: FilePath -> FilePath -> Action -> IO ()
assumeConversionIncorrect origFileName etalonFileName  action = do
  resultFileName <- applyConversion origFileName action
  --compareFiles etalonFileName resultFileName `shouldReturn` True
  assumeFilesDiffer resultFileName etalonFileName
