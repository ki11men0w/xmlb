module TestUtils where

import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), (<.>), takeFileName, takeDirectory, splitDirectories)
import System.IO (withBinaryFile, hGetContents, IOMode(..))
import Control.Monad (unless, when)
import Test.Hspec (expectationFailure)

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

getDirPrefix :: IO (Maybe String)
getDirPrefix = do
  curDir <- getCurrentDirectory

  let inBuildDir = (take 2 .drop 1 . reverse . splitDirectories) curDir == ["build", "dist"]
  --(takeFileName . takeDirectory) curDir == "build" && (takeDirectory . takeDirectory) curDir == "dist"

  if inBuildDir
    then return $ Just $ ".." </> ".." </> ".."
    else return Nothing

getDirWithResources :: IO FilePath
getDirWithResources = do
  let defaultResult = "test" </> "resources"
  prefix <- getDirPrefix
  return $ maybe defaultResult (</> defaultResult) prefix

getDirWithTempData :: IO FilePath
getDirWithTempData = do
  let defaultResult = "dist" </> "test" </> "tmp"

  prefix <- getDirPrefix
  let dir = maybe defaultResult (</> defaultResult) prefix

  createDirectoryIfMissing True dir
  return dir

findOriginalFile :: FilePath -> IO FilePath
findOriginalFile fileName = do
  exists <- doesFileExist fileName
  if exists
    then return fileName
    else if takeFileName fileName == fileName
           then getDirWithResources >>= return . (</> fileName)
           else return fileName

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
