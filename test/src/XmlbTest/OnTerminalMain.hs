module Main where

import XmlbSpec (specOnlyOnTerminal)
import Test.Hspec (hspec)
import System.IO (stdout, hIsTerminalDevice)

main :: IO ()
main = do
  stdout_isatty <- hIsTerminalDevice stdout
  if stdout_isatty
    then hspec specOnlyOnTerminal
    else error "STDOUT must be a terminal device for running this test suite"
       
