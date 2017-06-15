module Main where

import XmlbTest.XmlbSpec (specOnlyOnTerminal)
import Test.Hspec (hspec)

main :: IO ()
main = hspec specOnlyOnTerminal
       
