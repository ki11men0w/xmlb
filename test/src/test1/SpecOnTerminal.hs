module Main where

import XmlbSpec (specOnlyOnTerminal)
import Test.Hspec (hspec)

main :: IO ()
main = hspec specOnlyOnTerminal
       
