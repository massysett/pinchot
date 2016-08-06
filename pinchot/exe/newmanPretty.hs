module Main where

import Pinchot.Examples.Newman
import System.Environment (getArgs)

main :: IO ()
main = do
  a1:[] <- getArgs
  addressPretty a1
