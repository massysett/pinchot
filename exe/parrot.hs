{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- Reads the string given as the first argument.  Parses it and
-- then, for each parse result, uses 't'Address' to print the
-- terminals.  Each result output should be the same as the input.
module Main where

import Pinchot
import Data.Foldable (toList)
import Pinchot.Examples.Postal
import Pinchot.Examples.EarleyProduct
import System.Environment (getArgs)

import Text.Earley (parser, fullParses)

ruleTreeToTypes noOptics ''Char [] postal

main :: IO ()
main = do
  a1:[] <- getArgs
  let (ls, _) = fullParses (parser $(earleyGrammar "" postal)) a1 
      printSeq = putStrLn . toList . t'Address
  mapM_ printSeq ls
