{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | Like @parrot@ but uses
-- 'Pinchot.Examples.EarleyProduct.addressParser'.
-- 
-- Reads the string given as the first argument.  Parses it and
-- then, for each parse result, uses 'terminals' to print the
-- terminals.  Each result output should be the same as the input.
module Main where

import Data.Foldable (toList)
import Pinchot
import Pinchot.Examples.Postal
import Pinchot.Examples.EarleyProduct
import Pinchot.Examples.PostalAstAllRules
import System.Environment (getArgs)

import Text.Earley (parser, fullParses)

main :: IO ()
main = do
  a1:[] <- getArgs
  let (ls, _) = fullParses (parser addressParser) a1
      printSeq = putStrLn . toList . t'Address
  mapM_ printSeq ls
