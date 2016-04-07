{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Pinchot.Examples.Earley where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees
import qualified Pinchot.Examples.AllRulesRecord as AllRulesRecord
import Text.Earley

addressGrammar :: Grammar r (Prod r String Char (SyntaxTrees.Address ()))
addressGrammar = $(earleyGrammarFromRule "SyntaxTrees" rAddress)

addressAllProductions :: Grammar r (AllRulesRecord.Productions r ())
addressAllProductions = $(earleyProduct "SyntaxTrees" "AllRulesRecord"
  [rAddress])
