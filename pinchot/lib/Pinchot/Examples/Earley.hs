{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module shows how to generate Earley grammars for your
-- context-free grammar.  You will want to look at the source code.
module Pinchot.Examples.Earley where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees
import qualified Pinchot.Examples.AllRulesRecord as AllRulesRecord
import Text.Earley

addressGrammar
  :: Grammar r (Prod r String (Char, a) (SyntaxTrees.Address Char a))
addressGrammar = $(earleyGrammarFromRule "SyntaxTrees" rAddress)

addressAllProductions
  :: Grammar r (AllRulesRecord.Productions r Char a)
addressAllProductions = $(earleyProduct "SyntaxTrees" "AllRulesRecord"
  [rAddress])
