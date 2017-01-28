{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module shows how to use Template Haskell to generate
-- functions that will reduce any production to the terminal tokens
-- that were used to create it.
module Pinchot.Examples.Terminalize where

import Data.List.NonEmpty (NonEmpty)

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees

terminalizeAddress :: SyntaxTrees.Address t a -> NonEmpty (t, a)
terminalizeAddress = $(terminalizeRuleExp "SyntaxTrees" rAddress)

$(terminalizers "SyntaxTrees" [rAddress])
