{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Pinchot.Examples.Terminalize where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees

terminalizeAddress :: SyntaxTrees.Address t a -> NonEmpty (t, a)
terminalizeAddress = $(terminalizeRuleExp "SyntaxTrees" rAddress)

$(terminalizers "SyntaxTrees" ''Char [rAddress])
