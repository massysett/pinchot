{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module shows how to use Template Haskell to generate
-- optics (lenses, prisms, and isos) for the rules in your grammar.
-- You will want to look at the source code, as the Haddocks will
-- show the generated types but it will not show the Template
-- Haskell used to generate them.
module Pinchot.Examples.RulesToOptics where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees

$(rulesToOptics "SyntaxTrees" ''Char [rAddress])
