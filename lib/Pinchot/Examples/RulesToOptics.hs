{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Pinchot.Examples.RulesToOptics where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees

$(rulesToOptics "SyntaxTrees" ''Char [rAddress])
