{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Pinchot.Examples.AllRulesRecord where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees

$(allRulesRecord "SyntaxTrees" ''Char [rAddress])
