{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.Operations where

import Pinchot.SyntaxTree.Instancer
import qualified Pinchot.Examples.SyntaxTrees as Trees
import qualified Pinchot.Examples.Postal as Postal

bimapper :: (a -> b) -> (c -> d) -> Trees.Letters a c -> Trees.Letters b d
bimapper = $(bimapExpression "Trees" Postal.rLetters)
