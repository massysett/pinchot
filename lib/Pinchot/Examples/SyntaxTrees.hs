{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Pinchot.Examples.SyntaxTrees where

import Pinchot
import Pinchot.Examples.Postal

$(syntaxTrees ''Char
  [''Eq, ''Ord, ''Show, ''Foldable, ''Traversable, ''Functor]
  [rAddress])
