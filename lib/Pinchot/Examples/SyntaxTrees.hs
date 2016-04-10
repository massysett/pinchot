{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- The following extension is required only for the splice of Lens wrapped
-- instances in 'wrappedInstances'
{-# LANGUAGE TypeFamilies #-}

-- | This module shows how to use Template Haskell to generate the
-- data types corresponding to your context-free grammar.  You will
-- want to look at the source; the Haddocks will show only the
-- generated types and not the Template Haskell that was used to
-- generate them.
module Pinchot.Examples.SyntaxTrees where

import Pinchot
import Pinchot.SyntaxTree.Wrappers
import Pinchot.Examples.Postal

import qualified Control.Lens as Lens

-- This generates the data types corresponding to the 'rAddress'
-- 'Rule', as well as all the ancestors of that 'Rule'.
$(syntaxTrees ''Char
  [''Eq, ''Ord, ''Show, ''Foldable, ''Traversable, ''Functor]
  [rAddress])

-- This generates intances of the Lens Wrapped typeclass.
$(wrappedInstances [rAddress])
