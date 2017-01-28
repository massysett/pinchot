{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

-- The following extension is required only for the splice of Lens wrapped
-- instances in 'wrappedInstances'
{-# LANGUAGE TypeFamilies #-}

-- | This module shows how to use Template Haskell to generate the
-- data types corresponding to your context-free grammar.  You will
-- want to look at the source; the Haddocks will show only the
-- generated types and not the Template Haskell that was used to
-- generate them.
module Pinchot.Examples.SyntaxTrees where

import GHC.Generics (Generic)

import Pinchot
import Pinchot.Examples.Postal

-- This generates the data types corresponding to the 'rAddress'
-- 'Rule', as well as all the ancestors of that 'Rule'.
$(syntaxTrees
  [''Eq, ''Ord, ''Show, ''Foldable, ''Traversable, ''Functor, ''Generic]
  [rAddress])

-- This generates intances of the Lens Wrapped typeclass.
$(wrappedInstances [rAddress])

-- This generates instances of the Bifunctor typeclass.
$(bifunctorInstances [rAddress])

-- This generates instances of the Semigroup typeclass.
$(semigroupInstances [rAddress])

-- This generates instances of the Monoid typeclass.
$(monoidInstances [rAddress])

-- This generates instances of the 'Pretty.PrettyVal' typeclass.
$(prettyInstances [rAddress])
