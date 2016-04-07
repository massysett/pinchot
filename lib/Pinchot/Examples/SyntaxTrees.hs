{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- The following extension is required only for the splice of Lens wrapped
-- instances in 'wrappedInstances'
{-# LANGUAGE TypeFamilies #-}

module Pinchot.Examples.SyntaxTrees where

import Pinchot
import Pinchot.Examples.Postal

$(syntaxTrees ''Char
  [''Eq, ''Ord, ''Show, ''Foldable, ''Traversable, ''Functor]
  [rAddress])

$(wrappedInstances [rAddress])
