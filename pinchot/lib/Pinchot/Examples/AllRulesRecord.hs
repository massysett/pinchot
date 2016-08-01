{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module shows how to generate a product type holding all
-- the productions that result from a given 'Rule'.  You wil want to
-- look at the source code, as the Haddocks will show you only the
-- generated type, not the Template Haskell used to generate it.
module Pinchot.Examples.AllRulesRecord where

import Pinchot
import Pinchot.Examples.Postal
import qualified Pinchot.Examples.SyntaxTrees as SyntaxTrees

$(allRulesRecord "SyntaxTrees" [rAddress])
