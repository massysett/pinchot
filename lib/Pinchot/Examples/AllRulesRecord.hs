-- | Provides an example of the use of 'Pinchot.allRulesRecord'.
-- You will want to look at the source code to see how to write the
-- Template Haskell splice.

{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.AllRulesRecord where

import qualified Pinchot
import qualified Pinchot.Examples.Postal as Postal
import qualified Pinchot.Examples.PostalAstAllRules as AllRules

Pinchot.allRulesRecord "AllRules" ''Char Postal.postal
