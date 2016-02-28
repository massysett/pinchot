{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.AllRulesRecord where

import qualified Pinchot
import qualified Pinchot.Examples.Postal as Postal
import qualified Pinchot.Examples.PostalAstAllRules as AllRules

Pinchot.allRulesRecord "AllRules" "Rules" ''Char Postal.postal
