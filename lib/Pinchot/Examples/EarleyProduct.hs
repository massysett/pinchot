{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Examples.EarleyProduct where

import qualified Pinchot
import qualified Pinchot.Examples.Postal as Postal
import qualified Pinchot.Examples.PostalAstAllRules as AllRules
import qualified Pinchot.Examples.AllRulesRecord as Record
import qualified Text.Earley as Earley

allProductions :: Earley.Grammar r (Record.Rules r)
allProductions = $(Pinchot.earleyProduct "AllRules" "Record" Postal.postal)
