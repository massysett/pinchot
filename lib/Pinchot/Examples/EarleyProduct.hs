{-# LANGUAGE TemplateHaskell #-}

-- | Provides an example of the use of 'Pinchot.earleyProduct'.  You
-- will want to look at the source code, as looking at only the
-- Haddocks will show you only the result of all the Template
-- Haskell splices.
module Pinchot.Examples.EarleyProduct where

import qualified Pinchot

-- You will need to import your Pinchot grammar
import qualified Pinchot.Examples.Postal as Postal

-- And import the types that result from applying
-- 'Pinchot.allRulesToTypes' to the Pinchot grammar
import qualified Pinchot.Examples.PostalAstAllRules as AllRules

-- And, import the product type that results from applying
-- 'Pinchot.allRulesRecord' to your grammar
import qualified Pinchot.Examples.AllRulesRecord as Record

import qualified Text.Earley as Earley

-- | 'Pinchot.earleyProduct' gives you a big product type that has
-- every rule that is in your Pinchot grammar.
allProductions :: Earley.Grammar r (Record.Productions r)
allProductions = $(Pinchot.earleyProduct "AllRules" "Record" Postal.postal)

-- | To get the exact production you're interested in, just use
-- 'fmap'.  Then you can use this value for 'Earley.parser'.
addressParser :: Earley.Grammar r (Earley.Prod r String Char AllRules.Address)
addressParser = fmap Record.a'Address allProductions
