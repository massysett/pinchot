{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
-- | Provides an example of 'ruleTreeToTypes', but unlike
-- "Pinchot.Examples.PostalAstAllRules", does not make optics.

module Pinchot.Examples.PostalAstNoLenses where

import Pinchot
import Pinchot.Examples.Postal

-- Earley is imported only for the type signature for 'myParser'.  The
-- Template Haskell does not need the import.
import Text.Earley (Grammar, Prod)

-- This Template Haskell splice will produce a list of declarations,
-- with one declaration for each production rule in the grammar.
-- Unlike 'ruleTreeToTypes', this splice will contain every rule that
-- was defined in the 'Pinchot'.
allRulesToTypes noOptics ''Char [''Eq, ''Ord, ''Show] postal

-- | Earley grammar created using Template Haskell.

postalGrammar :: Grammar r (Prod r String Char Address)
postalGrammar = $(earleyGrammar "" postal)
