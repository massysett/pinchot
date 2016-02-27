{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

-- | Provides an example of the use of 'ruleTreeToTypes'.  You will
-- want to look at the source code, as it has a Template Haskell
-- splice that produces all of the data types that you see in the
-- Haddocks.
module Pinchot.Examples.PostalAstAllRules where

import Pinchot
import Pinchot.Examples.Postal

-- Earley is imported only for the type signature for 'postalGrammar'.  The
-- Template Haskell does not need the import.
import Text.Earley (Grammar, Prod)

-- This Template Haskell splice will produce a list of declarations,
-- with one declaration for each production rule in the grammar.
-- Unlike 'ruleTreeToTypes', this splice will contain every rule that
-- was defined in the 'Pinchot'.
allRulesToTypes makeOptics ''Char [''Eq, ''Ord, ''Show] postal

-- | Earley grammar created using Template Haskell.

postalGrammar :: Grammar r (Prod r String Char Address)
postalGrammar = $(earleyGrammar "" postal)
