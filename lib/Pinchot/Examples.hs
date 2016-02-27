{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TypeFamilies #-}

-- | Examples for the use of Pinchot.
-- You wil want to look at the source code for the modules; examining
-- just the Haddocks shows you the code that the Template Haskell
-- ultimately generates.
--
-- In "Pinchot.Examples.Postal" is an example grammar for US postal
-- addresses.
--
-- "Pinchot.Examples.PostalAstAllRules" shows you how to use
-- 'allRulesToTypes' and 'earleyGrammar', while
-- "Pinchot.Examples.PostalAstRuleTree" shows you how to use
-- 'ruleTreeToTypes' and 'earleyGrammar'.
--
-- "Pinchot.Examples.AllEarleyGrammars" shows you how to use
-- 'allEarleyGrammars'.
--
-- Three executables are included in the @pinchot@ package.  To get
-- them, compile @pinchot@ with the @executables@ Cabal flag.
--
-- The @print-postal-grammar@ executable will pretty print the Haskell
-- source that results from applying 'earleyGrammar' to the 'postal'
-- grammar.
--
-- The @postal-parser@ executable takes as its first and sole argument
-- a string.  It parses the string using the 'postal' grammar and
-- pretty prints the resultin parses to standard output.
--
-- The @parrot@ executable takes as its first and sole argument a
-- string.  It parses the string using the 'postal' grammar.  For
-- every result that is returned (there might be zero, or one, or more
-- than one) the result is reduced to terminal symbols using the
-- 'terminals' function and the result is printed to standard output.

module Pinchot.Examples where

import Pinchot
import Pinchot.Examples.Postal
