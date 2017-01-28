{- |

Pinchot provides a simple language that you use to write a Haskell
values that describes a context-free grammar.  You can then
use Template Haskell to take this value and generate a series of data
types that correspond to your context-free grammar.  You can also use
Template Haskell to create an Earley parser that will parse all
strings in the context-free language.  Other handy utilities generate
functions that will return all the terminal characters from a parsed
production rule.  It is also possible to easily determine the location
(line, column, and position) of any parsed production or character.

Everything you typically need should be in this module.

For examples, please consult "Pinchot.Examples".

You should also look at the BNF Converter.

<http://bnfc.digitalgrammars.com>

Primary differences between BNFC and this library:

* the BNF Converter works as a standalone binary that parses
text BNF files.  With Pinchot you specify your grammar in Haskell.

* the BNF Converter currently generates many more outputs, such
as LaTeX.  It also generates code for many languages.  Pinchot
only works in Haskell.

* the BNF Converter generates input for parser generators like
Happy and Bison.  Pinchot currently only generates input
for the Haskell Earley library.

* Pinchot integrates seamlessly into Haskell using Template Haskell.

* the BNF Converter is GPL.  Pinchot is BSD3.

Pinchot grows and harvests syntax trees, so it is named after
Gifford Pinchot, first chief of the United States Forest Service.

-}
module Pinchot
  (
  -- * Production rules
    RuleName
  , Rule
  , BranchName
  , terminal
  , nonTerminal
  , union
  , series
  , wrap
  , record
  , opt
  , star
  , plus

  -- ** Errors
  , label
  , (<?>)

  -- * Qualifiers
  , Qualifier

  -- * Creating data types corresponding to grammars
  , syntaxTrees
  , allRulesRecord

  -- ** Typeclass instances and optics
  , wrappedInstances
  , bifunctorInstances
  , semigroupInstances
  , monoidInstances
  , prettyInstances
  , rulesToOptics

  -- * Creating Earley grammars
  , earleyGrammarFromRule
  , earleyProduct

  -- * Terminalizers
  , terminalizeRuleExp
  , terminalizers

  -- * Locations
  , Loc(..)
  , line
  , col
  , pos
  , locations
  , noLocations

  -- * Running parsers with locations
  , locatedFullParses
  ) where

import Pinchot.Earley
import Pinchot.Locator
import Pinchot.Names
import Pinchot.Rules
import Pinchot.SyntaxTree
import Pinchot.SyntaxTree.Instancer
import Pinchot.SyntaxTree.Optics
import Pinchot.SyntaxTree.Wrappers
import Pinchot.Terminalize
import Pinchot.Types
