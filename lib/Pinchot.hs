{- |

Pinchot provides a simple language that you use to write a Haskell
program that describes a context-free grammar.  When run, this program
creates a value that stores your context-free grammar.  You can then
use Template Haskell to take this value and generate a series of data
types that correspond to your context-free grammar.  You can also use
Template Haskell to create an Earley parser that will parse all
strings in the context-free language.

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
  ( -- * Intervals
    Intervals
  , include
  , exclude
  , solo
  , pariah

  -- * Simple production rules
  , Pinchot
  , RuleName
  , AlternativeName
  , Rule
  , terminal
  , terminalSeq
  , nonTerminal
  , union
  , record

  -- * Rules that modify other rules
  , list
  , list1
  , option
  , wrap
  , label
  , (<?>)

  -- * Transforming a Pinchot value to code
  , MakeOptics
  , makeOptics
  , noOptics
  , allRulesToTypes
  , ruleTreeToTypes
  , earleyGrammar
  , allEarleyGrammars
  , allRulesRecord
  , earleyProduct
  ) where

import Pinchot.Internal
import Pinchot.Intervals
