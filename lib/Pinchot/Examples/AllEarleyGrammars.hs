{-# LANGUAGE TemplateHaskell #-}
-- | Provides an example of the use of 'Pinchot.AllEarleyGrammars'.
-- You will want to look at the source code, as it shows what sort
-- of Template Haskell splice to use.

module Pinchot.Examples.AllEarleyGrammars where

import Pinchot
import Pinchot.Examples.Postal
import Pinchot.Examples.PostalAstAllRules

allEarleyGrammars "" postal
