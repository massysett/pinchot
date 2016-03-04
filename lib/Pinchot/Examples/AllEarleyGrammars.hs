{-# LANGUAGE TemplateHaskell #-}
-- | Provides an example of the use of 'Pinchot.AllEarleyGrammars'.
-- You will want to look at the source code, as it shows what sort
-- of Template Haskell splice to use.

module Pinchot.Examples.AllEarleyGrammars where

import qualified Pinchot
import qualified Pinchot.Examples.Postal as Postal
import qualified Pinchot.Examples.PostalAstAllRules as PostalAstAllRules

Pinchot.allEarleyGrammars "PostalAstAllRules" ''Char Postal.postal
