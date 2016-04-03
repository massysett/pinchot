{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Pinchot.Examples.Locator where

import Pinchot
import qualified Pinchot.Examples.PostalTest as PostalTest
import qualified Pinchot.Examples.PostalAstAllRules as AllRules
import qualified Pinchot.Examples.LocatedTypes as LocatedTypes

testLocate :: _
testLocate = $(locateRuleFunction "AllRules" "LocatedTypes" 'advanceChar
  PostalTest.postal)
