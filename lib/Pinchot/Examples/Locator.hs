{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Pinchot.Examples.Locator where

import Pinchot
import qualified Pinchot.Examples.Postal as Postal
import qualified Pinchot.Examples.PostalAstAllRules as AllRules
import qualified Pinchot.Examples.LocatedTypes as LocatedTypes

locateAddress
  :: AllRules.Address
  -> Locator LocatedTypes.Address
locateAddress = $(locateRuleFunction "AllRules" "LocatedTypes" 'advanceChar
  Postal.postal)
