{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Pinchot.Examples.LocatedTypes where

import Pinchot
import Pinchot.Examples.Postal

$(allRulesToLocatedTypes makeOptics ''Char [''Eq, ''Ord, ''Show]
  postal)
