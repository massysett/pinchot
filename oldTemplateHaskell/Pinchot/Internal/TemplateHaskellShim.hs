-- | A compatibility shim so Pinchot can work with versions of the
-- template-haskell library both before and after version 2.11.
-- The .cabal file is set up so this version of this module is used
-- for 2.10 versions of template-haskell.
module Pinchot.Internal.TemplateHaskellShim where

import qualified Language.Haskell.TH as T

newtypeD
  :: T.CxtQ -> T.Name -> [T.TyVarBndr] -> T.ConQ -> [T.Name] -> T.DecQ
newtypeD = T.newtypeD

dataD
  :: T.CxtQ -> T.Name -> [T.TyVarBndr] -> [T.ConQ] -> [T.Name] -> T.DecQ
dataD = T.dataD
