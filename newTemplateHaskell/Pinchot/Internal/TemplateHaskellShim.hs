-- | A compatibility shim so Pinchot can work with versions of the
-- template-haskell library both before and after version 2.11.
-- The .cabal file is set up so this version of this module is used
-- for 2.11 and newer versions of template-haskell.

module Pinchot.Internal.TemplateHaskellShim where

import qualified Language.Haskell.TH as T

newtypeD
  :: T.CxtQ -> T.Name -> [T.TyVarBndr] -> T.ConQ -> [T.Name] -> T.DecQ
newtypeD cxt name tyVars ctor derives
  = T.newtypeD cxt name tyVars Nothing ctor (return $ fmap T.ConT derives)

dataD
  :: T.CxtQ -> T.Name -> [T.TyVarBndr] -> [T.ConQ] -> [T.Name] -> T.DecQ
dataD cxt name tyVars ctors derives
  = T.dataD cxt name tyVars Nothing ctors (return $ fmap T.ConT derives)
