{-# LANGUAGE TemplateHaskell #-}
module Pinchot.SyntaxTree.Wrappers where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Control.Lens as Lens
import qualified Language.Haskell.TH as T

import Pinchot.NonEmpty
import Pinchot.Rules
import Pinchot.Types

-- # Wrapped

-- | Creates a 'Lens.Wrapped' instance for each 'Rule' and its
-- ancestors, if there is an instance.  Only 'Wrap', 'Opt', 'Plus'
-- 'Star' get instances of 'Wrapped'.  Even though 'Terminal' and
-- 'Terminals' are both @newtype@s, they don't get instances of
-- 'Lens.Wrapped'.  You may choose to make a 'Lens.Prism' for those
-- using 'Pinchot.SyntaxTree.Optics.ruleToOptics'.
--
-- This must be
-- spliced in the same module in which the syntax tree types are
-- created; this way, no orphans are created.  Since ancestors are
-- included, you can get the entire tree of types that you need by
-- applying this function to a single start symbol.

wrappedInstances
  :: Seq (Rule t)
  -> T.DecsQ
wrappedInstances
  = return
  . catMaybes
  . toList
  . fmap singleWrappedInstance
  . families

-- | Creates a 'Lens.Wrapped' instance for the 'Rule', if there is
-- one.  Only 'Wrap', 'Opt', and 'Star' get instances of 'Wrapped'.
-- Even though 'Terminal' and 'Terminals' are both @newtype@s, they
-- don't get instances of 'Lens.Wrapped'.
-- 'This must be spliced in the same module in which the
-- syntax tree types are created.

singleWrappedInstance
  :: Rule t
  -> Maybe (T.Dec)
singleWrappedInstance (Rule nm _ ty) = case ty of
  Wrap (Rule inner _ _) -> Just $ wrappedWrap inner nm
  Opt (Rule inner _ _) -> Just $ wrappedOpt inner nm
  Star (Rule inner _ _) -> Just $ wrappedStar inner nm
  Plus (Rule inner _ _) -> Just $ wrappedPlus inner nm
  _ -> Nothing


makeWrapped
  :: T.Type
  -- ^ Name of wrapped type
  -> String
  -- ^ Name of wrapper type
  -> T.Dec
makeWrapped wrappedType nm = T.InstanceD [] typ decs
  where
    name = T.mkName nm
    local = T.mkName "_x"
    typ = (T.ConT ''Lens.Wrapped) `T.AppT`
      ((T.ConT name) `T.AppT` (T.VarT (T.mkName "a")))
    decs = [assocType, wrapper]
      where
        assocType = T.TySynInstD ''Lens.Unwrapped
          (T.TySynEqn [T.ConT name `T.AppT` (T.VarT (T.mkName "a"))]
                      wrappedType)
        wrapper = T.FunD 'Lens._Wrapped
          [T.Clause [] (T.NormalB body) []]
          where
            body = (T.VarE 'Lens.iso)
              `T.AppE` unwrap
              `T.AppE` doWrap
              where
                unwrap = T.LamE [lambPat] (T.VarE local)
                  where
                    lambPat = T.ConP name [T.VarP local]
                doWrap = T.LamE [lambPat] expn
                  where
                    expn = (T.ConE name)
                      `T.AppE` (T.VarE local)
                    lambPat = T.VarP local

wrappedOpt
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Dec
wrappedOpt wrappedName = makeWrapped maybeName
  where
    maybeName = (T.ConT ''Maybe) `T.AppT`
      ((T.ConT (T.mkName wrappedName)) `T.AppT` (T.VarT (T.mkName "a")))

wrappedStar
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Dec
wrappedStar wrappedName = makeWrapped innerName
  where
    innerName = (T.ConT ''Seq) `T.AppT`
      ((T.ConT (T.mkName wrappedName)) `T.AppT` (T.VarT (T.mkName "a")))

wrappedPlus
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Dec
wrappedPlus wrappedName = makeWrapped tupName
  where
    tupName = T.ConT ''NonEmpty
      `T.AppT` ((T.ConT (T.mkName wrappedName))
                  `T.AppT` (T.VarT (T.mkName "a")))

wrappedWrap
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Dec
wrappedWrap wrappedName = makeWrapped innerName
  where
    innerName =
      ((T.ConT (T.mkName wrappedName)) `T.AppT` (T.VarT (T.mkName "a")))
