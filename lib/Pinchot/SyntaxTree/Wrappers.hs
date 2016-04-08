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
-- ancestors, if there is an instance.  Only 'Terminal, 'Terminals',
-- 'Wrap', 'Opt', 'Plus'
-- 'Star' get instances of 'Wrapped'.
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
  = sequence
  . catMaybes
  . toList
  . fmap singleWrappedInstance
  . families

-- | Creates a 'Lens.Wrapped' instance for the 'Rule', if there is
-- one.  Only 'Terminal', 'Terminals', 'Wrap', 'Opt', 'Star', and 'Plus'
-- get instances of 'Wrapped'.
-- 'This must be spliced in the same module in which the
-- syntax tree types are created.

singleWrappedInstance
  :: Rule t
  -> Maybe (T.Q T.Dec)
singleWrappedInstance (Rule nm _ ty) = case ty of
  Terminal _ -> Just $ wrappedTerminal nm
  Wrap (Rule inner _ _) -> Just $ wrappedWrap inner nm
  Opt (Rule inner _ _) -> Just $ wrappedOpt inner nm
  Star (Rule inner _ _) -> Just $ wrappedStar inner nm
  Plus (Rule inner _ _) -> Just $ wrappedPlus inner nm
  _ -> Nothing


makeWrapped
  :: T.TypeQ
  -- ^ Name of wrapped type
  -> String
  -- ^ Name of wrapper type
  -> T.Q T.Dec
makeWrapped wrappedType nm = T.instanceD (return []) typ decs
  where
    name = T.mkName nm
    local = T.mkName "_x"
    typ = (T.conT ''Lens.Wrapped) `T.appT`
      ((T.conT name)
        `T.appT` (T.varT (T.mkName "t"))
        `T.appT` (T.varT (T.mkName "a")))
    decs = [assocType, wrapper]
      where
        assocType = T.tySynInstD ''Lens.Unwrapped
          (T.tySynEqn [T.conT name
            `T.appT` (T.varT (T.mkName "t"))
            `T.appT` (T.varT (T.mkName "a"))]
                      wrappedType)
        wrapper = T.funD 'Lens._Wrapped
          [T.clause [] (T.normalB body) []]
          where
            body = (T.varE 'Lens.iso)
              `T.appE` unwrap
              `T.appE` doWrap
              where
                unwrap = T.lamE [lambPat] (T.varE local)
                  where
                    lambPat = T.conP name [T.varP local]
                doWrap = T.lamE [lambPat] expn
                  where
                    expn = (T.conE name)
                      `T.appE` (T.varE local)
                    lambPat = T.varP local

wrappedOpt
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Q T.Dec
wrappedOpt wrappedName = makeWrapped maybeName
  where
    maybeName = (T.conT ''Maybe)
      `T.appT`
      ((T.conT (T.mkName wrappedName))
        `T.appT` (T.varT (T.mkName "t"))
        `T.appT` (T.varT (T.mkName "a")))

wrappedTerminal
  :: String
  -- ^ Wrapper Rule name
  -> T.Q T.Dec
wrappedTerminal = makeWrapped
  [t| ( $(T.varT (T.mkName "t")), $(T.varT (T.mkName "a")) ) |]

wrappedTerminals
  :: String
  -- ^ Wrapper Rule name
  -> T.Q T.Dec
wrappedTerminals = makeWrapped
  [t| Seq ( $(T.varT (T.mkName "t")), $(T.varT (T.mkName "a")) ) |]

wrappedStar
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Q T.Dec
wrappedStar wrappedName = makeWrapped innerName
  where
    innerName = (T.conT ''Seq) `T.appT`
      ((T.conT (T.mkName wrappedName))
        `T.appT` (T.varT (T.mkName "t"))
        `T.appT` (T.varT (T.mkName "a")))

wrappedPlus
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Q T.Dec
wrappedPlus wrappedName = makeWrapped tupName
  where
    tupName = T.conT ''NonEmpty
      `T.appT` ((T.conT (T.mkName wrappedName))
                  `T.appT` (T.varT (T.mkName "t"))
                  `T.appT` (T.varT (T.mkName "a")))

wrappedWrap
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Q T.Dec
wrappedWrap wrappedName = makeWrapped innerName
  where
    innerName =
      ((T.conT (T.mkName wrappedName))
        `T.appT` (T.varT (T.mkName "t"))
        `T.appT` (T.varT (T.mkName "a")))
