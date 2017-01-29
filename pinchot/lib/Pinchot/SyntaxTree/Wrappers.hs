{-# LANGUAGE TemplateHaskell #-}
module Pinchot.SyntaxTree.Wrappers where

import qualified Control.Lens as Lens
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import qualified Language.Haskell.TH as T

import Pinchot.Names
import Pinchot.Rules
import Pinchot.Types

-- # Wrapped

-- | Creates a 'Lens.Wrapped' instance for each 'Rule' and its
-- ancestors, if there is an instance.
-- Only 'Pinchot.terminal', 'Pinchot.wrap',
-- 'Pinchot.opt', 'Pinchot.star', and 'Pinchot.plus'
-- get instances of 'Lens.Wrapped'.
--
-- This must be
-- spliced in the same module in which the syntax tree types are
-- created; this way, no orphans are created.  Since ancestors are
-- included, you can get the entire tree of types that you need by
-- applying this function to a single start symbol.
--
-- Example: "Pinchot.Examples.SyntaxTrees".

wrappedInstances
  :: [Rule t]
  -> T.DecsQ
wrappedInstances
  = sequence
  . catMaybes
  . fmap singleWrappedInstance
  . families

-- | Creates a 'Lens.Wrapped' instance for the 'Rule', if there is
-- one.  Only 'Pinchot.terminal', 'Pinchot.wrap',
-- 'Pinchot.opt', 'Pinchot.star', and 'Pinchot.plus'
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
    typ = (T.conT ''Lens.Wrapped) `T.appT`
      ((T.conT name)
        `T.appT` (typeT)
        `T.appT` (typeA))
    decs = [assocType, wrapper]
      where
        assocType = T.tySynInstD ''Lens.Unwrapped
          (T.tySynEqn [T.conT name
            `T.appT` (typeT)
            `T.appT` (typeA)]
                      wrappedType)
        wrapper = T.funD 'Lens._Wrapped'
          [T.clause [] (T.normalB body) []]
          where
            body = (T.varE 'Lens.iso)
              `T.appE` unwrap
              `T.appE` doWrap
              where
                unwrap = do
                  local <- T.newName "_local"
                  let lambPat = T.conP name [T.varP local]
                  T.lamE [lambPat] (T.varE local)
                    
                doWrap = do
                  local <- T.newName "_local"
                  let expn = (T.conE name) `T.appE` (T.varE local)
                      lambPat = T.varP local
                  T.lamE [lambPat] expn


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
        `T.appT` (typeT)
        `T.appT` (typeA))

wrappedTerminal
  :: String
  -- ^ Wrapper Rule name
  -> T.Q T.Dec
wrappedTerminal = makeWrapped
  [t| ( $(typeT), $(typeA) ) |]

wrappedTerminals
  :: String
  -- ^ Wrapper Rule name
  -> T.Q T.Dec
wrappedTerminals = makeWrapped
  [t| [ ($(typeT), $(typeA)) ] |]

wrappedStar
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Q T.Dec
wrappedStar wrappedName = makeWrapped innerName
  where
    innerName =
      [t| [ $(T.conT (T.mkName wrappedName)) $(typeT)
                                             $(typeA) ] |]

wrappedPlus
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> T.Q T.Dec
wrappedPlus wrappedName = makeWrapped tupName
  where
    tupName = [t| NonEmpty ( $(T.conT (T.mkName wrappedName))
                             $(typeT)
                             $(typeA)) |]


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
        `T.appT` (typeT)
        `T.appT` (typeA))
