{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Grower - grows the types to hold a syntax tree

module Pinchot.SyntaxTree where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Language.Haskell.TH as T
import Pinchot.Internal.TemplateHaskellShim (newtypeD, dataD)

import Pinchot.NonEmpty
import Pinchot.Rules
import Pinchot.Types

-- | Makes the top-level declarations for each given 'Rule' and for
-- all ancestors of the given 'Rule's.  Since ancestors are
-- included, you can get the entire tree of types that you need by
-- applying this function to a single start symbol.  Example:
-- "Pinchot.Examples.SyntaxTrees".
syntaxTrees
  :: [T.Name]
  -- ^ What to derive, e.g. @[''Eq, ''Ord, ''Show]@
  -> Seq (Rule t)
  -> T.DecsQ
syntaxTrees derives
  = traverse (ruleToType derives)
  . toList
  . families

branchConstructor :: Branch t -> T.ConQ
branchConstructor (Branch nm rules) = T.normalC name fields
  where
    name = T.mkName nm
    mkField (Rule n _ _) = T.strictType T.notStrict
      [t| $(T.conT (T.mkName n)) $(charTypeVar) $(anyTypeVar) |]
    fields = toList . fmap mkField $ rules
    anyTypeVar = T.varT (T.mkName "a")
    charTypeVar = T.varT (T.mkName "t")

-- | Makes the top-level declaration for a given rule.
ruleToType
  :: [T.Name]
  -- ^ What to derive
  -> Rule t
  -> T.Q T.Dec
ruleToType derives (Rule nm _ ruleType) = case ruleType of
  Terminal _ ->
    newtypeD (T.cxt []) name [charType, anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict
          [t| ( $(charTypeVar), $(anyTypeVar) ) |] ]

  NonTerminal b1 bs -> dataD (T.cxt []) name [charType, anyType] cons derives
    where
      cons = branchConstructor b1 : toList (fmap branchConstructor bs)

  Wrap (Rule inner _ _) ->
    newtypeD (T.cxt []) name [charType, anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [ T.strictType T.notStrict
            [t| $(T.conT (T.mkName inner)) $(charTypeVar) $(anyTypeVar) |] ]

  Record sq -> dataD (T.cxt []) name [charType, anyType] [ctor] derives
    where
      ctor = T.recC name . zipWith mkField [(0 :: Int) ..] . toList $ sq
      mkField num (Rule rn _ _) = T.varStrictType (T.mkName fldNm)
        (T.strictType T.notStrict
          [t| $(T.conT (T.mkName rn)) $(charTypeVar) $(anyTypeVar) |])
        where
          fldNm = '_' : recordFieldName num nm rn

  Opt (Rule inner _ _) ->
    newtypeD (T.cxt []) name [charType, anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict
          [t| Maybe ( $(T.conT (T.mkName inner)) $(charTypeVar)
                                                 $(anyTypeVar)) |]]

  Star (Rule inner _ _) ->
    newtypeD (T.cxt []) name [charType, anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name [sq]
        where
          sq = T.strictType T.notStrict
            [t| Seq ( $(T.conT (T.mkName inner)) $(charTypeVar)
                                                 $(anyTypeVar) ) |]

  Plus (Rule inner _ _) ->
    newtypeD (T.cxt []) name [charType, anyType] cons derives
    where
      cons = T.normalC name [ne]
        where
          ne = T.strictType T.notStrict [t| NonEmpty $(ins) |]
            where
              ins = [t| $(T.conT (T.mkName inner))
                $(charTypeVar) $(anyTypeVar) |]

  where
    name = T.mkName nm
    anyType = T.PlainTV (T.mkName "a")
    anyTypeVar = T.varT (T.mkName "a")
    charType = T.PlainTV (T.mkName "t")
    charTypeVar = T.varT (T.mkName "t")


