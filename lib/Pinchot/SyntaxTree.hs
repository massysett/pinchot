{-# LANGUAGE TemplateHaskell #-}
-- | Grower - grows the types to hold a syntax tree

module Pinchot.SyntaxTree where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Language.Haskell.TH as T

import Pinchot.NonEmpty
import Pinchot.Rules
import Pinchot.Types

-- | Makes the top-level declarations for each given 'Rule' and for
-- all ancestors of the given 'Rule's.  Since ancestors are
-- included, you can get the entire tree of types that you need by
-- applying this function to a single start symbol.
syntaxTrees
  :: T.Name
  -- ^ Name of terminal type.  Typically you will get this from the
  -- Template Haskell quoting mechanism, e.g. @''Char@.
  -> [T.Name]
  -- ^ What to derive, e.g. @[''Eq, ''Ord, ''Show]@
  -> Seq (Rule t)
  -> T.DecsQ
syntaxTrees term derives
  = traverse (ruleToType term derives)
  . toList
  . families

branchConstructor :: Branch t -> T.ConQ
branchConstructor (Branch nm rules) = T.normalC name fields
  where
    name = T.mkName nm
    mkField (Rule n _ _) = T.strictType T.notStrict
      [t| $(T.conT (T.mkName n)) $(anyTypeVar) |]
    fields = toList . fmap mkField $ rules
    anyTypeVar = T.varT (T.mkName "a")

-- | Makes the top-level declaration for a given rule.
ruleToType
  :: T.Name
  -- ^ Name of terminal type
  -> [T.Name]
  -- ^ What to derive
  -> Rule t
  -> T.Q T.Dec
ruleToType typeName derives (Rule nm _ ruleType) = case ruleType of
  Terminal _ -> T.newtypeD (T.cxt []) name [anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict
          [t| ( $(T.conT typeName), $(T.varT (T.mkName "a")) ) |] ]

  NonTerminal b1 bs -> T.dataD (T.cxt []) name [anyType] cons derives
    where
      cons = branchConstructor b1 : toList (fmap branchConstructor bs)

  Terminals _ -> T.newtypeD (T.cxt []) name [anyType] cons derives
    where
      cons = T.normalC name
        [T.strictType T.notStrict
          [t| Seq ( ( $(T.conT typeName), $(T.varT (T.mkName "a")))) |] ]

  Wrap (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [ T.strictType T.notStrict
            [t| $(T.conT (T.mkName inner)) $(anyTypeVar) |] ]

  Record sq -> T.dataD (T.cxt []) name [anyType] [ctor] derives
    where
      ctor = T.recC name . zipWith mkField [(0 :: Int) ..] . toList $ sq
      mkField num (Rule rn _ _) = T.varStrictType (T.mkName fldNm)
        (T.strictType T.notStrict
          [t| $(T.conT (T.mkName rn)) $(anyTypeVar) |])
        where
          fldNm = '_' : recordFieldName num nm rn

  Opt (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict
          [t| Maybe ( $(T.conT (T.mkName inner)) $(anyTypeVar)) |]]

  Star (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [anyType] newtypeCon derives
    where
      newtypeCon = T.normalC name [sq]
        where
          sq = T.strictType T.notStrict
            [t| Seq ( $(T.conT (T.mkName inner)) $(anyTypeVar) ) |]

  Plus (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [anyType] cons derives
    where
      cons = T.normalC name [ne]
        where
          ne = T.strictType T.notStrict [t| NonEmpty $(ins) |]
            where
              ins = [t| $(T.conT (T.mkName inner)) $(anyTypeVar) |]

  where
    name = T.mkName nm
    anyType = T.PlainTV (T.mkName "a")
    anyTypeVar = T.varT (T.mkName "a")


