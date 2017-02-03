{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Grows the types to hold a syntax tree.

module Pinchot.SyntaxTree where

import Data.List.NonEmpty (NonEmpty, toList)
import qualified Language.Haskell.TH as T

import Pinchot.Names
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
  -> [Rule t]
  -> T.DecsQ
syntaxTrees derives
  = traverse (ruleToType derives)
  . families

branchConstructor :: Branch t -> T.ConQ
branchConstructor (Branch nm rules) = T.normalC name fields
  where
    name = T.mkName nm
    mkField (Rule n _ _) = notStrict
      [t| $(T.conT (T.mkName n)) $(typeT) $(typeA) |]
      where
        notStrict = T.bangType
          (T.bang T.noSourceUnpackedness T.noSourceStrictness)
    fields = fmap mkField $ rules

-- | Makes the top-level declaration for a given rule.
ruleToType
  :: [T.Name]
  -- ^ What to derive
  -> Rule t
  -> T.Q T.Dec
ruleToType deriveNames (Rule nm _ ruleType) = case ruleType of
  Terminal _ ->
    T.newtypeD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing newtypeCon derives
    where
      newtypeCon = T.normalC name
        [notStrict
          [t| ( $(typeT), $(typeA) ) |] ]

  NonTerminal bs -> T.dataD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing cons derives
    where
      cons = toList (fmap branchConstructor bs)

  Wrap (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing newtypeCon derives
    where
      newtypeCon = T.normalC name
        [ notStrict
            [t| $(T.conT (T.mkName inner)) $(typeT) $(typeA) |] ]

  Record sq -> T.dataD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing [ctor] derives
    where
      ctor = T.recC name . zipWith mkField [(0 :: Int) ..] $ sq
      mkField num (Rule rn _ _) = T.varBangType (T.mkName fldNm)
        (notStrict
          [t| $(T.conT (T.mkName rn)) $(typeT) $(typeA) |])
        where
          fldNm = '_' : recordFieldName num nm rn

  Opt (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing newtypeCon derives
    where
      newtypeCon = T.normalC name
        [notStrict
          [t| Maybe ( $(T.conT (T.mkName inner)) $(typeT)
                                                 $(typeA)) |]]

  Star (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing newtypeCon derives
    where
      newtypeCon = T.normalC name [sq]
        where
          sq = notStrict
            [t| [$(T.conT (T.mkName inner)) $(typeT) $(typeA)]  |]

  Plus (Rule inner _ _) ->
    T.newtypeD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing cons derives
    where
      cons = T.normalC name [ne]
        where
          ne = notStrict [t| NonEmpty $(ins) |]
            where
              ins = [t| $(T.conT (T.mkName inner))
                $(typeT) $(typeA) |]

  Series _ ->
    T.newtypeD (T.cxt []) name [tyVarBndrT, tyVarBndrA] Nothing cons derives
    where
      cons = T.normalC name [sq]
      sq = notStrict [t| NonEmpty ( $(typeT), $(typeA) ) |]

  where
    name = T.mkName nm
    derives = mapM T.conT deriveNames
    notStrict = T.bangType
      (T.bang T.noSourceUnpackedness T.noSourceStrictness)


