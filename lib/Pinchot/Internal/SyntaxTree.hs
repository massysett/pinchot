{-# LANGUAGE TemplateHaskell #-}
-- | Grower - grows the types to hold a syntax tree

module Pinchot.Internal.SyntaxTree where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Language.Haskell.TH as T

import Pinchot.Internal.Rules
import Pinchot.Internal.Types

-- | Makes the top-level declarations for each given 'Rule' and for
-- all ancestors of the given 'Rule's.  Since ancestors are
-- included, you can get the entire tree of types that you need by
-- applying this function to a single start symbol.
syntaxTrees
  :: T.Name
  -- ^ Name of terminal type
  -> [T.Name]
  -- ^ What to derive
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
    mkField (Rule n _ _) = T.strictType T.notStrict (T.conT (T.mkName n))
    fields = toList . fmap mkField $ rules

-- | Makes the top-level declaration for a given rule.
ruleToType
  :: T.Name
  -- ^ Name of terminal type
  -> [T.Name]
  -- ^ What to derive
  -> Rule t
  -> T.Q T.Dec
ruleToType typeName derives (Rule nm _ ruleType) = case ruleType of
  Terminal _ -> T.newtypeD (T.cxt []) name [] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict (T.conT typeName)]

  NonTerminal b1 bs -> T.dataD (T.cxt []) name [] cons derives
    where
      cons = branchConstructor b1 : toList (fmap branchConstructor bs)

  Terminals _ -> T.newtypeD (T.cxt []) name [] cons derives
    where
      cons = T.normalC name
        [T.strictType T.notStrict (T.appT [t| Seq |]
                                        (T.conT typeName))]

  Wrap (Rule inner _ _) -> T.newtypeD (T.cxt []) name [] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [ T.strictType T.notStrict (T.conT (T.mkName inner)) ]

  Record sq -> T.dataD (T.cxt []) name [] [ctor] derives
    where
      ctor = T.recC name . zipWith mkField [(0 :: Int) ..] . toList $ sq
      mkField num (Rule rn _ _) = T.varStrictType (T.mkName fldNm)
        (T.strictType T.notStrict (T.conT (T.mkName rn)))
        where
          fldNm = '_' : recordFieldName num nm rn

  Opt (Rule inner _ _) -> T.newtypeD (T.cxt []) name [] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict (T.appT [t| Maybe |]
                                    (T.conT (T.mkName inner)))]

  Star (Rule inner _ _) -> T.newtypeD (T.cxt []) name [] newtypeCon derives
    where
      newtypeCon = T.normalC name
        [T.strictType T.notStrict (T.appT [t| Seq |]
                                    (T.conT (T.mkName inner)))]

  Plus (Rule inner _ _) -> T.newtypeD (T.cxt []) name [] cons derives
    where
      cons = T.normalC name [tup]
        where
          tup = T.strictType T.notStrict [t| ( $(ins), Seq $(ins)) |]
            where
              ins = T.varT (T.mkName inner)

  where
    name = T.mkName nm

