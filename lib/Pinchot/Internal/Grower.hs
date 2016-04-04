-- | Grows syntax trees.

module Pinchot.Internal.Grower where


makeType
  :: Name
  -- ^ Name of terminal type
  -> Seq Name
  -- ^ What to derive
  -> String
  -- ^ Name of rule
  -> RuleType t
  -> TH.Q TH.Dec
makeType typeName derivesSeq nm ruleType = case ruleType of
  RTerminal _ -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (conT typeName)]

  RBranch (b1, bs) -> dataD (cxt []) name [] cons derives
    where
      cons = thBranch b1 : toList (fmap thBranch bs)

  RUnion (b1, bs) -> dataD (cxt []) name [] cons derives
    where
      cons = thUnionBranch nm b1 : toList (fmap (thUnionBranch nm) bs)

  RSeqTerm _ -> newtypeD (cxt []) name [] cons derives
    where
      cons = normalC name
        [strictType notStrict (appT [t| Seq |]
                                    (conT typeName))]

  ROptional (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Maybe |]
                                    (conT (mkName inner)))]

  RList (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Seq |]
                                    (conT (mkName inner)))]

  RList1 (Rule inner _ _) -> newtypeD (cxt []) name [] cons derives
    where
      cons = normalC name
        [ strictType notStrict (TH.tupleT 2 `appT` (conT (mkName inner))
            `appT` ([t| Seq |] `appT` (conT (mkName inner)))) ]

  RWrap (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [ strictType notStrict (conT (mkName inner)) ]

  RRecord sq -> dataD (cxt []) name [] [ctor] derives
    where
      ctor = recC name . zipWith mkField [(0 :: Int) ..] . toList $ sq
      mkField num (Rule rn _ _) = varStrictType (mkName fldNm)
        (strictType notStrict (conT (mkName rn)))
        where
          fldNm = '_' : fieldName num nm rn

  where
    name = mkName nm
    derives = toList derivesSeq

-- | Field name - without a leading underscore
fieldName
  :: Int
  -- ^ Index
  -> String
  -- ^ Parent type name
  -> String
  -- ^ Inner type name
  -> String
fieldName idx par inn = "r'" ++ par ++ "'" ++ show idx ++ "'" ++ inn

