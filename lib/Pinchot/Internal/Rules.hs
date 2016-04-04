module Pinchot.Internal.Rules where

import Control.Monad.Trans.Class (lift)

import Pinchot.Internal.State
import Pinchot.Internal.Types
import Pinchot.Intervals

-- | Name a 'Rule' for use in error messages.  If you do not name a
-- rule using this combinator, the rule's type name will be used in
-- error messages.
label :: String -> Rule t -> Rule t
label s (Rule n _ t) = Rule n (Just s) t

-- | Infix form of 'label' for use in a 'Pinchot'; handy for use in
-- @do@ or @mdo@ notation.
(<?>) :: Pinchot t (Rule t) -> String -> Pinchot t (Rule t)
p <?> s = fmap (label s) p
infixr 0 <?>

addRuleName
  :: RuleName
  -> Pinchot t ()
addRuleName name = Pinchot $ do
  old@(Names tyNames _ _ _) <- lift get
  case name of
    [] -> throw
    x:_ -> do
      when (not (isUpper x)) throw
      when (Set.member name tyNames) throw
      lift $ put (old { tyConNames = Set.insert name tyNames })
  where
    throw = throwE $ InvalidName name

addDataConName
  :: AlternativeName
  -> Pinchot t ()
addDataConName name = Pinchot $ do
  old@(Names _ dcNames _ _) <- lift get
  case name of
    [] -> throw
    x:_ -> do
      when (not (isUpper x)) throw
      when (Set.member name dcNames) throw
      lift $ put (old { dataConNames = Set.insert name dcNames })
  where
    throw = throwE $ InvalidName name

newRule
  :: RuleName
  -> RuleType t
  -> Pinchot t (Rule t)
newRule name ty = Pinchot $ do
  runPinchot (addRuleName name)
  st <- lift get
  let r = Rule name Nothing ty
      newSt = st { nextIndex = succ (nextIndex st)
                 , allRules = M.insert (nextIndex st) r
                            (allRules st)
                 }
  lift (put newSt)
  runPinchot $ addDataConNames r
  return r

-- | Creates a terminal production rule.
terminal

  :: RuleName

  -> Intervals t
  -- ^ Valid terminal symbols

  -> Pinchot t (Rule t)

terminal name ivls = newRule name (RTerminal ivls)

splitNonTerminal
  :: String
  -> Seq (String, Seq (Rule t))
  -> Pinchot t ((String, Seq (Rule t)), Seq (String, Seq (Rule t)))
splitNonTerminal n sq = Pinchot $ case viewl sq of
  EmptyL -> throwE $ EmptyNonTerminal n
  x :< xs -> return (x, xs)

-- | Creates a production for a sequence of terminals.  Useful for
-- parsing specific words.
terminalSeq

  :: RuleName

  -> Seq t
  -- ^ Sequence of terminal symbols to recognize

  -> Pinchot t (Rule t)

terminalSeq name sq = newRule name (RSeqTerm sq)

-- | Creates a new non-terminal production rule.
nonTerminal

  :: RuleName

  -> Seq (AlternativeName, Seq (Rule t))
  -- ^ Alternatives.  There must be at least one alternative;
  -- otherwise, an error will result.  In each pair @(a, b)@, @a@ will
  -- be the data constructor, so this must be a valid Haskell data
  -- constructor name.  @b@ is the sequence of production rules, which
  -- can be empty (this is how to create an epsilon production).

  -> Pinchot t (Rule t)

nonTerminal name sq = do
  (b1, bs) <- splitNonTerminal name sq
  let branches = RBranch (uncurry Branch b1, fmap (uncurry Branch) bs)
  newRule name branches

ruleConstructorNames
  :: Rule t
  -> Seq AlternativeName
ruleConstructorNames (Rule n _ t) = case t of
  RTerminal _ -> Seq.singleton n
  RBranch (b1, bs) -> branchName b1 <| fmap branchName bs
    where
      branchName (Branch x _) = x
  RUnion (b1, bs) -> branchName b1 <| fmap branchName bs
    where
      branchName (Rule x _ _) = unionBranchName n x
  RSeqTerm _ -> Seq.singleton n
  ROptional _ -> Seq.singleton n
  RList _ -> Seq.singleton n
  RList1 _ -> Seq.singleton n
  RWrap _ -> Seq.singleton n
  RRecord _ -> Seq.singleton n

unionBranchName
  :: RuleName
  -- ^ Name of the parent rule
  -> RuleName
  -- ^ Name of the branch rule
  -> AlternativeName
unionBranchName p b = p ++ '\'' : b

addDataConNames :: Rule t -> Pinchot t ()
addDataConNames = mapM_ addDataConName . ruleConstructorNames

-- | Creates a new non-terminal production rule where each alternative
-- produces only one rule.  The constructor name for each alternative
-- is
--
-- @RULE_NAME'PRODUCTION_NAME@
--
-- where @RULE_NAME@ is the name of the rule itself, and
-- @PRODUCTION_NAME@ is the rule name for what is being produced.  For
-- an example, see 'Pinchot.Examples.PostalAstAllRules.Suffix'.
--
-- Currently there is no way to change the names of the constructors;
-- however, you can use 'nonTerminal', which is more flexible.
union
  :: RuleName
  -> Seq (Rule t)
  -- ^ List of alternatives.  There must be at least one alternative;
  -- otherwise a compile-time error will occur.
  -> Pinchot t (Rule t)
union name sq = Pinchot $ case viewl sq of
  EmptyL -> throwE $ EmptyNonTerminal name
  x :< xs -> runPinchot $ newRule name (RUnion (x, xs))

-- | Creates a new non-terminal production rule with only one
-- alternative where each field has a record name.  The name of each
-- record is:
--
-- @_r\'RULE_NAME\'INDEX\'FIELD_TYPE@
--
-- where @RULE_NAME@ is the name of this rule, @INDEX@ is the index number
-- for this field (starting with 0), and @FIELD_TYPE@ is the type of the
-- field itself.  For an example, see
-- 'Pinchot.Examples.PostalAstAllRules.Address'.
--
-- Currently there is no way to change the names of the record fields.
record
  :: RuleName
  -- ^ The name of this rule, which is used both as the type name and
  -- the name of the sole data constructor.
  -> Seq (Rule t)
  -- ^ The right-hand side of this rule.  This sequence can be empty,
  -- which results in an epsilon production.
  -> Pinchot t (Rule t)
record name sq = newRule name (RRecord sq)


-- | Creates a rule for the production of a sequence of other rules.
-- The name for the created 'Rule' is the name of the 'Rule' to which
-- this function is applied, with @'Seq@ appended.
list
  :: Rule t
  -- ^ The resulting 'Rule' is a sequence of productions of this
  -- 'Rule'; that is, this 'Rule' may appear zero or more times.

  -> Pinchot t (Rule t)
list r@(Rule inner _ _) = newRule name (RList r)
  where
    name = inner ++ "'Seq"

-- | Creates a rule for a production that appears at least once.  The
-- name for the created 'Rule' is the name of the 'Rule' to which this
-- function is applied, with @'Seq1@ appended.
list1
  :: Rule t
  -- ^ The resulting 'Rule' produces this 'Rule' at least once.
  -> Pinchot t (Rule t)
list1 r@(Rule inner _ _) = newRule name (RList1 r)
  where
    name = inner ++ "'Seq1"

-- | Creates a rule for a production that optionally produces another
-- rule.  The name for the created 'Rule' is the name of the 'Rule' to
-- which this function is applied, with @'Maybe@ appended to the end.
option
  :: Rule t
  -- ^ The resulting 'Rule' optionally produces this 'Rule'; that is,
  -- this 'Rule' may appear once or not at all.

  -> Pinchot t (Rule t)
option r@(Rule inner _ _) = newRule name (ROptional r)
  where
    name = inner ++ "'Maybe"

-- | Creates a newtype wrapper.

wrap
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' simply wraps this 'Rule'.
  -> Pinchot t (Rule t)
wrap name r = newRule name (RWrap r)

-- | Gets all ancestor 'Rule's.  Skips duplicates.
getAncestors
  :: Rule t
  -> State (Set String) (Seq (Rule t))
getAncestors r@(Rule name _ ei) = do
  set <- get
  if Set.member name set
    then return Seq.empty
    else do
      put (Set.insert name set)
      case ei of
        RTerminal _ -> return (Seq.singleton r)
        RBranch (b1, bs) -> do
          as1 <- branchAncestors b1
          ass <- fmap join . mapM branchAncestors $ bs
          return $ r <| as1 <> ass
        RUnion (b1, bs) -> do
          c1 <- getAncestors b1
          cs <- fmap join . mapM getAncestors $ bs
          return $ r <| c1 <> cs
        RSeqTerm _ -> return (Seq.singleton r)
        ROptional c -> do
          cs <- getAncestors c
          return $ r <| cs
        RList c -> do
          cs <- getAncestors c
          return $ r <| cs
        RList1 c -> do
          cs <- getAncestors c
          return $ r <| cs
        RWrap c -> do
          cs <- getAncestors c
          return $ r <| cs
        RRecord ls -> do
          cs <- fmap join . mapM getAncestors $ ls
          return $ r <| cs
  where
    branchAncestors (Branch _ rs) = fmap join . mapM getAncestors $ rs

-- | Returns both this 'Rule' and any 'Rule's that are ancestors.
ruleAndAncestors
  :: Rule t
  -> Seq (Rule t)
ruleAndAncestors r = fst $ runState (getAncestors r) Set.empty

-- | Given a sequence of 'Rule', determine which rules are on a
-- right-hand side before they are defined.
rulesDemandedBeforeDefined :: Foldable f => f (Rule t) -> Set Name
rulesDemandedBeforeDefined = snd . foldl f (Set.empty, Set.empty)
  where
    f (lhsDefined, results) (Rule nm _ ty)
      = (Set.insert nm lhsDefined, results')
      where
        results' = case ty of
          RTerminal _ -> results
          RBranch (b1, bs) -> foldr checkBranch (checkBranch b1 results) bs
            where
              checkBranch (Branch _ rls) rslts = foldr checkRule rslts rls
          RUnion (b1, bs) -> foldr checkRule (checkRule b1 results) bs
          RSeqTerm _ -> results
          ROptional r -> checkRule r results
          RList r -> addHelper $ checkRule r results
          RList1 r -> addHelper $ checkRule r results
          RWrap r -> checkRule r results
          RRecord sq -> foldr checkRule results $ sq
        checkRule (Rule name _ _) rslts
          | Set.member name lhsDefined = rslts
          | otherwise = Set.insert (ruleName name) rslts
        addHelper = Set.insert (helperName nm)
  

