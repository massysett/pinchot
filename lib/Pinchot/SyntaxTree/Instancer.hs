{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Pinchot.SyntaxTree.Instancer where

import qualified Control.Lens as Lens
import Data.Foldable (foldlM, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Language.Haskell.TH as T

import Pinchot.Types
import Pinchot.Rules

-- Creates an expression of type
--
-- (a -> b) -> (c -> d) -> RuleData a c -> RuleData b d
bimapExpression
  :: Qualifier
  -> Rule t
  -> T.ExpQ
bimapExpression qual rule@(Rule ruleName _ ty) = do
  fa <- T.newName "fa"
  fb <- T.newName "fb"
  val <- T.newName "val"
  lkpName <- nameMap rule
  letBinds <- bimapLetBinds qual fa fb lkpName rule
  let resultExpn =
        [| $(T.varE $ errLookup ruleName lkpName) $(T.varE val) |]
  T.lamE [T.varP fa, T.varP fb, T.varP val]
    (T.letE letBinds resultExpn)


bimapLetBinds
  :: Qualifier
  -> T.Name
  -- ^ @(a -> b)@
  -> T.Name
  -- ^ @(c -> d)@
  -> Map RuleName T.Name
  -- ^ Looks up names for other let binds
  -> Rule t
  -> T.Q [T.DecQ]
bimapLetBinds qual fa fb lkp rule
  = traverse (bimapLetBind qual fa fb lkp) . toList . family $ rule

bimapLetBind
  :: Qualifier
  -> T.Name
  -- ^ @(a -> b)@
  -> T.Name
  -- ^ @(c -> d)@
  -> Map RuleName T.Name
  -> Rule t
  -> T.Q T.DecQ
bimapLetBind qual fa fb lkp (Rule name _ ty) = case ty of
  Terminal _ -> terminalBimapLetBind qual fa fb lkp name
  NonTerminal b1 bs -> nonTerminalBimapLetBind qual fa fb lkp name
    (b1 : toList bs)
  Wrap (Rule inner _ _) -> wrapBimapLetBind qual fa fb lkp name inner
  Record sq -> recordBimapLetBind qual fa fb lkp name sq
  Opt (Rule inner _ _) -> optBimapLetBind qual lkp name inner
  Star (Rule inner _ _) -> starBimapLetBind qual lkp name inner
  Plus (Rule inner _ _) -> plusBimapLetBind qual lkp name inner


terminalBimapLetBind
  :: Qualifier
  -> T.Name
  -> T.Name
  -> Map RuleName T.Name
  -> RuleName
  -> T.Q T.DecQ
terminalBimapLetBind qual fa fb lkp name = do
  val <- T.newName $ "terminalBimapLetBind" ++ name
  let body = T.lamE [T.conP (quald qual name) [T.varP val]]
        [| $(T.conE (quald qual name))
              ( $(T.varE fa) . fst $ $(T.varE val)
              , $(T.varE fb) . snd $ $(T.varE val)
              )
        |]
  return $ T.valD (T.varP (errLookup name lkp)) (T.normalB body) []

nonTerminalBimapLetBind
  :: Qualifier
  -> T.Name
  -> T.Name
  -> Map RuleName T.Name
  -> RuleName
  -> [Branch a]
  -> T.Q T.DecQ
nonTerminalBimapLetBind qual fa fb lkp name branches = do
  val <- T.newName $ "nonTerminalBimapLetBind" ++ name
  return $ T.funD val 
  {-
  let matches = fmap (bimapBranch qual lkp fa fb) branches
      expn = T.lamE [T.varP val] (T.caseE (T.varE val) matches)
  return $ T.valD (T.varP (errLookup name lkp)) (T.normalB expn) []
  -}

wrapBimapLetBind
  :: Qualifier
  -> T.Name
  -> T.Name
  -> Map RuleName T.Name
  -> RuleName
  -- ^ Name of this rule
  -> RuleName
  -- ^ Name of inner rule
  -> T.Q T.DecQ
wrapBimapLetBind qual fa fb lkp name inner = do
  val <- T.newName "wrapLetBind"
  let expn = T.lamE [T.conP (quald qual name) [T.varP val]]
        [| $(T.conE (quald qual name))
            ( $(T.varE (errLookup inner lkp)) $(T.varE val) ) |]
  return $ T.valD (T.varP (errLookup name lkp)) (T.normalB expn) []

recordBimapLetBind
  :: Qualifier
  -> T.Name
  -> T.Name
  -> Map RuleName T.Name
  -> RuleName
  -> Seq (Rule t)
  -> T.Q T.DecQ
recordBimapLetBind qual fa fb lkp name sq = do
  pairs <- traverse (recordBimapLetBindField lkp) . toList $ sq
  let body = foldl f (T.conE (quald qual name)) . fmap snd $ pairs
        where
          f acc expn = [| $(acc) $(expn) |]
  let pats = fmap fst pairs
  let clause = T.clause [T.conP (quald qual name) pats]
        (T.normalB body) []
  return $ T.funD (errLookup name lkp) [clause]

recordBimapLetBindField
  :: Map RuleName T.Name
  -> Rule t
  -> T.Q (T.PatQ, T.ExpQ)
recordBimapLetBindField lkp (Rule name _ _) = do
  tName <- T.newName $ "recordField" ++ name
  let expn = [| $(T.varE (errLookup name lkp)) $(T.varE tName) |]
  return (T.varP tName, expn)

optBimapLetBind
  :: Qualifier
  -> Map RuleName T.Name
  -> RuleName
  -- ^ Name of this rule
  -> RuleName
  -- ^ Name of inner rule
  -> T.Q T.DecQ
optBimapLetBind qual lkp name inner = do
  val <- T.newName $ "optBimapLetBind" ++ name
  let body = [| $(T.conE (quald qual name)) $ case $(T.varE val) of
                  Nothing -> Nothing
                  Just v -> $(T.varE (errLookup inner lkp)) v
             |]
  let clause = T.clause [T.conP (quald qual name) [T.varP val]]
        (T.normalB body) []
  return $ T.funD (errLookup name lkp) [clause]

starBimapLetBind
  :: Qualifier
  -> Map RuleName T.Name
  -> RuleName
  -- ^ Name of this rule
  -> RuleName
  -- ^ Name of inner rule
  -> T.Q T.DecQ
starBimapLetBind qual lkp name inner = do
  val <- T.newName $ "starBimapLetBind" ++ name
  let body = [| $(T.conE (quald qual name))
                $ fmap ( $(T.varE (errLookup inner lkp)) $(T.varE val)) |]
  let clause = T.clause [T.conP (quald qual name) [T.varP val]]
        (T.normalB body) []
  return $ T.funD (errLookup name lkp) [clause]

plusBimapLetBind
  :: Qualifier
  -> Map RuleName T.Name
  -> RuleName
  -- ^ Name of this rule
  -> RuleName
  -- ^ Name of inner rule
  -> T.Q T.DecQ
plusBimapLetBind qual lkp name inner = do
  val <- T.newName $ "plusBimapLetBind" ++ name
  let body = [| $(T.conE (quald qual name))
                $ fmap ( $(T.varE (errLookup inner lkp)) $(T.varE val)) |]
  let clause = T.clause [T.conP (quald qual name) [T.varP val]]
        (T.normalB body) []
  return $ T.funD (errLookup name lkp) [clause]


errLookup
  :: (Ord a, Show a)
  => a
  -> Map a b
  -> b
errLookup k m = case Map.lookup k m of
  Nothing -> error $ "lookup: key not found: " ++ show k
  Just v -> v

-- | Creates a map of all ancestor rule names and a corresponding TH
-- name.
nameMap
  :: Rule t
  -> T.Q (Map RuleName T.Name)
nameMap = foldlM f Map.empty . family
  where
    f acc (Rule ruleName _ _) = do
      thName <- T.newName $ "bindName" ++ ruleName
      return $ Map.insert ruleName thName acc

bimapBranch
  :: Qualifier
  -> Map RuleName T.Name
  -- ^ Name of expression for each rule
  -> T.Name
  -- ^ Expression for @a -> b@
  -> T.Name
  -- ^ Expression for @c -> d@
  -> Branch t
  -> T.Q T.DecQ
bimapBranch qual lkp fa fb (Branch branchName rules)
  = recordBimapLetBind qual fa fb lkp branchName rules
  

{-
bimapBranch
  :: Qualifier
  -> Map RuleName T.Name
  -- ^ Name of expression for each rule
  -> T.Name
  -- ^ Expression for @a -> b@
  -> T.Name
  -- ^ Expression for @c -> d@
  -> Branch t
  -> T.MatchQ
bimapBranch qual ruleExps expA expB (Branch branchName rules) = do
  let f (pats, exps) (Rule ruleName _ _) = do
        let ruleExp = errLookup ruleName ruleExps
        x <- T.newName "x"
        let pat' = pats `Lens.snoc` (T.varP x)
            exp' = [| $(T.varE ruleExp) $(T.varE expA) $(T.varE expB)
              $(T.varE x) |]
        return (pat', exp')
  (pats, expn) <- foldlM f (Seq.empty, [| T.varE (quald qual branchName) |])
    rules
  let pat = T.conP (quald qual branchName) (toList pats)
      body = T.normalB expn
  T.match pat body []
-}

    

-- | If possible, creates an expression of type
--
-- RuleData
--
-- which is a Monoid 'mempty'.
memptyExpression
  :: Qualifier
  -> Rule t
  -> Maybe T.ExpQ
memptyExpression qual rule = do
  ctors <- monoidCtors rule
  return $ wrappedMemptyExpression qual ctors

wrappedMemptyExpression
  :: Qualifier
  -> Seq RuleName
  -> T.ExpQ
wrappedMemptyExpression qual rules = foldr f [| mempty |] rules
  where
    f name acc = [| $(T.conE (quald qual name)) $(acc) |]

-- | If possible, creates an expression of type
--
-- RuleData -> RuleData -> RuleData
--
-- which is a Monoid 'mappend'.
mappendExpression
  :: Qualifier
  -> Rule t
  -> Maybe T.ExpQ
mappendExpression qual rule = do
  ctors <- monoidCtors rule
  return $ wrappedSemigroupExpression 'mappend qual ctors

monoidCtors
  :: Rule t
  -> Maybe (Seq RuleName)
monoidCtors (Rule ruleName _ ty) = case ty of
  Wrap r -> do
    rest <- monoidCtors r
    return $ ruleName `Lens.cons` rest
  Star _ -> Just (Seq.singleton ruleName)
  _ -> Nothing

-- | If possible, creates an expression of type
--
-- RuleData -> RuleData -> RuleData
--
-- which is a Semigroup '<>'.

semigroupExpression
  :: Qualifier
  -> Rule t
  -> Maybe T.ExpQ
semigroupExpression qual rule = do
  ctors <- semigroupCtors rule
  return $ wrappedSemigroupExpression '(<>) qual ctors


semigroupCtors
  :: Rule t
  -> Maybe (Seq RuleName)
semigroupCtors (Rule ruleName _ ty) = case ty of
  Wrap r -> do
    rest <- semigroupCtors r
    return $ ruleName `Lens.cons` rest
  Plus _ -> Just (Seq.singleton ruleName)
  _ -> Nothing

wrappedSemigroupExpression
  :: T.Name
  -- ^ mappend operator
  -> Qualifier
  -> Seq RuleName
  -- ^ Rule names, with the outermost name on the left side of the
  -- 'Seq'.
  -> T.ExpQ
  -- ^ An expression of type
  --
  -- RuleData -> RuleData -> RuleData
  --
  -- It unwraps the two argument types, uses the Semigroup @<>@
  -- operator to combine them, and rewraps the result.
wrappedSemigroupExpression append qual rules = names >>= makeExp
  where
    names = (,) <$> T.newName "x1" <*> T.newName "x2"
    makeExp (x1, x2) = [| \ $(mkPat x1) $(mkPat x2) -> $(mkRes) |]
      where
        mkPat name = foldr f (T.varP name) rules
          where
            f rule acc = T.conP (quald qual rule) [acc]
        mkRes = foldr f
          [| $(T.varE append) $(T.varE x1) $(T.varE x2) |] rules
          where
            f rule acc = T.appE (T.varE (quald qual rule)) acc
