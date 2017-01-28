{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
module Pinchot.SyntaxTree.Instancer where

import Control.Monad (replicateM)
import qualified Data.Bifunctor as Bifunctor
import Data.Maybe (catMaybes)
import qualified Data.Semigroup as Semigroup
import qualified Control.Lens as Lens
import Data.Foldable (foldlM, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.List.NonEmpty (NonEmpty)
import qualified Language.Haskell.TH as T
import qualified Text.Show.Pretty as Pretty

import Pinchot.Types
import Pinchot.Pretty
import Pinchot.Rules

-- | Creates an instance of 'Bifunctor.Bifunctor' for every 'Rule'
-- in the 'Seq' as well as their ancestors.
--
-- This function must be
-- spliced in the same module as the module in which the syntax tree
-- types are created.  This avoids problems with orphan instances.
-- Since ancestors are included, you can get the entire tree of
-- instances that you need by applying this function to a single
-- start symbol.
--
-- Example: "Pinchot.Examples.SyntaxTrees".

bifunctorInstances
  :: [Rule t]
  -> T.DecsQ
bifunctorInstances = traverse f . families
  where
    f rule@(Rule ruleName _ _) = T.instanceD (T.cxt [])
      [t| Bifunctor.Bifunctor $(T.conT (T.mkName ruleName)) |]
      [T.valD (T.varP 'Bifunctor.bimap)
              (T.normalB (bimapExpression "" rule)) []]

-- | Where possible, creates an instance of 'Semigroup.Semigroup'
-- for every 'Rule' in the 'Seq' as well as their ancestors.  Only
-- 'star' and 'plus' rules, as well as rules that through one or
-- more layers of 'wrap' ultimately wrap a 'star' or 'plus' rule,
-- get a 'Semigroup.Semigroup' instance.
--
-- This function must be
-- spliced in the same module as the module in which the syntax tree
-- types are created.  This avoids problems with orphan instances.
-- Since ancestors are included, you can get the entire tree of
-- instances that you need by applying this function to a single
-- start symbol.
--
-- Example: "Pinchot.Examples.SyntaxTrees".

semigroupInstances
  :: [Rule t]
  -> T.DecsQ
semigroupInstances = fmap catMaybes . traverse f . toList . families
  where
    nameT = T.varT $ T.mkName "t"
    nameA = T.varT $ T.mkName "a"
    f rule@(Rule ruleName _ _) = case semigroupExpression "" rule of
      Nothing -> return Nothing
      Just expn -> fmap Just
        $ T.instanceD (T.cxt [])
          [t| Semigroup.Semigroup
            ( $(T.conT (T.mkName ruleName)) $(nameT) $(nameA)) |]
          [T.valD (T.varP '(Semigroup.<>))
                  (T.normalB expn) []]

-- | Where possible, creates an instance of 'Monoid'
-- for every 'Rule' in the 'Seq' as well as their ancestors.  Only
-- 'star' rules, as well as rules that through one or
-- more layers of 'wrap' ultimately wrap a 'star' rule,
-- get a 'Monoid' instance.
--
-- This function must be
-- spliced in the same module as the module in which the syntax tree
-- types are created.  This avoids problems with orphan instances.
-- Since ancestors are included, you can get the entire tree of
-- instances that you need by applying this function to a single
-- start symbol.
--
-- Example: "Pinchot.Examples.SyntaxTrees".
monoidInstances
  :: [Rule t]
  -> T.DecsQ
monoidInstances = fmap catMaybes . traverse f . toList . families
  where
    nameT = T.varT $ T.mkName "t"
    nameA = T.varT $ T.mkName "a"
    f rule@(Rule ruleName _ _)
      = case (semigroupExpression "" rule, memptyExpression "" rule) of
      (Just expAppend, Just expMempty) -> fmap Just
        $ T.instanceD (T.cxt [])
          [t| Monoid ( $( T.conT (T.mkName ruleName) ) $(nameT) $(nameA) ) |]
          [ T.valD (T.varP 'mappend)
                   (T.normalB expAppend) []
          , T.valD (T.varP 'mempty)
                   (T.normalB expMempty) [] ]
      _ -> return Nothing



-- Creates an expression of type
--
-- (a -> b) -> (c -> d) -> RuleData a c -> RuleData b d
bimapExpression
  :: Qualifier
  -> Rule t
  -> T.ExpQ
bimapExpression qual rule@(Rule ruleName _ _) = do
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
  NonTerminal bs -> return $ nonTerminalBimapLetBind qual lkp name
    (toList bs)
  Wrap (Rule inner _ _) -> wrapBimapLetBind qual lkp name inner
  Record sq -> recordBimapLetBind qual lkp name sq
  Opt (Rule inner _ _) -> optBimapLetBind qual lkp name inner
  Star (Rule inner _ _) -> starBimapLetBind qual lkp name inner
  Plus (Rule inner _ _) -> plusBimapLetBind qual lkp name inner
  Series _ -> seriesBimapLetBind qual fa fb lkp name


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
  -> Map RuleName T.Name
  -> RuleName
  -> [Branch a]
  -> T.DecQ
nonTerminalBimapLetBind qual lkp name branches =
  let mkClause (Branch branchName branches)
        = recordBimapClause qual lkp branchName branches
      clauses = fmap mkClause branches
  in T.funD (errLookup name lkp) clauses

wrapBimapLetBind
  :: Qualifier
  -> Map RuleName T.Name
  -> RuleName
  -> RuleName
  -> T.Q T.DecQ
wrapBimapLetBind qual lkp name inner = do
  val <- T.newName "wrapLetBind"
  let expn = T.lamE [T.conP (quald qual name) [T.varP val]]
        [| $(T.conE (quald qual name))
            ( $(T.varE (errLookup inner lkp)) $(T.varE val) ) |]
  return $ T.valD (T.varP (errLookup name lkp)) (T.normalB expn) []

recordBimapLetBind
  :: Qualifier
  -> Map RuleName T.Name
  -> RuleName
  -> [Rule t]
  -> T.Q T.DecQ
recordBimapLetBind qual lkp name sq = do
  let clause = recordBimapClause qual lkp name sq
  return $ T.funD (errLookup name lkp) [clause]

recordBimapClause
  :: Qualifier
  -> Map RuleName T.Name
  -> RuleName
  -> [Rule t]
  -> T.ClauseQ
recordBimapClause qual lkp name sq = do
  pairs <- traverse (recordBimapLetBindField lkp) . toList $ sq
  let body = foldl f (T.conE (quald qual name)) . fmap snd $ pairs
        where
          f acc expn = [| $(acc) $(expn) |]
  let pats = fmap fst pairs
  T.clause [T.conP (quald qual name) pats] (T.normalB body) []

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
                  Just v -> Just $ $(T.varE (errLookup inner lkp)) v
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
                $ fmap $(T.varE (errLookup inner lkp)) $(T.varE val) |]
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
                $ fmap $(T.varE (errLookup inner lkp)) $(T.varE val) |]
  let clause = T.clause [T.conP (quald qual name) [T.varP val]]
        (T.normalB body) []
  return $ T.funD (errLookup name lkp) [clause]

seriesBimapLetBind
  :: Qualifier
  -> T.Name
  -> T.Name
  -> Map RuleName T.Name
  -> RuleName
  -> T.Q T.DecQ
seriesBimapLetBind qual fa fb lkp name = do
  val <- T.newName $ "termBimapLetBind" ++ name
  let body = T.lamE [T.conP (quald qual name) [T.varP val]]
        [| $(T.conE (quald qual name))
              (fmap ( Bifunctor.bimap $(T.varE fa) $(T.varE fb) )
                    $(T.varE val) ) |]
  return $ T.valD (T.varP (errLookup name lkp)) (T.normalB body) []

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
  -> [RuleName]
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
  -> Maybe ([RuleName])
monoidCtors (Rule ruleName _ ty) = case ty of
  Wrap r -> do
    rest <- monoidCtors r
    return $ ruleName `Lens.cons` rest
  Star _ -> Just [ruleName]
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
  -> Maybe ([RuleName])
semigroupCtors (Rule ruleName _ ty) = case ty of
  Wrap r -> do
    rest <- semigroupCtors r
    return $ ruleName `Lens.cons` rest
  Plus _ -> Just [ruleName]
  Star _ -> Just [ruleName]
  _ -> Nothing

wrappedSemigroupExpression
  :: T.Name
  -- ^ mappend operator
  -> Qualifier
  -> [RuleName]
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
            f rule acc = T.appE (T.conE (quald qual rule)) acc

-- | Creates an expression of type
--
-- RuleData -> Value
prettyExpression
  :: Qualifier
  -> Rule t
  -> T.ExpQ
prettyExpression qual rule@(Rule name _ _) = do
  lkp <- nameMap rule
  thisRule <- lookupRule lkp name
  let decs = map mkDec . toList . family $ rule
        where
          mkDec inner = do
            innerNm <- lookupRule lkp (_ruleName inner)
            T.valD (T.varP innerNm)
              (T.normalB (prettyExpressionInEnv qual lkp inner)) []
  T.letE decs (T.varE thisRule)

-- | Creates a 'Pretty.PrettyVal' instance.  This must be spliced into the same
-- module in which the corresponding data type is spliced.
prettyInstance
  :: Rule t
  -> T.DecQ
prettyInstance rule = do
  let a = T.varT $ T.mkName "a"
      t = T.varT $ T.mkName "t"
      ruleTypeName = T.conT . T.mkName . _ruleName $ rule
      cxt = [ [t| Pretty.PrettyVal $t |]
            , [t| Pretty.PrettyVal $a |]
            ]
      ty = [t| Pretty.PrettyVal ( $ruleTypeName $t $a ) |]
      dec = T.funD 'Pretty.prettyVal [clause]
        where
          clause = T.clause [] (T.normalB (prettyExpression "" rule)) []
  T.instanceD (T.cxt cxt) ty [dec]

-- | Creates a 'Pretty.PrettyVal' instance for a rule and all its
-- ancestors.
prettyInstanceFamily
  :: Rule t
  -> T.DecsQ
prettyInstanceFamily
  = fmap toList . sequence . fmap prettyInstance . family

-- | Creates a 'Pretty.PrettyVal' instance for each 'Rule' in the
-- 'Seq', as well as for all the ancestors of each 'Rule' in the
-- 'Seq'.
--
-- This function must be
-- spliced in the same module as the module in which the syntax tree
-- types are created.  This avoids problems with orphan instances.
-- Since ancestors are included, you can get the entire tree of
-- instances that you need by applying this function to a single
-- start symbol.
--
-- Example: "Pinchot.Examples.SyntaxTrees".
prettyInstances
  :: [Rule t]
  -> T.DecsQ
prettyInstances
  = fmap toList . sequence . fmap prettyInstance . families

-- | Creates an expression of type
--
-- RuleData -> Value
prettyExpressionInEnv
  :: Qualifier
  -> Map RuleName T.Name
  -- ^ All expressions
  -> Rule t
  -> T.ExpQ
prettyExpressionInEnv qual lkp (Rule name _ ty) = case ty of
  Terminal _ -> do
    x <- T.newName "x"
    [| \ $(T.conP (quald qual name) [T.varP x])
          -> Pretty.Con name [Pretty.prettyVal $(T.varE x)] |]
  NonTerminal sq -> prettyBranches qual lkp sq
  Wrap (Rule inner _ _) -> do
    x <- T.newName "x"
    fVal <- lookupRule lkp inner
    [| \ $(T.conP (quald qual name) [T.varP x])
         -> Pretty.Con name [$(T.varE fVal) $(T.varE x)] |]
  Record rules -> do
    (pat, expn) <- prettyConstructor qual lkp name rules
    [| \ $pat -> $expn |]
  Opt (Rule inner _ _) -> do
    x <- T.newName "x"
    fVal <- lookupRule lkp inner
    [| \ $(T.conP (quald qual name) [T.varP x]) ->
      Pretty.Con name [prettyMaybe $(T.varE fVal) $(T.varE x)] |]
  Star (Rule inner _ _) -> do
    x <- T.newName "x"
    fVal <- lookupRule lkp inner
    [| \ $(T.conP (quald qual name) [T.varP x]) ->
       Pretty.Con name [prettyList $(T.varE fVal) $(T.varE x)] |]
  Plus (Rule inner _ _) -> do
    x <- T.newName "x"
    fVal <- lookupRule lkp inner
    [| \ $(T.conP (quald qual name) [T.varP x]) ->
       Pretty.Con name [prettyNonEmpty $(T.varE fVal) $(T.varE x)] |]
  Series _ -> do
    x <- T.newName "x"
    [| \ $(T.conP (quald qual name) [T.varP x])
         -> Pretty.Con name
            [prettyNonEmpty Pretty.prettyVal $(T.varE x)] |]

prettyBranches
  :: Qualifier
  -> Map RuleName T.Name
  -> NonEmpty (Branch t)
  -> T.ExpQ
prettyBranches qual lkp branches = do
  x <- T.newName "x"
  T.lam1E (T.varP x) . T.caseE (T.varE x)
    . fmap (prettyBranch qual lkp) . toList $ branches

lookupRule
  :: Map RuleName T.Name
  -> RuleName
  -> T.Q T.Name
lookupRule mp name = case Map.lookup name mp of
  Nothing -> fail $ "rule lookup failed: " ++ name
  Just n -> return n

-- | Creates a 'T.MatchQ' when given a value constructor name and
-- functions that work on the field values.
deconstruct
  :: T.Name
  -- ^ Name of the constructor
  -> Int
  -- ^ Create this many fields
  -> ([T.ExpQ] -> T.ExpQ)
  -- ^ Each expression that results from each field
  -- is passed to this function.
  -> T.Q (T.PatQ, T.ExpQ)
deconstruct ctorName numFields getFinal = do
  names <- replicateM numFields (T.newName "x")
  let pat = T.conP ctorName . map T.varP $ names
  let body = getFinal . map T.varE $ names
  return (pat, body)

prettyBranch
  :: Qualifier
  -> Map RuleName T.Name
  -> Branch t
  -> T.MatchQ
prettyBranch qual lkp (Branch branchName branches)
  = prettyConstructor qual lkp branchName branches >>= getMatch
  where
    getMatch (pat, expn) = T.match pat (T.normalB expn) []

prettyConstructor
  :: Qualifier
  -> Map RuleName T.Name
  -> String
  -- ^ Name of branch, or name of data constructor
  -> [Rule t]
  -> T.Q (T.PatQ, T.ExpQ)
prettyConstructor qual lkp branchName branches
  = deconstruct (quald qual branchName) (length fieldNames)
      getFinal
  where
    fieldNames = toList . fmap _ruleName $ branches
    getFinal fields = [| Pretty.Con branchName $(values) |]
      where
        values = T.listE $ zipWith getField fields fieldNames
          where
            getField field fieldName = do
              rule <- lookupRule lkp fieldName
              [| $(T.varE rule) $(field) |]
