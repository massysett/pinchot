{-# LANGUAGE TemplateHaskell #-}
module Pinchot.Terminalize where

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (foldlM, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Syntax as Syntax

import Pinchot.NonEmpty (NonEmpty)
import qualified Pinchot.NonEmpty as NonEmpty
import Pinchot.Types
import Pinchot.Rules

-- | For all the given rules and their ancestors, creates
-- declarations that reduce the rule and all its ancestors to
-- terminal symbols.  Each rule gets a declaration named
-- @t'RULE_NAME@ where @RULE_NAME@ is the name of the rule.  The
-- type of the declaration is
--
-- Production a -> Seq (t, a)
--
-- where @Production@ is the production corresponding to the given
-- 'Rule', and @t@ is the terminal token type.
terminalizers
  :: Foldable c
  => Qualifier
  -- ^ Qualifier for the module containing the data types created
  -- from the 'Rule's
 
  -> T.Name
  -- ^ Name of terminal type.  Typically you will get this through
  -- the Template Haskell quoting mechanism, such as @''Char@.

  -> c (Rule t)
  -> T.Q [T.Dec]

terminalizers qual termType
  = fmap concat
  . traverse (terminalizer qual termType)
  . toList

-- | For the given rule, creates declarations that reduce the rule
-- to terminal symbols.  No ancestors are handled.  Each rule gets a
-- declaration named @t'RULE_NAME@ where @RULE_NAME@ is the name of
-- the rule.  The type of the declaration is
--
-- Production a -> Seq (t, a)
--
-- where @Production@ is the production corresponding to the given
-- 'Rule', and @t@ is the terminal token type.
terminalizer
  :: Qualifier
  -- ^ Qualifier for the module containing the data types created
  -- from the 'Rule's
 
  -> T.Name
  -- ^ Name of terminal type.  Typically you will get this through
  -- the Template Haskell quoting mechanism, such as @''Char@.

  -> Rule t
  -> T.Q [T.Dec]

terminalizer qual termType rule@(Rule nm _ _) = sequence [sig, expn]
  where
    declName = "t'" ++ nm
    anyType = T.varT (T.mkName "a")
    sig = T.sigD (T.mkName declName)
      [t| $(T.varT (quald qual nm)) $(anyType)
          -> Seq ($(T.varT termType), $anyType) |]
    expn = T.valD (T.varP $ T.mkName declName)
      (T.normalB (terminalizeRuleExp qual rule)) []

-- | For the given rule, returns an expression that has type
--
-- Production a -> Seq (t, a)
--
-- where @Production@ is the production corresponding to the given
-- 'Rule', and t is the terminal token type.
terminalizeRuleExp
  :: Qualifier
  -> Rule t
  -> T.Q T.Exp
terminalizeRuleExp qual rule@(Rule nm _ _) = do
  let allRules = family rule 
  lkp <- ruleLookupMap allRules
  let mkDec r@(Rule rn _ _) =
        let expn = terminalizeSingleRule qual lkp r
            decName = lookupName lkp rn
        in T.valD (T.varP decName) (T.normalB expn) []
  T.letE (fmap mkDec . toList $ allRules) (T.varE (lookupName lkp nm))

-- | Creates a 'Map' where each key is the name of the 'Rule' and
-- each value is a name corresponding to that 'Rule'.  No
-- ancestors are used.
ruleLookupMap
  :: Foldable c
  => c (Rule t)
  -> T.Q (Map RuleName (T.Name))
ruleLookupMap = foldlM f Map.empty
  where
    f mp (Rule nm _ _) = do
      name <- T.newName $ "rule" ++ nm
      return $ Map.insert nm name mp

lookupName
  :: Map RuleName T.Name
  -> RuleName
  -> T.Name
lookupName lkp n = case Map.lookup n lkp of
  Nothing -> error $ "lookupName: name not found: " ++ n
  Just r -> r

-- | For the given rule, returns an expression that has type
--
-- Production a -> Seq (t, a)
--
-- where @Production@ is the production corresponding to the given
-- 'Rule', and t is the terminal token type.  Gets no ancestors.
terminalizeSingleRule
  :: Qualifier
  -- ^ Module qualifier for module containing the generated types
  -- corresponding to all 'Rule's
  -> Map RuleName T.Name
  -- ^ For a given Rule, looks up the name of the expression that
  -- will terminalize that rule.
  -> Rule t
  -> T.Q T.Exp
terminalizeSingleRule qual lkp rule@(Rule nm _ ty) = case ty of
  Terminal _ -> [| NonEmpty.singleton . coerce |]

  NonTerminal b1 bs -> do
    x <- T.newName "x"
    let pat = T.varP x
        tzr
          | atLeastOne rule = terminalizeBranchAtLeastOne
          | otherwise = terminalizeBranchAllowsZero
        m1 = tzr qual lkp b1
        ms = fmap (tzr qual lkp) . toList $ bs
    T.lamE [T.varP x] (T.caseE (T.varE x) (m1 : ms))

  Terminals sq
    | not . Seq.null $ sq ->
        [| \s -> case NonEmpty.seqToNonEmpty . coerce $ s of
                  Nothing -> error
                    $ "terminals: is empty: " ++ $(Syntax.lift nm)
                  Just ne -> ne
        |]
    | otherwise -> [| coerce |]

  Wrap (Rule inner _ _) ->
    [| $(T.varE (lookupName lkp inner)) . coerce |]

  Record rs -> tzr qual nm lkp rs
    where
      tzr | atLeastOne rule = terminalizeRecordAtLeastOne
          | otherwise = terminalizeRecordAllowsZero

  Opt (Rule inner _ _) ->
    [| maybe Seq.empty $(T.varE (lookupName lkp inner)) . coerce |]

  Star (Rule inner _ _) ->
    [| join . fmap $(T.varE (lookupName lkp inner)) . coerce |]

  Plus (Rule inner _ _) ->
    [| let getTermSeq = $(T.varE (lookupName lkp inner))
           getTerms (e1, es) = getTermSeq e1 `mappend` getTermSeq es
       in getTerms . coerce
    |]

terminalizeRecordAllowsZero
  :: Qualifier
  -> RuleName
  -> Map RuleName T.Name
  -> Seq (Rule t)
  -> T.Q T.Exp
terminalizeRecordAllowsZero qual nm lkp rs = do
  pairs <- traverse (terminalizeBranchRule lkp) . toList $ rs
  let pat = T.conP (quald qual nm) (fmap (fst . snd) pairs)
      expn = case fmap (snd . snd) pairs of
        [] -> [| Seq.empty |]
        x:xs -> foldl f x xs
          where
            f acc expn = [| $(acc) `mappend` $(expn) |]
  [| \ $(pat) -> $(expn) |]
    
terminalizeRecordAtLeastOne
  :: Qualifier
  -> RuleName
  -> Map RuleName T.Name
  -> Seq (Rule t)
  -> T.Q T.Exp
terminalizeRecordAtLeastOne qual name lkp bs = do
  pairs <- fmap toList . traverse (terminalizeBranchRule lkp) $ bs
  let pat = T.conP (quald qual name) (fmap (fst . snd) pairs)
      body = [| ( $(leadSeq) `NonEmpty.prependSeq` $(firstNonEmpty))
                `NonEmpty.appendSeq` $(trailSeq) |]
        where
          (leadRules, lastRules) = span (atLeastOne . fst) pairs
          (firstNonEmptyRule, trailRules) = case lastRules of
            [] -> error $ "terminalizeRecordAtLeastOne: failure 1: " ++ name
            x:xs -> (x, xs)
          leadSeq = case fmap (snd . snd) leadRules of
            [] -> [| Seq.empty |]
            x:xs -> foldl f x xs
              where
                f acc expn = [| $(acc) `mappend` $(expn) |]
          firstNonEmpty = [| $(snd . snd $ firstNonEmptyRule) |]

          trailSeq = foldl f [| Seq.empty |] trailRules
            where
              f acc (rule, (_, expn))
                | atLeastOne rule =
                    [| $(acc) `mappend` NonEmpty.flatten $(expn) |]
                | otherwise =
                    [| $(acc) `mappend` $(expn) |]
  [| \ $(pat) -> $(body) |]


terminalizeBranchAllowsZero
  :: Qualifier
  -> Map RuleName T.Name
  -> Branch t
  -> T.Q T.Match
terminalizeBranchAllowsZero qual lkp (Branch name bs) = do
  pairs <- fmap toList . traverse (terminalizeBranchRule lkp) $ bs
  let pat = T.conP (quald qual name) (fmap (fst . snd) pairs)
      body = case fmap (snd . snd) pairs of
        [] -> [| Seq.empty |]
        x:xs -> foldl f x xs
          where
            f acc expn = [| $(acc) `mappend` $(expn) |]
  T.match pat (T.normalB body) []

terminalizeBranchAtLeastOne
  :: Qualifier
  -> Map RuleName T.Name
  -> Branch t
  -> T.Q T.Match
terminalizeBranchAtLeastOne qual lkp (Branch name bs) = do
  pairs <- fmap toList . traverse (terminalizeBranchRule lkp) $ bs
  let pat = T.conP (quald qual name) (fmap (fst . snd) pairs)
      body = [| ( $(leadSeq) `NonEmpty.prependSeq` $(firstNonEmpty))
                `NonEmpty.appendSeq` $(trailSeq) |]
        where
          (leadRules, lastRules) = span (atLeastOne . fst) pairs
          (firstNonEmptyRule, trailRules) = case lastRules of
            [] -> error $ "terminalizeBranchAtLeastOne: failure 1: " ++ name
            x:xs -> (x, xs)
          leadSeq = case fmap (snd . snd) leadRules of
            [] -> [| Seq.empty |]
            x:xs -> foldl f x xs
              where
                f acc expn = [| $(acc) `mappend` $(expn) |]
          firstNonEmpty = [| $(snd . snd $ firstNonEmptyRule) |]

          trailSeq = foldl f [| Seq.empty |] trailRules
            where
              f acc (rule, (_, expn))
                | atLeastOne rule =
                    [| $(acc) `mappend` NonEmpty.flatten $(expn) |]
                | otherwise =
                    [| $(acc) `mappend` $(expn) |]
  T.match pat (T.normalB body) []

terminalizeBranchRule
  :: Map RuleName T.Name
  -> Rule t
  -> T.Q (Rule t, (T.Q T.Pat, T.Q T.Exp))
terminalizeBranchRule lkp r@(Rule nm _ _) = do
  x <- T.newName $ "terminalizeBranchRule'" ++ nm
  let getTerms = [| $(T.varE (lookupName lkp nm)) $(T.varE x) |]
  return (r, (T.varP x, getTerms))

-- | Examines a rule to determine whether when terminalizing it will
-- always return at least one terminal symbol.
atLeastOne
  :: Rule t
  -> Bool
  -- ^ True if the rule will always have at least one terminal
  -- symbol.
atLeastOne (Rule _ _ ty) = case ty of
  Terminal _ -> True
  NonTerminal b1 bs -> branchAtLeastOne b1 && all branchAtLeastOne bs
    where
      branchAtLeastOne (Branch _ rs) = any atLeastOne rs
  Terminals sq -> not . Seq.null $ sq
  Wrap r -> atLeastOne r
  Record rs -> any atLeastOne rs
  Opt _ -> False
  Star _ -> False
  Plus r -> atLeastOne r

