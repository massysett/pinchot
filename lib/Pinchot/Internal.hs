{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home, show-extensions #-}

-- | Pinchot internals.  Ordinarily the "Pinchot" module should have
-- everything you need.

-- TODO
-- Optics
-- Single Located type

module Pinchot.Internal where

import Pinchot.Intervals
import Pinchot.Internal.Types
import Pinchot.Internal.Rules
import Pinchot.Internal.State

import Control.Applicative ((<|>), liftA2)
import Control.Exception (Exception)
import qualified Control.Lens as Lens
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.State (State, runState)
import Data.Char (isUpper)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Sequence (Seq, ViewL(EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Language.Haskell.TH
  (ExpQ, ConQ, normalC, mkName, strictType, notStrict, newtypeD,
   cxt, conT, Name, dataD, appT, DecsQ, appE, Q, uInfixE,
   varE, varP, conE, Pat, Exp, recC, varStrictType, dyn)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as Syntax
import Text.Earley (satisfy, rule, symbol)
import qualified Text.Earley

-- | Runs a 'Pinchot' with a starting empty state.  Fails in the Q
-- monad if the grammar is bad.
goPinchot :: Pinchot t a -> Q (Names t, a)
goPinchot (Pinchot pinc) = case fst pair of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right g -> return (snd pair, g)
  where
    pair = runState (runExceptT pinc)
      (Names Set.empty Set.empty 0 M.empty)

thBranch :: Branch t -> ConQ
thBranch (Branch nm rules) = normalC name fields
  where
    name = mkName nm
    mkField (Rule n _ _) = strictType notStrict (conT (mkName n))
    fields = toList . fmap mkField $ rules

thUnionBranch
  :: RuleName
  -- ^ Parent rule name
  -> Rule t
  -- ^ Child rule
  -> ConQ
thUnionBranch parent (Rule child _ _) = normalC name fields
  where
    name = mkName (unionBranchName parent child)
    fields = [strictType notStrict (conT (mkName child))]

thRule
  :: Syntax.Lift t
  => Bool
  -- ^ If True, make lenses.
  -> Name
  -- ^ Name of terminal type
  -> Seq Name
  -- ^ What to derive
  -> Rule t
  -> TH.Q [TH.Dec]
thRule doLenses typeName derives (Rule nm _ ruleType) = do
  ty <- makeType typeName derives nm ruleType
  lenses <- if doLenses then ruleToOptics typeName nm ruleType
    else return []
  inst <- productionDecl nm typeName ruleType
  return (ty : inst ++ lenses)


thAllRules
  :: Syntax.Lift t
  => Bool
  -- ^ If True, make optics as well.
  -> Name
  -- ^ Terminal type constructor name
  -> Seq Name
  -- ^ What to derive
  -> Map Int (Rule t)
  -> DecsQ
thAllRules doOptics typeName derives
  = fmap join
  . sequence
  . fmap (thRule doOptics typeName derives)
  . fmap snd
  . M.toAscList

makeWrapped
  :: TH.Type
  -- ^ Name of wrapped type
  -> String
  -- ^ Name of wrapper type
  -> TH.Dec
makeWrapped wrappedType nm = TH.InstanceD [] typ decs
  where
    name = TH.mkName nm
    local = mkName "_x"
    typ = (TH.ConT ''Lens.Wrapped) `TH.AppT` (TH.ConT name)
    decs = [assocType, wrapper]
      where
        assocType = TH.TySynInstD ''Lens.Unwrapped
          (TH.TySynEqn [TH.ConT name] wrappedType)
        wrapper = TH.FunD 'Lens._Wrapped
          [TH.Clause [] (TH.NormalB body) []]
          where
            body = (TH.VarE 'Lens.iso)
              `TH.AppE` unwrap
              `TH.AppE` doWrap
              where
                unwrap = TH.LamE [lambPat] (TH.VarE local)
                  where
                    lambPat = TH.ConP name [TH.VarP local]
                doWrap = TH.LamE [lambPat] expn
                  where
                    expn = (TH.ConE name)
                      `TH.AppE` (TH.VarE local)
                    lambPat = TH.VarP local

-- | TH helper like 'dyn' but for patterns
dynP :: String -> TH.PatQ
dynP = TH.varP . TH.mkName

seqTermToOptics
  :: Syntax.Lift t
  => Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> Seq t
  -> TH.Q [TH.Dec]
seqTermToOptics termName nm sq = do
  e1 <- TH.sigD (TH.mkName ('_':nm)) (TH.conT ''Lens.Prism'
    `TH.appT` (TH.conT ''Seq `TH.appT` TH.conT termName)
    `TH.appT` TH.conT (TH.mkName nm))
  e2 <- TH.valD prismName (TH.normalB expn) []
  return [e1, e2]
  where
    prismName = TH.varP (TH.mkName ('_' : nm))
    fetchPat = TH.conP (TH.mkName nm) [TH.varP (TH.mkName "_x")]
    fetchName = TH.varE (TH.mkName "_x")
    ctor = TH.conE (TH.mkName nm)
    expn = [| let fetch $fetchPat = $fetchName
                  store _term
                    | $(liftSeq sq) == _term = Right ($ctor _term)
                    | otherwise = Left _term
              in Lens.prism fetch store
           |]

-- | Creates a prism for a terminal type.  Although a newtype wraps
-- each terminal, do not make a Wrapped or an Iso, because the
-- relationship between the outer type and the type that it wraps
-- typically is not isometric.  Thus, use a Prism instead, which
-- captures this relationship properly.
terminalToOptics
  :: Syntax.Lift t
  => Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> Intervals t
  -> TH.Q [TH.Dec]
terminalToOptics termName nm ivls = do
  e1 <- TH.sigD (TH.mkName ('_':nm)) (TH.conT ''Lens.Prism'
        `TH.appT` TH.conT termName
        `TH.appT` TH.conT (TH.mkName nm))
  e2 <- TH.valD prismName (TH.normalB expn) []
  return [e1, e2]
  where
    prismName = TH.varP (TH.mkName ('_' : nm))
    fetchPat = TH.conP (TH.mkName nm) [TH.varP (TH.mkName "_x")]
    fetchName = TH.varE (TH.mkName "_x")
    ctor = TH.conE (TH.mkName nm)
    expn = [| let fetch $fetchPat = $fetchName
                  store _term
                    | inIntervals ivls _term = Right ($ctor _term)
                    | otherwise = Left _term
              in Lens.prism fetch store
           |]
    

optionalToOptics
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> TH.Dec
optionalToOptics wrappedName = makeWrapped maybeName
  where
    maybeName = (TH.ConT ''Maybe) `TH.AppT` (TH.ConT (TH.mkName wrappedName))

many1ToOptics
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> TH.Dec
many1ToOptics wrappedName = makeWrapped tupName
  where
    tupName = (TH.TupleT 2)
      `TH.AppT` (TH.ConT (TH.mkName wrappedName))
      `TH.AppT` ((TH.ConT ''Seq) `TH.AppT` (TH.ConT (TH.mkName wrappedName)))

manyToOptics
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> TH.Dec
manyToOptics wrappedName = makeWrapped innerName
  where
    innerName = (TH.ConT ''Seq) `TH.AppT` (TH.ConT (TH.mkName wrappedName))

wrapToOptics
  :: String
  -- ^ Wrapped rule name
  -> String
  -- ^ Wrapping Rule name
  -> TH.Dec
wrapToOptics wrappedName = makeWrapped innerName
  where
    innerName = TH.ConT (TH.mkName wrappedName)

terminalSeqToOptics
  :: Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> TH.Dec
terminalSeqToOptics terminalName = makeWrapped sqType
  where
    sqType = (TH.ConT ''Seq) `TH.AppT` (TH.ConT terminalName)

branchesToOptics
  :: String
  -- ^ Rule name
  -> Branch t
  -> Seq (Branch t)
  -> [TH.Dec]
branchesToOptics nm b1 bsSeq = concat $ makePrism b1 : fmap makePrism bs
  where
    bs = toList bsSeq
    makePrism (Branch inner rulesSeq) = [ signature, binding ]
      where
        rules = toList rulesSeq
        prismName = TH.mkName ('_' : inner)
        signature = TH.SigD prismName
          $ (TH.ConT ''Lens.Prism')
          `TH.AppT` (TH.ConT (TH.mkName nm))
          `TH.AppT` fieldsType
          where
            fieldsType = case rules of
              [] -> TH.TupleT 0
              Rule r1 _ _ : [] -> TH.ConT (TH.mkName r1)
              rs -> foldl addType (TH.TupleT (length rs)) rs
                where
                  addType soFar (Rule r _ _) = soFar `TH.AppT`
                    (TH.ConT (TH.mkName r))
        binding = TH.ValD (TH.VarP prismName) body []
          where
            body = TH.NormalB
              $ (TH.VarE 'Lens.prism)
              `TH.AppE` setter
              `TH.AppE` getter
              where
                setter = TH.LamE [pat] expn
                  where
                    (pat, expn) = case rules of
                      [] -> (TH.TupP [], TH.ConE (TH.mkName inner))
                      _ : [] -> (TH.VarP local,
                        TH.ConE (TH.mkName inner)
                        `TH.AppE` TH.VarE local)
                        where
                          local = TH.mkName "_x"
                      ls -> (TH.TupP pats, set)
                        where
                          pats = fmap (\i -> TH.VarP (mkName ("_x" ++ show i)))
                            . take (length ls) $ [(0 :: Int) ..]
                          set = foldl addVar start . take (length ls)
                            $ [(0 :: Int) ..]
                            where
                              addVar acc i = acc `TH.AppE`
                                (TH.VarE (TH.mkName ("_x" ++ show i)))
                              start = TH.ConE (TH.mkName inner)

                getter = TH.LamE [pat] expn
                  where
                    local = TH.mkName "_x"
                    pat = TH.VarP local
                    expn = TH.CaseE (TH.VarE (TH.mkName "_x")) $
                      TH.Match patCtor bodyCtor []
                      : rest
                      where
                        patCtor = TH.ConP (TH.mkName inner)
                          . fmap (\i -> TH.VarP (TH.mkName $ "_y" ++ show i))
                          . take (length rules)
                          $ [(0 :: Int) ..]
                        bodyCtor = TH.NormalB . (TH.ConE 'Right `TH.AppE`)
                          $ case rules of
                          [] -> TH.TupE []
                          _:[] -> TH.VarE (TH.mkName "_y0")
                          _ -> TH.TupE
                            . fmap (\i -> TH.VarE (TH.mkName $ "_y" ++ show i))
                            . take (length rules)
                            $ [(0 :: Int) ..]
                        rest = case bs of
                          [] -> []
                          _ -> [TH.Match patBlank bodyBlank []]
                          where
                            patBlank = TH.VarP (TH.mkName "_z")
                            bodyBlank = TH.NormalB
                              $ TH.ConE ('Left)
                              `TH.AppE` TH.VarE (TH.mkName "_z")

unionToOptics
  :: String
  -- ^ Rule name
  -> Rule t
  -- ^ First rule
  -> Seq (Rule t)
  -- ^ Remaining rules
  -> TH.DecsQ
unionToOptics parentName r1 rs
  = fmap concat . sequence $ optics r1 : fmap optics (toList rs)
  where
    optics (Rule r _ _) = sequence $ sig : prism : []
      where
        sig = TH.sigD prismName [t| Lens.Prism' $bigType $innerType |]
        prismName = TH.mkName $ "_" ++ parentName ++ "'" ++ r
        bigType = TH.conT (TH.mkName parentName)
        innerType = TH.conT (TH.mkName r)
        prism = TH.valD (TH.varP prismName)
          (TH.normalB [| Lens.prism $dataCtor $sToA |] ) []
        sToA = TH.lamE [pat] expn
          where
            pat = dynP "_x"
            expn = TH.caseE (dyn "_x")
              [ TH.match (TH.conP (TH.mkName (unionBranchName parentName r))
                           [TH.varP (TH.mkName "_a")])
                         (TH.normalB [| Right $(dyn "_a") |]) []
              , TH.match (dynP "_b")
                         (TH.normalB [| Left $(dyn "_b") |]) []
              ]
        dataCtor = TH.conE (TH.mkName (unionBranchName parentName r))

recordsToOptics
  :: String
  -- ^ Rule name
  -> Seq (Rule t)
  -> [TH.Dec]
recordsToOptics nm
  = concat . zipWith makeLens [(0 :: Int) ..] . toList
  where
    makeLens index (Rule inner _ _) = [ signature, function ]
      where
        fieldNm = fieldName index nm inner
        lensName = mkName fieldNm
        signature = TH.SigD lensName
          $ (TH.ConT ''Lens.Lens')
          `TH.AppT` (TH.ConT (TH.mkName nm))
          `TH.AppT` (TH.ConT (TH.mkName inner))

        function = TH.FunD lensName [TH.Clause [] (TH.NormalB body) []]
          where
            namedRec = TH.mkName "_namedRec"
            namedNewVal = TH.mkName "_namedNewVal"
            body = (TH.VarE 'Lens.lens) `TH.AppE` getter `TH.AppE` setter
              where
                getter = TH.LamE [pat] expn
                  where
                    pat = TH.VarP namedRec
                    expn = (TH.VarE (TH.mkName ('_' : fieldNm)))
                      `TH.AppE` (TH.VarE namedRec)

                setter = TH.LamE [patRec, patNewVal] expn
                  where
                    patRec = TH.VarP namedRec
                    patNewVal = TH.VarP namedNewVal
                    expn = TH.RecUpdE (TH.VarE namedRec)
                      [ (TH.mkName ('_' : fieldNm), TH.VarE namedNewVal) ]


ruleToOptics
  :: Syntax.Lift t
  => Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> RuleType t
  -> TH.DecsQ
ruleToOptics terminalName nm ty = case ty of
  RTerminal ivl -> terminalToOptics terminalName nm ivl
  RBranch (b1, bs) -> return $ branchesToOptics nm b1 bs
  RUnion (r1, rs) -> unionToOptics nm r1 rs
  RSeqTerm sq -> seqTermToOptics terminalName nm sq
  ROptional (Rule inner _ _) -> return [optionalToOptics inner nm]
  RList (Rule inner _ _) -> return [manyToOptics inner nm]
  RList1 (Rule inner _ _) -> return [many1ToOptics inner nm]
  RWrap (Rule inner _ _) -> return [wrapToOptics inner nm]
  RRecord recs -> return $ recordsToOptics nm recs

-- | Creates optics.
--
-- If you use this option, you will need
-- @
-- \{\-\# LANGUAGE TypeFamilies \#\-\}
-- @
--
-- at the top of the module into which you splice in the
-- declarations, because you will get instances of 'Lens.Wrapped'.
--
-- Creates the listed optics for each kind of
-- 'Rule', as follows:
--
-- * 'terminal': @'Lens.Prism'' a b@, where @a@ is the type of the
-- terminal token (often 'Char') and @b@ is the type of this
-- particular production.  For an example, see
-- 'Pinchot.Examples.PostalAstAllRules._Comma'.
--
-- >>> ',' ^? _Comma
-- Just (Comma ',')
-- >>> 'a' ^? _Comma
-- Nothing
-- >>> Comma ',' ^. re _Comma
-- ','
--
-- Thus this gives you a safe way to insert tokens into types made
-- with 'terminal' (useful if you want to construct a syntax tree.)
--
-- * 'terminalSeq': @'Lens.Prism'' ('Seq' a) b@, where @a@ is the type
-- of the terminal token (often 'Char') and @b@ is the type of this
-- particular production.  As with 'terminal' this gives you a safe
-- way to insert values into the types made with 'terminalSeq'.
--
-- * 'nonTerminal': one 'Lens.Prism'' for each data constructor (even if
-- there is only one data constructor)
--
-- * 'union': one 'Lens.Prism' for each data constructor (even if
-- there is only one data constructor)
--
-- * 'record': one 'Lens.Lens' for each field
--
-- * 'list': 'Lens.Wrapped', wrapping a @'Seq' a@
--
-- * 'list1': 'Lens.Wrapped', wrapping a pair @(a, 'Seq' a)@
--
-- * 'option': 'Lens.Wrapped', wrapping a @'Maybe' a@
--
-- * 'wrap': 'Lens.Wrapped', wrapping the underlying type
makeOptics :: MakeOptics
makeOptics = True

-- | Do not make any optics.
noOptics :: MakeOptics
noOptics = False

-- | Creates data types for every 'Rule' created in the 'Pinchot'.  The data
-- types are created in the same order in which they were created in
-- the 'Pinchot'.  When spliced, the 'DecsQ' is a list of
-- declarations, each of which is an appropriate @data@ or @newtype@.
-- For an example use of 'allRulesToTypes', see
-- "Pinchot.Examples.PostalAstAllRules".
--
-- Also creates bindings whose names are prefixed with @t'@.  Each
-- of these is a function that, when given a particular production,
-- reduces it to a sequence of terminal symbols.

allRulesToTypes
  :: Syntax.Lift t
  => MakeOptics

  -> Name
  -- ^ Terminal type constructor name.  Typically you will use the
  -- Template Haskell quoting mechanism to get this.

  -> Seq Name
  -- ^ What to derive.  For instance, you might use @Eq@, @Ord@, and
  -- @Show@ here.  Each created data type will derive these instances.

  -> Pinchot t a
  -- ^ The return value from the 'Pinchot' is ignored.

  -> DecsQ
allRulesToTypes doOptics typeName derives pinchot = do
  st' <- fmap fst $ goPinchot pinchot
  thAllRules doOptics typeName derives (allRules st')

-- | Creates data types only for the 'Rule' returned from the 'Pinchot', and
-- for its ancestors.
--
-- Also creates bindings whose names are prefixed with @t'@.  Each
-- of these is a function that, when given a particular production,
-- reduces it to a sequence of terminal symbols.
ruleTreeToTypes
  :: Syntax.Lift t
  => MakeOptics

  -> Name
  -- ^ Terminal type constructor name.  Typically you will use the
  -- Template Haskell quoting mechanism to get this.

  -> Seq Name
  -- ^ What to derive.  For instance, you might use @Eq@, @Ord@, and
  -- @Show@ here.  Each created data type will derive these instances.

  -> Pinchot t (Rule t)
  -- ^ A data type is created for the 'Rule' that the 'Pinchot'
  -- returns, and for the ancestors of the 'Rule'.
  -> DecsQ
ruleTreeToTypes doOptics typeName derives pinchot = do
  r <- fmap snd $ goPinchot pinchot
  fmap join . sequence . toList
    . fmap (thRule doOptics typeName derives)
    . runCalc . getAncestors $ r
  where
    runCalc stateCalc = fst $ runState stateCalc Set.empty

addPrefix
  :: String
  -> String
  -> String
addPrefix pfx suf
  | null pfx = suf
  | otherwise = pfx ++ '.':suf

ruleToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Rule t
  -> [(Name, ExpQ)]
ruleToParser prefix (Rule nm mayDescription rt) = case rt of

  RTerminal ivls -> [makeRule expression]
    where
      expression = [| fmap $constructor (satisfy (inIntervals ivls)) |]

  RBranch (b1, bs) -> [makeRule expression]
    where
      expression = foldl addBranch (branchToParser prefix b1) bs
        where
          addBranch tree branch =
            [| $tree <|> $(branchToParser prefix branch) |]

  RUnion (Rule r1 _ _, rs) -> [makeRule expression]
    where
      expression = foldl adder start rs
        where
          branch r = [| $(conE (mkName
            (addPrefix prefix . unionBranchName nm $ r))) <$>
            $(varE (ruleName r)) |]
          start = branch r1
          adder soFar (Rule r _ _) = [| $soFar <|> $(branch r) |]

  RSeqTerm sq -> [nestRule, topRule]
    where
      nestRule = (helper, [| rule $(foldl addTerm start sq) |])
        where
          start = [|pure Seq.empty|]
          addTerm acc x = [| liftA2 (<|) (symbol x) $acc |]
      topRule = makeRule (wrapper helper)

  ROptional (Rule innerNm _ _) -> [makeRule expression]
    where
      expression = [| fmap $constructor (pure Nothing <|> $(just)) |]
        where
          just = [| fmap Just $(varE (ruleName innerNm)) |]

  RList (Rule innerNm _ _) -> [nestRule, makeRule (wrapper helper)]
    where
      nestRule = (helper, ([|rule|] `appE` parseSeq))
        where
          parseSeq = uInfixE [|pure Seq.empty|] [|(<|>)|] pSeq
            where
              pSeq = [|liftA2 (<|) $(varE (ruleName innerNm)) $(varE helper) |]

  RList1 (Rule innerNm _ _) -> [nestRule, makeRule topExpn]
    where
      nestRule = (helper, [|rule $(parseSeq)|])
        where
          parseSeq = [| pure Seq.empty <|> $pSeq |]
            where
              pSeq = [| (<|) <$> $(varE (ruleName innerNm))
                             <*> $(varE helper) |]
      topExpn = [| $constructor <$> ( (,) <$> $(varE (ruleName innerNm))
                                        <*> $(varE helper)
                                    ) |]

  RWrap (Rule innerNm _ _) -> [makeRule expression]
    where
      expression = [|fmap $constructor $(varE (ruleName innerNm)) |]

  RRecord sq -> [makeRule expression]
    where
      expression = case viewl sq of
        EmptyL -> [| pure $constructor |]
        Rule r1 _ _ :< restFields -> foldl addField fstField restFields
          where
            fstField = [| $constructor <$> $(varE (ruleName r1)) |]
            addField soFar (Rule r _ _)
              = [| $soFar <*> $(varE (ruleName r)) |]
    

  where
    makeRule expression = (ruleName nm,
      [|rule ($expression Text.Earley.<?> $(textToExp desc))|])
    desc = maybe nm id mayDescription
    textToExp txt = [| $(Syntax.lift txt) |]
    constructor = constructorName prefix nm
    wrapper wrapRule = [|fmap $constructor $(varE wrapRule) |]
    helper = helperName nm


constructorName
  :: String
  -- ^ Module prefix
  -> String
  -- ^ Name of constructor
  -> ExpQ
constructorName pfx nm = conE (mkName name)
  where
    name = pfx' ++ nm
    pfx'
      | null pfx = ""
      | otherwise = pfx ++ "."

ruleName :: String -> Name
ruleName suffix = mkName ("_rule'" ++ suffix)

helperName :: String -> Name
helperName suffix = mkName ("_helper'" ++ suffix)

branchToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Branch t
  -> ExpQ
branchToParser prefix (Branch name rules) = case viewl rules of
  EmptyL -> [| pure $constructor |]
  (Rule rule1 _ _) :< xs -> foldl f z xs
    where
      z = [| $constructor <$> $(varE (ruleName rule1)) |]
      f soFar (Rule rule2 _ _) = [| $soFar <*> $(varE (ruleName rule2)) |]
  where
    constructor = constructorName prefix name

-- # lazyPattern and bigTuple - because TH has no support for
-- mdo notation
    
-- | Creates a lazy pattern for all the given names.  Adds an empty
-- pattern onto the front.  This is the counterpart of 'bigTuple'.
-- All of the given names are bound.  In addition, a single,
-- wildcard pattern is bound to the front.
-- 
-- For example, @lazyPattern (map mkName ["x", "y", "z"])@ gives a
-- pattern that looks like
--
-- @~(_, (x, (y, (z, ()))))@
--
-- The idea is that the named patterns are needed so that the
-- recursive @do@ notation works, and that the wildcard pattern is
-- the return value, which is not needed here.
lazyPattern
  :: Foldable c
  => c Name
  -> Q Pat
lazyPattern = finish . foldr gen [p| () |]
  where
    gen name rest = [p| ($(varP name), $rest) |]
    finish pat = [p| ~(_, $pat) |]

-- | Creates a big tuple.  It is nested in the second element, such
-- as (1, (2, (3, (4, ())))).  Thus, the big tuple is terminated
-- with a unit value.  It resembles a list where each tuple is a
-- cons cell and the terminator is unit.
bigTuple
  :: Foldable c
  => ExpQ
  -- ^ This expression will be the first one in the tuple.
  -> c ExpQ
  -- ^ Remaining expressions in the tuple.
  -> ExpQ
bigTuple top = finish . foldr f [| () |]
  where
    f n rest = [| ( $(n), $rest) |]
    finish tup = [| ($(top), $tup) |]

-- | Creates an Earley grammar for a given 'Rule'.  For examples of how
-- to use this, see the source code for
-- "Pinchot.Examples.PostalAstRuleTree" and for
-- "Pinchot.Examples.PostalAstAllRules".

earleyGrammar
  :: Syntax.Lift t

  => Qualifier
  -- ^ Qualifier for data types crated with 'ruleTreeToTypes' or
  -- 'allRulesToTypes'

  -> Pinchot t (Rule t)
  -- ^ Creates an Earley parser for the 'Rule' that the 'Pinchot'
  -- returns.

  -> Q Exp
  -- ^ When spliced, this expression has type
  -- @'Text.Earley.Grammar' r ('Text.Earley.Prod' r 'String' t a)@
  --
  -- where
  -- 
  -- @r@ is left universally quantified
  --
  -- @t@ is the type of the token (usually 'Char')
  --
  -- @a@ is the type defined by the 'Rule'.
earleyGrammar prefix pinc = do
  r <- fmap snd $ goPinchot pinc
  earleyGrammarFromRule prefix r

-- | Builds a recursive @do@ expression (because TH has no support
-- for @mdo@ notation).
recursiveDo
  :: [(Name, ExpQ)]
  -- ^ Binding statements
  -> ExpQ
  -- ^ Final return value from @do@ block.  The type of this 'ExpQ'
  -- must be in the same monad as the @do@ block; it must not be a
  -- pure value.
  -> ExpQ
  -- ^ Returns an expression whose value is the final return value
  -- from the @do@ block.
recursiveDo binds final = [| fmap fst $ mfix $(fn) |]
  where
    fn = [| \ $(lazyPattern (fmap fst binds)) -> $doBlock |]
    doBlock = TH.doE (bindStmts ++ returnStmts)
    bindStmts = map mkBind binds
      where
        mkBind (name, exp)
          = TH.bindS (TH.varP name) exp
    returnStmts = [bindRtnVal, returner]
      where
        rtnValName = TH.mkName "_returner"
        bindRtnVal = TH.bindS (TH.varP rtnValName) final
        returner
          = TH.noBindS
            [| return $(bigTuple (TH.varE rtnValName) 
                                 (fmap (TH.varE . fst) binds)) |]

earleyGrammarFromRule
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Rule t
  -> Q Exp
earleyGrammarFromRule prefix r@(Rule top _ _) = recursiveDo binds final
  where
    binds = concatMap (ruleToParser prefix) . toList . ruleAndAncestors $ r
    final = [| return $(TH.varE $ ruleName top) |]

-- | Creates an Earley grammar for each 'Rule' created in a
-- 'Pinchot'.  For a 'Pinchot' with a large number of 'Rule's, this
-- can create a large number of declarations that can take a long
-- time to compile--sometimes several minutes.  For lower
-- compilation times, try 'earleyProduct'.

allEarleyGrammars
  :: Syntax.Lift t

  => Qualifier
  -- ^ Qualifier for data types created with 'ruleTreeToTypes' or
  -- 'allRulesToTypes'

  -> Name
  -- ^ Name for the terminal type; often this is 'Char'.  Typically
  -- you will use the Template Haskell quoting mechanism--for
  -- example, @\'\'Char@.

  -> Pinchot t a
  -- ^ Creates an Earley grammar for each 'Rule' created in the
  -- 'Pinchot'.  The return value of the 'Pinchot' computation is
  -- ignored.

  -> DecsQ
  -- ^ When spliced, this is a list of declarations.  Each
  -- declaration has type
  -- @'Text.Earley.Grammar' r ('Text.Earley.Prod' r 'String' t a)@
  --
  -- where
  -- 
  -- @r@ is left universally quantified
  --
  -- @t@ is the type of the token (usually 'Char')
  --
  -- @a@ is the type defined by the 'Rule'.
  --
  -- The name of each declaration is
  -- g'TYPE_NAME
  --
  -- where TYPE_NAME is the name of the type defined in the
  -- corresponding 'Rule'.
allEarleyGrammars prefix termName pinc = do
  st <- fmap fst $ goPinchot pinc
  sequence . concat . fmap makeDecl . fmap snd . M.toList
    . allRules $ st
  where
    makeDecl rule@(Rule nm _ _) = [signature, TH.valD pat body []]
      where
        signature = TH.sigD name types
        r = TH.mkName "r"
        types = TH.forallT [TH.PlainTV r] (return [])
          $ [t| Text.Earley.Grammar $(TH.varT r)
              (Text.Earley.Prod $(TH.varT r) String $(TH.conT termName)
                                         $(TH.conT qualRuleName)) |]
        name = TH.mkName $ "g'" ++ nm
        pat = TH.varP name
        body = TH.normalB (earleyGrammarFromRule prefix rule)
        qualRuleName
          | null prefix = TH.mkName nm
          | otherwise = TH.mkName (prefix ++ "." ++ nm)


prodDeclName :: String -> TH.Name
prodDeclName name = TH.mkName $ "t'" ++ name

prodFn :: String -> TH.ExpQ
prodFn = TH.varE . prodDeclName

addIndices :: Foldable c => c a -> [(Int, a)]
addIndices = zip [0..] . toList

-- | Creates a production declaration for a 'Rule'.
productionDecl
  :: String
  -- ^ Rule name
  -> Name
  -- ^ Name of terminal type
  -> RuleType t
  -> TH.DecsQ
productionDecl n termType t
  = sequence [signature, TH.funD (prodDeclName n) clauses]
  where
    signature = TH.sigD (prodDeclName n) types
      where
        types = TH.appT (TH.appT TH.arrowT (TH.conT (TH.mkName n)))
          (TH.appT (TH.conT ''Seq) (TH.conT termType))
    clauses = case t of
      RTerminal _ -> [TH.clause [pat] bdy []]
        where
          pat = TH.conP (TH.mkName n) [TH.varP (TH.mkName "_x")]
          bdy = TH.normalB [| Seq.singleton $(TH.varE (TH.mkName "_x")) |]

      RBranch (b1, bs) -> branchToClause b1
        : fmap branchToClause (toList bs)

      RSeqTerm _ -> [TH.clause [pat] bdy []]
        where
          pat = TH.conP (TH.mkName n) [TH.varP (TH.mkName "_x")]
          bdy = TH.normalB (dyn "_x")

      ROptional (Rule inner _ _) -> [justClause, nothingClause]
        where
          justClause
            = TH.clause [TH.conP (TH.mkName n)
                [TH.conP 'Just [TH.varP (TH.mkName "_b")]]]
                (TH.normalB [| $(prodFn inner)
                               $(TH.varE (TH.mkName "_b")) |])
                []
          nothingClause
            = TH.clause [TH.conP (TH.mkName n)
                [TH.conP 'Nothing []]]
                (TH.normalB [| Seq.empty |]) []

      RList (Rule inner _ _) -> [TH.clause [pat] bdy []]
        where
          pat = TH.conP (TH.mkName n) [TH.varP (TH.mkName "_a")] 
          bdy = TH.normalB [| join
            $ fmap $(prodFn inner) $(TH.varE (TH.mkName "_a")) |]

      RList1 (Rule inner _ _) -> [TH.clause [pat] bdy []]
        where
          pat = TH.conP (TH.mkName n)
            [TH.tupP [ dynP "_x1", dynP "_xs" ]]
          bdy = TH.normalB
            [| $lft `mappend` (join (fmap $(prodFn inner)
                $(dyn "_xs"))) |]
            where
              lft = [| $(prodFn inner) $(dyn "_x1") |]

      RWrap (Rule inner _ _) -> [TH.clause [pat] bdy []]
        where
          pat = TH.conP (TH.mkName n) [dynP "_x"]
          bdy = TH.normalB [| $(prodFn inner) $(dyn "_x") |]

      RRecord sq -> [TH.clause [pat] (TH.normalB bdy) []]
        where
          pat = TH.conP (TH.mkName n) . fmap mkPat
            . fmap fst . addIndices $ sq
            where
              mkPat idx = dynP ("_x'" ++ show idx)
          bdy = foldr addField [| Seq.empty |] . addIndices $ sq
            where
              addField (idx, (Rule nm _ _)) acc = [| $this `mappend` $acc |]
                where
                  this = [| $(prodFn nm) $(dyn ("_x'" ++ show idx)) |]

      RUnion (r1, rs) -> mkClause r1 : fmap mkClause (toList rs)
        where
          mkClause (Rule inner _ _) = TH.clause [pat] bdy []
            where
              pat = TH.conP (TH.mkName (unionBranchName n inner))
                [dynP "_x"]
              bdy = TH.normalB [| $(prodFn inner) $(dyn "_x") |]


branchToClause :: Branch t -> TH.ClauseQ
branchToClause (Branch n rs) = TH.clause [pat] bdy []
  where
    pat = TH.conP (TH.mkName n) fields
      where
        fields = fmap mkField . fmap fst . addIndices $ rs
          where
            mkField idx = TH.varP (TH.mkName ("_x'" ++ show idx))
    bdy = TH.normalB [| join $sq |]
      where
        sq = foldr addField (TH.varE 'Seq.empty) . addIndices $ rs
          where
            addField (idx, (Rule inner _ _)) acc = [| $newTerm <| $acc |]
              where
                newTerm = [| $(prodFn inner) $(TH.varE
                              (TH.mkName ("_x'" ++ show idx))) |]

-- | Many functions take an argument that holds the name qualifier
-- for the module that contains the data types created by applying
-- 'ruleTreeToTypes' or 'allRulesToTypes' to the 'Pinchot.'
--
-- You have to make sure that the data types you created with
-- 'ruleTreeToTypes', 'allRulesToTypes', or 'allRulesRecord' are in
-- scope.  The spliced Template Haskell code has to know where to
-- look for these data types.  If you did an unqualified @import@ or
-- if the types are in the same module as is the splice of
-- 'earleyParser', just pass the empty string here.  If you did a
-- qualified import, use the appropriate qualifier here.
--
-- For example, if you used @import qualified MyAst@, pass
-- @\"MyAst\"@ here.  If you used @import qualified
-- Data.MyLibrary.MyAst as MyLibrary.MyAst@, pass
-- @\"MyLibrary.MyAst\"@ here.
--
-- I recommend that you always create a new module and that all you
-- do in that module is apply 'ruleTreeToTypes' or
-- 'allRulesToTypes', and that you then perform an @import
-- qualified@ to bring those names into scope in the module in which
-- you use a function that takes a 'Qualifier' argument.  This
-- avoids unlikely, but possible, issues that could otherwise arise
-- due to naming conflicts.
type Qualifier = String


-- | Creates a record data type that holds a value of type
--
-- @'Text.Earley.Prod' r 'String' t a@
--
-- for every rule created in the 'Pinchot'.  @r@ is left
-- universally quantified, @t@ is the token type (typically 'Char')
-- and @a@ is the type of the rule.
--
-- This always creates a single product type whose name is
-- @Productions@; currently the name cannot be configured.
--
-- For an example of the use of 'allRulesRecord', please see
-- "Pinchot.Examples.AllRulesRecord".
allRulesRecord
  :: Qualifier
  -- ^ Qualifier for data types created with 'ruleTreeToTypes' or
  -- 'allRulesToTypes'
  -> Name
  -- ^ Name of terminal type.  Typically you will get this through
  -- the Template Haskell quoting mechanism, such as @''Char@.
  -> Pinchot t a
  -- ^ A record is created that holds a value for each 'Rule'
  -- created in the 'Pinchot'; the return value of the 'Pinchot' is
  -- ignored.
  -> DecsQ
  -- ^ When spliced, this will create a single declaration that is a
  -- record with the name @Productions@.  It will have one type variable,
  -- @r@.  Each record in the declaration will have a name like so:
  --
  -- @a'NAME@
  --
  -- where @NAME@ is the name of the type.  Don't count on these
  -- records being in any particular order.
allRulesRecord prefix termName pinc
  = sequence [TH.dataD (return []) (TH.mkName nameStr) tys [con] []]
  where
    nameStr = "Productions"
    tys = [TH.PlainTV (TH.mkName "r")]
    con = do
      names <- fmap fst $ goPinchot pinc
      TH.recC (TH.mkName nameStr)
        (fmap (mkRecord . snd) . M.assocs . allRules $ names)
    mkRecord (Rule ruleNm _ _) = TH.varStrictType recName st
      where
        recName = TH.mkName ("a'" ++ ruleNm)
        st = TH.strictType TH.notStrict ty
          where
            ty = (TH.conT ''Text.Earley.Prod)
              `TH.appT` (TH.varT (TH.mkName "r"))
              `TH.appT` (TH.conT ''String)
              `TH.appT` (TH.conT termName)
              `TH.appT` (TH.conT (TH.mkName nameWithPrefix))
            nameWithPrefix = case prefix of
              [] -> ruleNm
              _ -> prefix ++ '.' : ruleNm

-- | Creates a 'Text.Earley.Grammar' that contains a
-- 'Text.Earley.Prod' for every 'Rule' created in the 'Pinchot'.
earleyProduct
  :: Syntax.Lift t

  => Qualifier
  -- ^ Qualifier for data types created with 'ruleTreeToTypes' or
  -- 'allRulesToTypes'

  -> Qualifier
  -- ^ Module prefix for the type created with 'allRulesRecord'

  -> Pinchot t a
  -- ^ Creates an Earley grammar that contains a 'Text.Earley.Prod'
  -- for each 'Rule' in the 'Pinchot'.  The return value from the
  -- 'Pinchot' is ignored.

  -> ExpQ
  -- ^ When spliced, 'earleyProduct' creates an expression whose
  -- type is @'Text.Earley.Grammar' r (Productions r)@, where
  -- @Productions@ is
  -- the type created by 'allRulesRecord'.
earleyProduct pfxRule pfxRec pinc = do
  names <- fmap fst $ goPinchot pinc
  let binds = concatMap (ruleToParser pfxRule)
        . fmap snd . M.assocs . allRules $ names
      final = [| return
        $(TH.recConE (TH.mkName rulesRecName) (recs names)) |]
  recursiveDo binds final
  where
    rulesRecName
      | null pfxRec = "Productions"
      | otherwise = pfxRec ++ ".Productions"
    recs = fmap mkRec . fmap snd . M.assocs . allRules
      where
        mkRec (Rule n _ _) = return (TH.mkName recName, recVal)
          where
            recName
              | null pfxRec = "a'" ++ n
              | otherwise = pfxRec ++ ".a'" ++ n
            recVal = TH.VarE . ruleName $ n

-- Locater

-- | A location.
data Loc = Loc
  { _line :: Int
  , _col :: Int
  , _pos :: Int
  } deriving (Eq, Ord, Read, Show)

Lens.makeLenses ''Loc

type Locator = State Loc

-- | Runs a 'Locator' computation.
-- Starts out with the line, column, and position all set to 1.
locate
  :: (a -> Locator b)
  -> a
  -> b
locate k a = State.evalState (k a) (Loc 1 1 1)

-- | Creates a data type for a 'Rule' that includes location
-- information.
locationData
  :: Name
  -- ^ Type name of terminal type
  -> [Name]
  -- ^ Derive these types
  -> Rule t
  -> TH.Q TH.Dec
locationData typeName derives (Rule name _ ty) = case ty of
  RTerminal _ -> dataD (return []) (mkName name) []
    [TH.normalC (mkName name)
                [ TH.strictType TH.notStrict (TH.conT ''Loc)
                , TH.strictType TH.notStrict (TH.conT typeName)
                ]]
    derives

  RBranch (b1, bs) -> TH.dataD (return []) (mkName name) []
    (mkCon b1 : toList (fmap mkCon bs)) derives
    where
      mkCon (Branch branchName rules) = TH.normalC (TH.mkName branchName)
        (ctor1 : toList (fmap mkField rules))
        where
          ctor1 = TH.strictType TH.notStrict (TH.conT ''Loc)
          mkField (Rule fieldName _ _) = TH.strictType TH.notStrict
            (TH.conT (TH.mkName fieldName))

  RUnion (r1, rs) -> TH.dataD (return []) (mkName name) []
    (mkCon r1 : toList (fmap mkCon rs)) derives
    where
      mkCon (Rule branchName _ _) = TH.normalC (TH.mkName
        (unionBranchName name branchName))
        [ TH.strictType TH.notStrict (TH.conT ''Loc)
        , (TH.strictType TH.notStrict (TH.conT (TH.mkName branchName)))
        ]

  RSeqTerm _ -> newtypeD (return []) (mkName name) []
    (TH.normalC (mkName name)
                [TH.strictType TH.notStrict
                  [t| Seq (Loc, $(TH.conT typeName)) |]])
    derives

  ROptional (Rule opt _ _) -> TH.dataD (return []) (mkName name) []
    [ TH.normalC (TH.mkName name)
      [ TH.strictType TH.notStrict (TH.conT ''Loc)
      , TH.strictType TH.notStrict [t| Maybe $(TH.conT (mkName opt)) |]
      ]
    ]
    derives

  RList (Rule subName _ _) -> TH.dataD (return []) (mkName name) []
    [TH.normalC (mkName name)
      [ TH.strictType TH.notStrict (TH.conT ''Loc)
      
      , TH.strictType TH.notStrict
          [t| Seq (Loc, $(TH.conT (mkName subName))) |]
      ]
    ] derives

  RList1 (Rule subName _ _) -> TH.dataD (return []) (mkName name) []
    [ TH.normalC (TH.mkName name)
        [ TH.strictType TH.notStrict (TH.conT ''Loc)
        , TH.strictType TH.notStrict
            [t| ( $(TH.conT (mkName subName))
                , Seq (Loc, $(TH.conT (mkName subName)))) |]
        ]
    ] derives

  RWrap (Rule subName _ _) -> TH.newtypeD (return []) (mkName name) []
    ( TH.normalC (TH.mkName name)
        [TH.strictType TH.notStrict (TH.conT (mkName subName))])
    derives

  RRecord rs -> dataD (cxt []) (mkName name) [] [ctor] derives
    where
      ctor = recC (mkName name) . zipWith mkField [(0 :: Int) ..]
        . toList $ rs
      mkField num (Rule rn _ _) = varStrictType (mkName fldNm)
        (strictType notStrict [t| (Loc, $(TH.conT (mkName rn))) |])
        where
          fldNm = '_' : fieldName num name rn

-- | Create types that include location information, for the given
-- 'Rule' and all its ancestors. TODO implement optics.
ruleTreeToLocatedTypes
  :: MakeOptics
  -> Name
  -- ^ Type name of terminal type
  -> Seq Name
  -- ^ Derive these types
  -> Pinchot t (Rule t)
  -> DecsQ
ruleTreeToLocatedTypes _ termType derives pinc = do
  (_, rule) <- goPinchot pinc
  let rs = ruleAndAncestors rule
  fmap toList . traverse (locationData termType (toList derives))
    $ rs

-- | Create types that include location information for every 'Rule'
-- created in the 'Pinchot'.  TODO implement optics.
allRulesToLocatedTypes
  :: MakeOptics
  -> Name
  -- ^ Type name of terminal type
  -> Seq Name
  -- ^ Derive these types
  -> Pinchot t a
  -- ^ Data types are created for every 'Rule' created in the
  -- 'Pinchot'.  The return value from the 'Pinchot' is ignored.
  -> DecsQ
allRulesToLocatedTypes _ termType derives pinc = do
  (names, _) <- goPinchot pinc
  let rs = fmap snd . M.assocs . allRules $ names
  traverse (locationData termType (toList derives)) rs

-- ## Creating locator functions

-- | Advances the location for 'Char' values.  Tabs advance to the
-- next eight-column tab stop; newlines advance to the next line and
-- reset the column number to 1.  All other characters advance the
-- column by 1.
advanceChar :: Char -> Loc -> Loc
advanceChar c (Loc !lin !col !pos)
  | c == '\n' = Loc (lin + 1) 1 (pos + 1)
  | c == '\t' = Loc lin (col + 8 - ((col - 1) `mod` 8)) (pos + 1)
  | otherwise = Loc lin (col + 1) (pos + 1)

-- | Returns a function that locates a particular value.  The
-- function it returns has type
--
-- Parsed -> Locator Located

locateRuleFunction
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> Name
  -- ^ Function of type (t -> Loc -> Loc) that advances locations
  -> Pinchot t (Rule t)
  -> ExpQ
locateRuleFunction qualP qualL adv pinc = do
  (_, rule) <- goPinchot pinc
  locatorExpression qualP qualL adv rule

-- | Creates locators for all rules created in the Pinchot.  Each
-- locator has type
--
-- Parsed -> Locator Located
--
-- Each locator is named @lRULE@, where RULE is the name of the rule
-- that is located.
--
-- The return value of the 'Pinchot' is ignored.

locateAllRules
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> Name
  -- ^ Function of type (t -> Loc -> Loc) that advances locations
  -> Pinchot t a
  -> DecsQ
locateAllRules qualP qualL adv pinc = do
  (names, _) <- goPinchot pinc
  let rules = fmap snd . M.assocs . allRules $ names
  fmap concat . traverse (locateSingleRule qualP qualL adv) $ rules

locateSingleRule
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> Name
  -- ^ Function of type (t -> Loc -> Loc) that advances locations
  -> Rule t
  -> DecsQ
locateSingleRule qualParsed qualLoc adv rule@(Rule name _ _)
  = sequence [sig, def]
  where
    locatorName = mkName ("l" ++ name)
    parsedName = quald qualParsed name
    locName = quald qualLoc name
    sig = TH.sigD locatorName [t| $(TH.conT parsedName)
      -> Locator $(TH.conT locName) |]
    def = TH.valD (TH.varP locatorName) (TH.normalB expn) []
      where
        expn = locatorExpression qualParsed qualLoc adv rule

quald :: String -> String -> Name
quald qual suf
  | null qual = mkName suf
  | otherwise = mkName (qual ++ '.':suf)

-- | Given a sequence of rule names, creates a map where each key is
-- a rule name and each value is a 'Name' referring to a function
-- that will lookup the location of a parsed 'Rule'.
functionLookupMap
  :: [String]
  -- ^ All names
  -> TH.Q (M.Map String Name)
functionLookupMap = go M.empty
  where
    go acc [] = return acc
    go acc (x:xs) = do
      n <- TH.newName $ "lookup" ++ x
      go (M.insert x n acc) xs

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

getRuleLocator
  :: Rule t
  -> M.Map String Name
  -> Name
getRuleLocator (Rule nm _ _) mp = case M.lookup nm mp of
  Nothing -> error $ "getRuleLocator: error: rule " ++ nm ++ " not found"
  Just r -> r

-- | Returns an expression of type
--
-- Parsed -> Locator Located
--
-- Includes all expressions that locate each necessary rule.

locatorExpression
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> Name
  -- ^ Function of type (t -> Loc -> Loc) that advances locations
  -> Rule t
  -> ExpQ
locatorExpression qualP qualL adv rule = do
  let rules = toList . ruleAndAncestors $ rule
  lkp <- functionLookupMap . fmap (\(Rule n _ _) -> n) $ rules
  let nameExpnPair rule =
        (getRuleLocator rule lkp, 
         (localLocatorExpression qualP qualL adv lkp rule) )
      nameExpnPairs = fmap nameExpnPair rules
      makeDec (name, expn) = TH.valD (TH.varP name) (TH.normalB expn) []
      thisName = getRuleLocator rule lkp
  TH.letE (fmap makeDec nameExpnPairs) (TH.varE thisName)

-- | Given a map of expressions that locate each rule, returns an
-- expression of type
--
-- Parsed -> Locator Located
--
-- The expressions that locate each necessary rule must be created
-- separately.
localLocatorExpression
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> Name
  -- ^ Function of type (t -> Loc -> Loc) that advances locations
  -> M.Map String Name
  -- ^ Map of all rule locating functions
  -> Rule t
  -> ExpQ
localLocatorExpression qualP qualL adv lkp (Rule name _ ty) = case ty of
  RTerminal _ -> do
    char <- TH.newName "_char"
    state <- TH.newName "_state"
    let outerPat = TH.conP (quald qualP name) [TH.varP char]
        stmts = [|
          do { $(TH.varP state) <- get
             ; put $ $(TH.varE adv) $(TH.varE char) $(TH.varE state)
             ; return $ $(TH.conE (quald qualL name))
                $(TH.varE state) $(TH.varE char)
             } |]
    [| \ $(outerPat) -> $(stmts) |]

  RSeqTerm _ -> do
    bound <- TH.newName "_seqTerm"
    let outerPat = TH.conP (quald qualP name) [TH.varP bound]
        traverser = [| \c -> do { pos <- get
                                ; put ( $(TH.varE adv) c pos )
                                ; return (pos, c)
                                }
                    |]
    [| \ $(outerPat) -> fmap $(TH.conE (quald qualL name))
        $ traverse $(traverser) $(TH.varE bound) |]

  RBranch (b1, bs) -> do
    bound <- TH.newName "_branch"
    let outerPat = TH.varP bound
    [| \ $(outerPat) -> $(TH.caseE (TH.varE bound) matches) |]
    where
      calcBranch = branchParsedToLocated qualP qualL lkp
      matches = calcBranch b1 : toList (fmap calcBranch  bs)

  RUnion (r1, rs) -> do
    bound <- TH.newName "_union"
    let outerPat = TH.varP bound
    [| \ $(outerPat) -> $(TH.caseE (TH.varE bound) matches) |]
    where
      calcBranch = unionBranchToLocated qualP qualL name lkp
      matches = calcBranch r1 : toList (fmap calcBranch rs)

  ROptional rule -> do
    bound <- TH.newName "_optional"
    let outerPat = TH.conP (quald qualP name) [TH.varP bound]
    [| \ $(outerPat) -> do
        { pos <- get
        ; inside <- case $(TH.varE bound) of
            { Nothing -> return Nothing
            ; Just ins -> fmap Just
                $ $(TH.varE (getRuleLocator rule lkp)) ins
            }
        ; return $ $(TH.conE (quald qualL name)) pos inside
        }
      |]

  RList rule -> do
    bound <- TH.newName "_list"
    let outerPat = TH.conP (quald qualP name) [TH.varP bound]
        traverser = [| \ listElem -> do
                        { pos <- get
                        ; listElem' <- $(TH.varE (getRuleLocator rule lkp))
                            listElem
                        ; return (pos, listElem')
                        }
                    |]
    [| \ $(outerPat) -> do
          { pos <- get
          ; ls' <- traverse $(traverser) $(TH.varE bound)
          ; return $ $(TH.conE (quald qualL name)) pos ls'
          } |]

  RList1 rule -> do
    elem1 <- TH.newName "_elem1"
    elemSq <- TH.newName "_elemSq"
    let outerPat = TH.conP (quald qualP name)
          [TH.tupP [TH.varP elem1, TH.varP elemSq]]
        traverser = [| \ listElem -> do
                        { pos <- get
                        ; listElem' <- $(TH.varE (getRuleLocator rule lkp))
                            listElem
                        ; return (pos, listElem')
                        }
                    |]
    [| \ $(outerPat) -> do
          { pos <- get
          ; elem1' <- $(TH.varE (getRuleLocator rule lkp))
              $(varE elem1)
          ; elemSq' <- traverse $(traverser) $(TH.varE elemSq)
          ; return $ $(TH.conE (quald qualL name)) pos (elem1', elemSq')
          } |]

  RWrap rule -> do
    bound <- TH.newName "_wrap"
    let outerPat = TH.conP (quald qualP name) [TH.varP bound]
    [| \ $(outerPat) -> do
          { inner <- $(TH.varE (getRuleLocator rule lkp)) $(TH.varE bound)
          ; return ( $(TH.conE (quald qualL name)) inner)
          } |]

  RRecord rules -> do
    let parsedCtorName = quald qualP name
        locCtorName = quald qualL name
    recs <- fmap toList $ traverse (recordField lkp) rules
    let outerPat = TH.conP parsedCtorName (fmap fst3 recs)
        rtnVal = foldl addField (TH.conE locCtorName) . fmap thd3 $ recs
          where
            addField acc field = [| $(acc) $(TH.varE field) |]
        returner = TH.noBindS [| return $(rtnVal) |]
        doExpn = TH.doE (fmap snd3 recs ++ [returner])
    [| \ $(outerPat) -> $(doExpn) |]

branchParsedToLocated
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> M.Map String Name
  -- ^ Map of all rule locating functions
  -> Branch t
  -> TH.MatchQ
branchParsedToLocated qualP qualL lkp (Branch branchName rules) = do
  patsStmtsNames <-
    traverse (branchRuleToPatAndBody lkp)
      . toList $ rules
  location <- TH.newName "_location"
  let pat = TH.ConP (quald qualP branchName)
        (fmap (\(a, _, _) -> a) patsStmtsNames)
      expn = TH.DoE (getLocation : locatedStmts ++ [lastStmt])
      getLocation = TH.BindS (TH.VarP location) (TH.VarE 'get)
      locatedStmts = fmap (\(_, a, _) -> a) patsStmtsNames
      lastStmt = TH.NoBindS $ TH.VarE 'return
        `TH.AppE` locatedValue
      locatedValue = foldl addField start patsStmtsNames
        where
          addField acc (_, _, fieldName) = acc `TH.AppE`
            (TH.VarE fieldName)
          start = TH.ConE (quald qualL branchName)
            `TH.AppE` (TH.VarE location)
  return $ TH.Match pat (TH.NormalB expn) []

branchRuleToPatAndBody
  :: M.Map String Name
  -- ^ Map of all rule locating functions
  -> Rule t
  -> TH.Q (TH.Pat, TH.Stmt, TH.Name)
  -- ^ Returns:
  -- * a pattern that matches a field in the parsed value
  --
  -- * a do statement that binds the located value for this field
  --
  -- * the name of the variable bound in the do statement
branchRuleToPatAndBody lkp rule = do
  parsedField <- TH.newName "_parsedField"
  locatedValue <- TH.newName "_locatedValue"
  locateRule <- [| $(TH.varE (getRuleLocator rule lkp))
    $(varE parsedField) |]
  let stmt = TH.BindS (TH.VarP locatedValue) locateRule
  return (TH.VarP parsedField, stmt, locatedValue)

unionBranchToLocated
  :: Qualifier
  -- ^ Module that contains types for the parsed value
  -> Qualifier
  -- ^ Module that contains types for the located values
  -> String
  -- ^ Rule name
  -> M.Map String Name
  -- ^ Map of all rule locating functions
  -> Rule t
  -> TH.MatchQ
unionBranchToLocated qualP qualL ruleName lkp inner@(Rule innerNm _ _) = do
  field <- TH.newName "_field"
  let pat = TH.conP
        (quald qualP (unionBranchName ruleName innerNm))
        [TH.varP field]
      getVal = TH.varE (getRuleLocator inner lkp)
      ctor = TH.conE (quald qualL (unionBranchName ruleName innerNm))
      expn = [| do { loc <- get
                   ; val <- $(getVal) $(TH.varE field)
                   ; return $ $(ctor) loc val
                   } |]
  TH.match pat (TH.normalB expn) []

-- | Processes fields in a record.  Given a Rule, returns:
--
-- * a pattern used to bind the corresponding parsed value;
-- * a do statement to get the location and located value, and
-- * the name of the value bound in the do statement.
recordField
  :: M.Map String Name
  -- ^ Map of all rule locating functions
  -> Rule t
  -> TH.Q (TH.PatQ, TH.StmtQ, TH.Name)
recordField lkp rule = do
  nameParsed <- TH.newName "nameParsed"
  nameLocated <- TH.newName "nameLocated"
  let patParsed = TH.varP nameParsed
      expBind = [| do { loc <- get
                      ; val <- $(TH.varE (getRuleLocator rule lkp))
                                $(TH.varE nameParsed)
                      ; return (loc, val)
                      }
                |]
      stmt = TH.bindS (TH.varP nameLocated) expBind
  return (patParsed, stmt, nameLocated)


