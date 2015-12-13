{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Pinchot internals.  Ordinarily the "Pinchot" module should have
-- everything you need.

module Pinchot.Internal where

import Pinchot.Intervals

import Control.Applicative ((<|>), liftA2)
import Control.Exception (Exception)
import qualified Control.Lens as Lens
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.State (State, runState, get, put)
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
   cxt, conT, Name, dataD, appT, DecsQ, appE, Q, Stmt(NoBindS), uInfixE, bindS,
   varE, varP, conE, Pat, Exp(AppE, DoE), lamE, recC, varStrictType)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as Syntax
import Text.Earley (satisfy, rule, symbol)
import qualified Text.Earley ((<?>))

-- | Type synonym for the name of a production rule.  
-- This will be the name of the type constructor for the corresponding
-- type in the AST, so this must be a valid Haskell type constructor
-- name.
--
-- If you are creating a 'terminal', 'option', 'list', 'list1', or
-- 'wrap', the 'RuleName' will also be used for the name of the single
-- data construtor.  If you are creating a 'nonTerminal', you will
-- specify the name of each data constructor with 'AlternativeName'.
type RuleName = String

-- | Type synonym the the name of an alternative in a 'nonTerminal'.
-- This name must not conflict with any other data constructor, either
-- one specified as an 'AlternativeName' or one that was created using
-- 'terminal', 'option', 'list', or 'list1'.
type AlternativeName = String

-- | A branch in a sum rule.  In @Branch s ls@, @s@ is the name of the
-- data constructor, and @ls@ is the list of rules that this branch
-- produces.
data Branch t = Branch String (Seq (Rule t))
  deriving (Eq, Ord, Show)

data RuleType t
  = RTerminal (Intervals t)
  | RBranch (Branch t, Seq (Branch t))
  | RSeqTerm (Seq t)
  | ROptional (Rule t)
  | RMany (Rule t)
  | RMany1 (Rule t)
  | RWrap (Rule t)
  | RRecord (Seq (Rule t))
  deriving (Eq, Ord, Show)

-- Rule n d t, where
--
-- n is the name of the rule.  This is used as the name of the
-- corresponding data type.
--
-- d is the description of the rule.  This is optional and is used for
-- the parser's error messages.  If there is no description, the name
-- is used for error messages.
--
-- t is the type of rule (terminal, branch, etc.)

-- | A single production rule.  It may be a terminal or a non-terminal.
data Rule t = Rule String (Maybe String) (RuleType t)
  deriving (Eq, Ord, Show)

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

data Names t = Names
  { tyConNames :: Set RuleName
  , dataConNames :: Set String
  , nextIndex :: Int
  , allRules :: Map Int (Rule t)
  } deriving (Eq, Ord, Show)

-- | Errors that may arise when constructing an AST.
data Error
  = InvalidName String
  -- ^ A name was invalid.  The field is the invalid name.  The name
  -- might be invalid because it was already used, or because it does
  -- not begin with a capital letter.
  | EmptyNonTerminal String
  -- ^ A non-terminal must have at least one summand.  The field is
  -- the name of the empty non-terminal.
  deriving (Show, Typeable)

instance Exception Error

-- | Constructs new 'Rule's.  @t@ is the type of the token; often this
-- will be 'Char'.
--
-- 'Pinchot' is a 'Monad' and an 'Applicative' so you can combine
-- computations using the usual methods of those classes.  Also,
-- 'Pinchot' is a 'MonadFix'.  This allows you to construct a 'Rule'
-- that depends on itself, and to construct sets of 'Rule's that have
-- mutually recursive dependencies.  'MonadFix' also allows you to use
-- the GHC @RecursiveDo@ extension.  Put
--
-- @
-- {-\# LANGUAGE RecursiveDo \#-}
-- @
--
-- at the top of your module, then use @mdo@ instead of @do@.  Because
-- an @mdo@ block is recursive, you can use a binding before it is
-- defined, just as you can in a set of @let@ bindings.

newtype Pinchot t a
  = Pinchot { runPinchot :: (ExceptT Error (State (Names t)) a) }
  deriving (Functor, Applicative, Monad, MonadFix)

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
  :: String
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
  RSeqTerm _ -> Seq.singleton n
  ROptional _ -> Seq.singleton n
  RMany _ -> Seq.singleton n
  RMany1 _ -> Seq.singleton n
  RWrap _ -> Seq.singleton n
  RRecord _ -> Seq.singleton n

addDataConNames :: Rule t -> Pinchot t ()
addDataConNames = mapM_ addDataConName . ruleConstructorNames

-- | Creates a new non-terminal production rule where
-- each alternative produces only one rule.
union
  :: RuleName
  -> Seq (AlternativeName, Rule t)
  -> Pinchot t (Rule t)
union name = nonTerminal name . fmap (\(n, r) -> (n, Seq.singleton r))

-- | Creates a new non-terminal production rule with only one
-- alternative where each field has a record name.  The name of each
-- record is:
--
-- @_f\'RULE_NAME\'INDEX\'FIELD_TYPE@
--
-- where RULE_NAME is the name of this rule, INDEX is the index number
-- for this field (starting with 0), and FIELD_TYPE is the type of the
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
list
  :: RuleName

  -> Rule t
  -- ^ The resulting 'Rule' is a sequence of productions of this
  -- 'Rule'; that is, this 'Rule' may appear zero or more times.

  -> Pinchot t (Rule t)
list name r = newRule name (RMany r)

-- | Creates a rule for a production that appears at least once.
list1
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' produces this 'Rule' at least once.
  -> Pinchot t (Rule t)
list1 name r = newRule name (RMany1 r)

-- | Creates a rule for a production that optionally produces another
-- rule.
option
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' optionally produces this 'Rule'; that is,
  -- this 'Rule' may appear once or not at all.

  -> Pinchot t (Rule t)
option name r = newRule name (ROptional r)

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
        RSeqTerm _ -> return (Seq.singleton r)
        ROptional c -> do
          cs <- getAncestors c
          return $ r <| cs
        RMany c -> do
          cs <- getAncestors c
          return $ r <| cs
        RMany1 c -> do
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
          RSeqTerm _ -> results
          ROptional r -> checkRule r results
          RMany r -> addHelper $ checkRule r results
          RMany1 r -> addHelper $ checkRule r results
          RWrap r -> checkRule r results
          RRecord sq -> foldr checkRule results $ sq
        checkRule (Rule name _ _) rslts
          | Set.member name lhsDefined = rslts
          | otherwise = Set.insert (ruleName name) rslts
        addHelper = Set.insert (helperName nm)
  

thBranch :: Branch t -> ConQ
thBranch (Branch nm rules) = normalC name fields
  where
    name = mkName nm
    mkField (Rule n _ _) = strictType notStrict (conT (mkName n))
    fields = toList . fmap mkField $ rules


thRule
  :: Bool
  -- ^ If True, make lenses.
  -> Name
  -- ^ Name of terminal type
  -> Seq Name
  -- ^ What to derive
  -> Rule t
  -> TH.Q [TH.Dec]
thRule doLenses typeName derives (Rule nm _ ruleType) = do
  ty <- makeType typeName derives nm ruleType
  return (ty : lenses)
  where
    lenses
      | doLenses = ruleToOptics typeName nm ruleType
      | otherwise = []


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

  RMany (Rule inner _ _) -> newtypeD (cxt []) name [] newtypeCon derives
    where
      newtypeCon = normalC name
        [strictType notStrict (appT [t| Seq |]
                                    (conT (mkName inner)))]

  RMany1 (Rule inner _ _) -> newtypeD (cxt []) name [] cons derives
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
fieldName idx par inn = "f'" ++ par ++ "'" ++ show idx ++ "'" ++ inn

thAllRules
  :: Bool
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

terminalToOptics
  :: Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> TH.Dec
terminalToOptics terminalName = makeWrapped term
  where
    term = TH.ConT terminalName

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
branchesToOptics nm b1 bsSeq = concat $ makePrism b1 : toList (fmap makePrism bs)
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
  :: Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> RuleType t
  -> [TH.Dec]
ruleToOptics terminalName nm ty = case ty of
  RTerminal _ -> [terminalToOptics terminalName nm]
  RBranch (b1, bs) -> branchesToOptics nm b1 bs
  RSeqTerm _ -> [terminalSeqToOptics terminalName nm]
  ROptional (Rule inner _ _) -> [optionalToOptics inner nm]
  RMany (Rule inner _ _) -> [manyToOptics inner nm]
  RMany1 (Rule inner _ _) -> [many1ToOptics inner nm]
  RWrap (Rule inner _ _) -> [wrapToOptics inner nm]
  RRecord recs -> recordsToOptics nm recs

-- | Should optics be made?
type MakeOptics = Bool

-- | Creates optics.  If you use this option you will need to have a
--
-- {-\# LANGUAGE TypeFamilies \#-}
--
-- pragma at the top of the module in which you splice this in.
--
-- Creates the listed optics for each kind of
-- 'Rule', as follows:
--
-- * 'terminal': 'Lens.Wrapped', wrapping the type of the terminal.
--
-- * 'terminalSeq': 'Lens.Wrapped', wrapping a @'Seq' a@, where @a@ is
-- the type of the terminal.
--
-- * 'nonTerminal': one 'Lens.Prism' for each data constructor (even if
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

allRulesToTypes

  :: MakeOptics

  -> Name
  -- ^ Terminal type constructor name.  Typically you will use the
  -- Template Haskell quoting mechanism to get this.

  -> Seq Name
  -- ^ What to derive.  For instance, you might use @Eq@, @Ord@, and
  -- @Show@ here.  Each created data type will derive these instances.

  -> Pinchot t a
  -- ^ The return value from the 'Pinchot' is ignored.

  -> DecsQ
allRulesToTypes doOptics typeName derives pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right _ -> thAllRules doOptics typeName derives (allRules st')
  where
    (ei, st') = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)

-- | Creates data types only for the 'Rule' returned from the 'Pinchot', and
-- for its ancestors.
ruleTreeToTypes
  :: MakeOptics

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
ruleTreeToTypes doOptics typeName derives pinchot = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right r -> fmap join . sequence . toList
    . fmap (thRule doOptics typeName derives)
    . runCalc . getAncestors $ r
  where
    runCalc stateCalc = fst $ runState stateCalc (Set.empty)
    (ei, _) = runState (runExceptT (runPinchot pinchot))
      (Names Set.empty Set.empty 0 M.empty)



ruleToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Rule t
  -> Q [Stmt]
ruleToParser prefix (Rule nm mayDescription rt) = case rt of

  RTerminal ivls -> do
    topRule <- makeRule expression
    return [topRule]
    where
      expression = [| fmap $constructor (satisfy (inIntervals ivls)) |]

  RBranch (b1, bs) -> do
    topRule <- makeRule expression
    return [topRule]
    where
      expression = foldl addBranch (branchToParser prefix b1) bs
        where
          addBranch tree branch =
            [| $tree <|> $(branchToParser prefix branch) |]

  RSeqTerm sq -> do
    let nestRule = bindS (varP helper) [| rule $(foldl addTerm start sq) |]
          where
            start = [|pure Seq.empty|]
            addTerm acc x = [| liftA2 (<|) (symbol x) $acc |]
    nest <- nestRule
    topRule <- makeRule (wrapper helper)
    return [nest, topRule]

  ROptional (Rule innerNm _ _) -> fmap (:[]) (makeRule expression)
    where
      expression = [| fmap $constructor (pure Nothing <|> $(just)) |]
        where
          just = [| fmap Just $(varE (ruleName innerNm)) |]

  RMany (Rule innerNm _ _) -> do
    let nestRule = bindS (varP helper) ([|rule|] `appE` parseSeq)
          where
            parseSeq = uInfixE [|pure Seq.empty|] [|(<|>)|] pSeq
              where
                pSeq = [|liftA2 (<|) $(varE (ruleName innerNm)) $(varE helper) |]
    nest <- nestRule
    top <- makeRule $ wrapper helper
    return [nest, top]

  RMany1 (Rule innerNm _ _) -> do
    let nestRule = bindS (varP helper) [|rule $(parseSeq)|]
          where
            parseSeq = [| pure Seq.empty <|> $pSeq |]
              where
                pSeq = [| (<|) <$> $(varE (ruleName innerNm))
                               <*> $(varE helper) |]
    nest <- nestRule
    let topExpn = [| $constructor <$> ( (,) <$> $(varE (ruleName innerNm))
                                        <*> $(varE helper)
                                      ) |]
    top <- makeRule topExpn
    return [nest, top]

  RWrap (Rule innerNm _ _) -> fmap (:[]) (makeRule expression)
    where
      expression = [|fmap $constructor $(varE (ruleName innerNm)) |]

  RRecord sq -> fmap (:[]) (makeRule expression)
    where
      expression = case viewl sq of
        EmptyL -> [| pure $constructor |]
        Rule r1 _ _ :< restFields -> foldl addField fstField restFields
          where
            fstField = [| $constructor <$> $(varE (ruleName r1)) |]
            addField soFar (Rule r _ _)
              = [| $soFar <*> $(varE (ruleName r)) |]
    

  where
    makeRule expression = varP (ruleName nm) `bindS`
      [|rule ($expression Text.Earley.<?> $(textToExp desc))|]
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
ruleName suffix = mkName ("_r'" ++ suffix)

helperName :: String -> Name
helperName suffix = mkName ("_h'" ++ suffix)

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
    
-- | Creates a lazy pattern for all the given names.  Adds an empty
-- pattern onto the front.
lazyPattern
  :: Foldable c
  => c Name
  -> Q Pat
lazyPattern = finish . foldr gen [p| () |]
  where
    gen name rest = [p| ($(varP name), $rest) |]
    finish pat = [p| ~(_, $pat) |]

bigTuple
  :: Foldable c
  => Name
  -> c Name
  -> ExpQ
bigTuple top = finish . foldr f [| () |]
  where
    f n rest = [| ( $(varE n), $rest) |]
    finish tup = [| ($(varE top), $tup) |]

-- | Creates an Earley grammar for a given 'Rule'.  For examples of how
-- to use this, see the source code for
-- "Pinchot.Examples.PostalAstRuleTree" and for
-- "Pinchot.Examples.PostalAstAllRules".

earleyGrammar
  :: Syntax.Lift t

  => String
  -- ^ Module prefix.  You have to make sure that the data types you
  -- created with 'ruleTreeToTypes' or with 'allRulesToTypes' are in
  -- scope, either because they were spliced into the same module that
  -- 'earleyParser' is spliced into, or because they are @import@ed
  -- into scope.  The spliced Template Haskell code has to know where
  -- to look for these data types.  If you did an unqualified @import@
  -- or if the types are in the same module as is the splice of
  -- 'earleyParser', just pass the empty string here.  If you did a
  -- qualified import, pass the appropriate namespace here.
  --
  -- For example, if you used @import qualified MyAst@, pass
  -- @\"MyAst\"@ here.  If you used @import qualified
  -- Data.MyLibrary.MyAst as MyLibrary.MyAst@, pass
  -- @\"MyLibrary.MyAst\"@ here.
  --
  -- For an example where the types are in the same module, see
  -- "Pinchot.Examples.PostalAstRuleTree" or
  -- "Pinchot.Examples.PostalAstAllRules".
  --
  -- For an example using a qualified import, see
  -- "Pinchot.Examples.QualifiedImport".

  -> Pinchot t (Rule t)
  -- ^ Creates an Earley parser for the 'Rule' that the 'Pinchot'
  -- returns.
  -> Q Exp
earleyGrammar prefix pinc = case ei of
  Left err -> fail $ "pinchot: bad grammar: " ++ show err
  Right r@(Rule top _ _) -> do
    let neededRules = ruleAndAncestors r
        otherNames = rulesDemandedBeforeDefined neededRules
        lamb = lamE [lazyPattern otherNames] expression
        expression = do
          stmts <- fmap concat . mapM (ruleToParser prefix)
            . toList $ neededRules
          result <- bigTuple (ruleName top) otherNames
          rtn <- [|return|]
          let returner = rtn `AppE` result
          return $ DoE (stmts ++ [NoBindS returner])
    [| fmap fst (mfix $lamb) |]
  where
    (ei, _) = runState (runExceptT (runPinchot pinc))
      (Names Set.empty Set.empty 0 M.empty)