{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Creating Earley parsers.

module Pinchot.Earley where

import Pinchot.NonEmpty
import Pinchot.RecursiveDo
import Pinchot.Rules
import Pinchot.Types
import Pinchot.Intervals

import Control.Applicative ((<|>), liftA2)
import Data.Foldable (toList)
import Data.Sequence ((<|), viewl, ViewL(EmptyL, (:<)), Seq)
import qualified Data.Sequence as Seq
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Syntax as Syntax
import qualified Text.Earley

-- | Creates a list of pairs.  Each list represents a statement in
-- @do@ notation.  The first element of the pair is the name of the
-- variable to which to bind the result of the expression, which is
-- the second element of the pair.
ruleToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Rule t
  -> [(T.Name, T.ExpQ)]
ruleToParser prefix (Rule nm mayDescription rt) = case rt of

  Terminal ivls -> [makeRule expression]
    where
      expression =
        [| let f (c, a)
                | inIntervals ivls c = Just
                    ($(T.conE (quald prefix nm)) (c, a))
                | otherwise = Nothing
           in Text.Earley.terminal f |]

  NonTerminal b1 bs -> [makeRule expression]
    where
      expression = foldl addBranch (branchToParser prefix b1) bs
        where
          addBranch tree branch =
            [| $tree <|> $(branchToParser prefix branch) |]

  Wrap (Rule innerNm _ _) -> [makeRule expression]
    where
      expression = [|fmap $constructor $(T.varE (localRuleName innerNm)) |]

  Record sq -> [makeRule expression]
    where
      expression = case viewl sq of
        EmptyL -> [| pure $constructor |]
        Rule r1 _ _ :< restFields -> foldl addField fstField restFields
          where
            fstField = [| $constructor <$> $(T.varE (localRuleName r1)) |]
            addField soFar (Rule r _ _)
              = [| $soFar <*> $(T.varE (localRuleName r)) |]
    
  Opt (Rule innerNm _ _) -> [makeRule expression]
    where
      expression = [| fmap $constructor (pure Nothing <|> $(just)) |]
        where
          just = [| fmap Just $(T.varE (localRuleName innerNm)) |]

  Star (Rule innerNm _ _) -> [nestRule, makeRule (wrapper helper)]
    where
      nestRule = (helper, ([|Text.Earley.rule|] `T.appE` parseSeq))
        where
          parseSeq = T.uInfixE [|pure Seq.empty|] [|(<|>)|] pSeq
            where
              pSeq = [|liftA2 (<|)
                $(T.varE (localRuleName innerNm)) $(T.varE helper) |]

  Plus (Rule innerNm _ _) -> [nestRule, makeRule topExpn]
    where
      nestRule = (helper, [|Text.Earley.rule $(parseSeq)|])
        where
          parseSeq = [| pure Seq.empty <|> $pSeq |]
            where
              pSeq = [| (<|) <$> $(T.varE (localRuleName innerNm))
                             <*> $(T.varE helper) |]
      topExpn = [| $constructor <$>
        ( NonEmpty <$> $(T.varE (localRuleName innerNm))
                   <*> $(T.varE helper)) |]


  where
    makeRule expression = (localRuleName nm,
      [|Text.Earley.rule ($expression Text.Earley.<?> $(textToExp desc))|])
    desc = maybe nm id mayDescription
    textToExp txt = [| $(Syntax.lift txt) |]
    constructor = T.conE (quald prefix nm)
    wrapper wrapRule = [|fmap $constructor $(T.varE wrapRule) |]
    helper = helperName nm

localRuleName :: String -> T.Name
localRuleName suffix = T.mkName ("_rule'" ++ suffix)

helperName :: String -> T.Name
helperName suffix = T.mkName ("_helper'" ++ suffix)

branchToParser
  :: Syntax.Lift t
  => String
  -- ^ Module prefix
  -> Branch t
  -> T.ExpQ
branchToParser prefix (Branch name rules) = case viewl rules of
  EmptyL -> [| pure $constructor |]
  (Rule rule1 _ _) :< xs -> foldl f z xs
    where
      z = [| $constructor <$> $(T.varE (localRuleName rule1)) |]
      f soFar (Rule rule2 _ _) = [| $soFar <*> $(T.varE (localRuleName rule2)) |]
  where
    constructor = T.conE (quald prefix name)

-- | Creates an expression that has type
--
-- 'Text.Earley.Grammar' r (Prod r String (c, a) (p c a))
--
-- where @r@ is left universally quantified; @c@ is the terminal
-- type (often 'Char'), @a@ is arbitrary metadata about each token
-- (often 'Loc') and @p@ is the data type corresponding to
-- the given 'Rule'.
--
-- Example:  'Pinchot.Examples.Earley.addressGrammar'.
earleyGrammarFromRule
  :: Syntax.Lift t
  => Qualifier
  -- ^ Module prefix holding the data types created with
  -- 'Pinchot.syntaxTrees'
  -> Rule t
  -- ^ Create a grammar for this 'Rule'
  -> T.Q T.Exp
earleyGrammarFromRule prefix r@(Rule top _ _) = recursiveDo binds final
  where
    binds = concatMap (ruleToParser prefix) . toList . family $ r
    final = [| return $(T.varE $ localRuleName top) |]

-- | Creates a record data type that holds a value of type
--
-- @'Text.Earley.Prod' r 'String' (t, a) (p t a)@
--
-- where
--
-- * @r@ is left universally quantified
--
-- * @t@ is the token type (often 'Char')
--
-- * @a@ is any additional information about each token (often
-- 'Pinchot.Loc')
--
-- * @p@ is the type of the particular production
--
-- This always creates a single product type whose name is
-- @Productions@; currently the name cannot be configured.
--
-- Example: "Pinchot.Examples.SyntaxTrees".
allRulesRecord
  :: Qualifier
  -- ^ Qualifier for data types corresponding to those created from
  -- the 'Rule's
  -> T.Name
  -- ^ Name of terminal type.  Typically you will get this through
  -- the Template Haskell quoting mechanism, such as @''Char@.
  -> Seq (Rule t)
  -- ^ A record is created that holds a value for each 'Rule'
  -- in the 'Seq', as well as for every ancestor of these 'Rule's.
  -> T.DecsQ
  -- ^ When spliced, this will create a single declaration that is a
  -- record with the name @Productions@.
  --
  -- @a'NAME@
  --
  -- where @NAME@ is the name of the type.  Don't count on these
  -- records being in any particular order.
allRulesRecord prefix termName ruleSeq
  = sequence [T.dataD (return []) (T.mkName nameStr) tys [con] []]
  where
    nameStr = "Productions"
    tys = [T.PlainTV (T.mkName "r"), T.PlainTV (T.mkName "t"),
      T.PlainTV (T.mkName "a")]
    con = T.recC (T.mkName nameStr)
        (fmap mkRecord . toList . families $ ruleSeq)
    mkRecord (Rule ruleNm _ _) = T.varStrictType recName st
      where
        recName = T.mkName ("a'" ++ ruleNm)
        st = T.strictType T.notStrict ty
          where
            ty = [t| Text.Earley.Prod $(T.varT (T.mkName "r"))
                      String
                      ( $(T.varT (T.mkName "t")), $(T.varT (T.mkName "a")))
                      ( $(T.conT (T.mkName nameWithPrefix))
                            $(T.varT (T.mkName "t"))
                            $(T.varT (T.mkName "a"))) |]
            nameWithPrefix = case prefix of
              [] -> ruleNm
              _ -> prefix ++ '.' : ruleNm

-- | Creates a 'Text.Earley.Grammar' that contains a
-- 'Text.Earley.Prod' for every given 'Rule' and its ancestors.
-- Example: 'Pinchot.Examples.Earley.addressAllProductions'.
earleyProduct
  :: Syntax.Lift t

  => Qualifier
  -- ^ Qualifier for data types corresponding to those created from
  -- the 'Rule's

  -> Qualifier
  -- ^ Qualifier for the type created with 'allRulesRecord'

  -> Seq (Rule t)
  -- ^ Creates an Earley grammar that contains a 'Text.Earley.Prod'
  -- for each 'Rule' in this 'Seq', as well as all the ancestors of
  -- these 'Rule's.

  -> T.ExpQ
  -- ^ When spliced, 'earleyProduct' creates an expression whose
  -- type is @'Text.Earley.Grammar' r (Productions r t a)@, where
  -- @Productions@ is
  -- the type created by 'allRulesRecord'; @r@ is left universally
  -- quantified; @t@ is the token type (often 'Char'), and @a@ is
  -- any additional information about each token (often
  -- 'Pinchot.Loc').
earleyProduct pfxRule pfxRec ruleSeq = do
  let binds = concatMap (ruleToParser pfxRule)
        . toList . families $ ruleSeq
      final = [| return
        $(T.recConE (T.mkName rulesRecName)
                    (fmap mkRec . toList . families $ ruleSeq)) |]
        
  recursiveDo binds final
  where
    rulesRecName
      | null pfxRec = "Productions"
      | otherwise = pfxRec ++ ".Productions"
    mkRec (Rule n _ _) = return (T.mkName recName, recVal)
      where
        recName
          | null pfxRec = "a'" ++ n
          | otherwise = pfxRec ++ ".a'" ++ n
        recVal = T.VarE . localRuleName $ n

