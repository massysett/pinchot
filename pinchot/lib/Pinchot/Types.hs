{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pinchot.Types where

import qualified Control.Lens as Lens
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import qualified Language.Haskell.TH as T
import Text.Show.Pretty (PrettyVal(prettyVal))
import qualified Text.Show.Pretty as Pretty

import Pinchot.Pretty

-- | Type synonym for the name of a production rule.  This will be the
-- name of the type constructor for the corresponding type that will
-- be created, so this must be a valid Haskell type constructor name.
-- Typically each context-free grammar that you write will have
-- several production rules; you will want to make sure that every
-- 'RuleName' that you create for a single context-free grammar is
-- unique.  However, Pinchot currently does not check for
-- uniqueness.  If you use names that are not unique, GHC will give
-- an error message when you try to splice the resulting code, as
-- the data types will not have unique names.
type RuleName = String

-- | Type synonym the the name of an alternative in a 'nonTerminal'.
-- This name must not conflict with any other data constructor in
-- your grammar.
type BranchName = String

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

-- | A single production rule.
data Rule t = Rule
  { _ruleName :: RuleName
  , _ruleDescription :: Maybe String
  , _ruleType :: RuleType t
  } deriving (Show, Generic, PrettyVal)

-- Can't use Template Haskell in this module due to corecursive
-- types

ruleName :: Lens.Lens' (Rule t) RuleName
ruleName
  = Lens.lens _ruleName (\r n -> r { _ruleName = n })

ruleDescription :: Lens.Lens' (Rule t) (Maybe String)
ruleDescription
  = Lens.lens _ruleDescription (\r d -> r { _ruleDescription = d })

ruleType :: Lens.Lens' (Rule t) (RuleType t)
ruleType
  = Lens.lens _ruleType (\r t -> r { _ruleType = t })

-- | A branch in a sum rule.  In @Branch s ls@, @s@ is the name of the
-- data constructor, and @ls@ is the list of rules that this branch
-- produces.
data Branch t = Branch
  { _branchName :: BranchName
  , _branches :: [Rule t]
  } deriving (Show, Generic, PrettyVal)

branchName :: Lens.Lens' (Branch t) BranchName
branchName
  = Lens.lens _branchName (\b n -> b { _branchName = n })

branches :: Lens.Lens' (Branch t) [Rule t]
branches
  = Lens.lens _branches (\b s -> b { _branches = s})

newtype Predicate a = Predicate { unPredicate :: T.Q (T.TExp (a -> Bool)) }

instance Show (Predicate a) where show _ = "<predicate>"
instance PrettyVal (Predicate a) where prettyVal _ = Pretty.Con "Predicate" []

-- | The type of a particular rule.
data RuleType t
  = Terminal (Predicate t)
  | NonTerminal (NonEmpty (Branch t))
  | Wrap (Rule t)
  | Record [Rule t]
  | Opt (Rule t)
  | Star (Rule t)
  | Plus (Rule t)
  | Series (NonEmpty t)
  deriving (Show, Generic)

instance PrettyVal t => PrettyVal (RuleType t) where
  prettyVal r = case r of
    Terminal t -> Pretty.Con "Terminal" [prettyVal t]
    NonTerminal ne -> Pretty.Con "NonTerminal" [prettyNonEmpty prettyVal ne]
    Wrap r -> Pretty.Con "Wrap" [prettyVal r]
    Record rs -> Pretty.Con "Record" [Pretty.List $ fmap prettyVal rs]
    Opt r -> Pretty.Con "Opt" [prettyVal r]
    Star r -> Pretty.Con "Star" [prettyVal r]
    Plus r -> Pretty.Con "Plus" [prettyVal r]
    Series ne -> Pretty.Con "Series" [prettyNonEmpty prettyVal ne]

_Terminal :: Lens.Prism' (RuleType t) (T.Q (T.TExp (t -> Bool)))
_Terminal = Lens.prism' (Terminal . Predicate)
  (\r -> case r of { Terminal (Predicate i) -> Just i; _ -> Nothing })

_NonTerminal :: Lens.Prism' (RuleType t) (NonEmpty (Branch t))
_NonTerminal = Lens.prism' NonTerminal
  (\r -> case r of { NonTerminal b -> Just b; _ -> Nothing })

_Wrap :: Lens.Prism' (RuleType t) (Rule t)
_Wrap = Lens.prism' Wrap
  (\r -> case r of { Wrap x -> Just x; _ -> Nothing })

_Record :: Lens.Prism' (RuleType t) [Rule t]
_Record = Lens.prism' Record
  (\r -> case r of { Record x -> Just x; _ -> Nothing })

_Opt :: Lens.Prism' (RuleType t) (Rule t)
_Opt = Lens.prism' Opt
  (\r -> case r of { Opt x -> Just x; _ -> Nothing })

_Star :: Lens.Prism' (RuleType t) (Rule t)
_Star = Lens.prism' Star
  (\r -> case r of { Star x -> Just x; _ -> Nothing })

_Plus :: Lens.Prism' (RuleType t) (Rule t)
_Plus = Lens.prism' Plus
  (\r -> case r of { Plus x -> Just x; _ -> Nothing })

_Series :: Lens.Prism' (RuleType t) (NonEmpty t)
_Series = Lens.prism' Series
  (\r -> case r of { Series s -> Just s; _ -> Nothing })

-- | The name of a field in a record, without the leading
-- underscore.
recordFieldName
  :: Int
  -- ^ Index
  -> String
  -- ^ Parent type name
  -> String
  -- ^ Inner type name
  -> String
recordFieldName idx par inn = "r'" ++ par ++ "'" ++ show idx ++ "'" ++ inn

-- | A location.

data Loc = Loc
  { _line :: Int
  , _col :: Int
  , _pos :: Int
  } deriving (Eq, Ord, Read, Show, Data, Generic, PrettyVal)

line :: Lens.Lens' Loc Int
line = Lens.lens _line (\r l -> r { _line = l })

col :: Lens.Lens' Loc Int
col = Lens.lens _col (\r l -> r { _col = l })

pos :: Lens.Lens' Loc Int
pos = Lens.lens _pos (\r l -> r { _pos = l })
