module Pinchot.Types where

import Pinchot.Intervals

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Language.Haskell.TH as T

-- | Type synonym for the name of a production rule.  This will be the
-- name of the type constructor for the corresponding type that will
-- be created, so this must be a valid Haskell type constructor name.
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

-- | A single production rule.  It may be a terminal or a non-terminal.
data Rule t = Rule
  { _ruleName :: RuleName
  , _ruleDescription :: Maybe String
  , _ruleType :: RuleType t
  } deriving (Eq, Ord, Show)

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
  , _branches :: Seq (Rule t)
  } deriving (Eq, Ord, Show)

branchName :: Lens.Lens' (Branch t) BranchName
branchName
  = Lens.lens _branchName (\b n -> b { _branchName = n })

branches :: Lens.Lens' (Branch t) (Seq (Rule t))
branches
  = Lens.lens _branches (\b s -> b { _branches = s})

-- | The type of a particular rule.
data RuleType t
  = Terminal (Intervals t)
  | NonTerminal (Branch t) (Seq (Branch t))
  | Wrap (Rule t)
  | Record (Seq (Rule t))
  | Opt (Rule t)
  | Star (Rule t)
  | Plus (Rule t)
  deriving (Eq, Ord, Show)

_Terminal :: Lens.Prism' (RuleType t) (Intervals t)
_Terminal = Lens.prism' (\i -> Terminal i)
  (\r -> case r of { Terminal i -> Just i; _ -> Nothing })

_NonTerminal :: Lens.Prism' (RuleType t) (Branch t, Seq (Branch t))
_NonTerminal = Lens.prism' (\(b, bs) -> NonTerminal b bs)
  (\r -> case r of { NonTerminal b bs -> Just (b, bs); _ -> Nothing })

_Wrap :: Lens.Prism' (RuleType t) (Rule t)
_Wrap = Lens.prism' (\r -> Wrap r)
  (\r -> case r of { Wrap x -> Just x; _ -> Nothing })

_Record :: Lens.Prism' (RuleType t) (Seq (Rule t))
_Record = Lens.prism' (\r -> Record r)
  (\r -> case r of { Record x -> Just x; _ -> Nothing })

_Opt :: Lens.Prism' (RuleType t) (Rule t)
_Opt = Lens.prism' (\r -> Opt r)
  (\r -> case r of { Opt x -> Just x; _ -> Nothing })

_Star :: Lens.Prism' (RuleType t) (Rule t)
_Star = Lens.prism' (\r -> Star r)
  (\r -> case r of { Star x -> Just x; _ -> Nothing })

_Plus :: Lens.Prism' (RuleType t) (Rule t)
_Plus = Lens.prism' (\r -> Plus r)
  (\r -> case r of { Plus x -> Just x; _ -> Nothing })

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


-- | Prepends a qualifier to a string, and returns the resulting
-- Name.
quald
  :: Qualifier
  -> String
  -- ^ Item to be named - constructor, value, etc.
  -> T.Name
quald qual suf
  | null qual = T.mkName suf
  | otherwise = T.mkName (qual ++ '.':suf)

-- | A location.

data Loc = Loc
  { _line :: Int
  , _col :: Int
  , _pos :: Int
  } deriving (Eq, Ord, Read, Show)

line :: Lens.Lens' Loc Int
line = Lens.lens _line (\r l -> r { _line = l })

col :: Lens.Lens' Loc Int
col = Lens.lens _col (\r l -> r { _col = l })

pos :: Lens.Lens' Loc Int
pos = Lens.lens _pos (\r l -> r { _pos = l })
