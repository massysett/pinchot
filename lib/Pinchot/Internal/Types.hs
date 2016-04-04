{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pinchot.Internal.Types where

import Control.Exception (Exception)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (State)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Typeable (Typeable)

import Pinchot.Intervals

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
type AlternativeName = String

-- | A branch in a sum rule.  In @Branch s ls@, @s@ is the name of the
-- data constructor, and @ls@ is the list of rules that this branch
-- produces.
data Branch t = Branch String (Seq (Rule t))
  deriving (Eq, Ord, Show)

data RuleType t
  = RTerminal (Intervals t)
  | RBranch (Branch t, Seq (Branch t))
  | RUnion (Rule t, Seq (Rule t))
  | RSeqTerm (Seq t)
  | ROptional (Rule t)
  | RList (Rule t)
  | RList1 (Rule t)
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
  = Pinchot { runPinchot :: ExceptT Error (State (Names t)) a }
  deriving (Functor, Applicative, Monad, MonadFix)

-- | Should optics be made?
type MakeOptics = Bool

