{-# LANGUAGE OverloadedLists #-}
module Pinchot.Internal.Rules where

import qualified Control.Lens as Lens
import Control.Monad (join)
import Control.Monad.Trans.State (get, put, State)
import qualified Control.Monad.Trans.State as State
import Data.Monoid ((<>))
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Pinchot.Internal.Types
import Pinchot.Intervals

-- | Name a 'Rule' for use in error messages.  If you do not name a
-- rule using this combinator, the rule's type name will be used in
-- error messages.
label :: Rule t -> String -> Rule t
label (Rule n _ t) s = Rule n (Just s) t

-- | Infix synonym for 'label'.
(<?>) :: Rule t -> String -> Rule t
(<?>) = label
infixr 0 <?>

-- | Constructs a 'Rule' with no description.
rule :: RuleName -> RuleType t -> Rule t
rule n = Rule n Nothing

-- | Creates a terminal production rule.
terminal
  :: RuleName
  -> Intervals t
  -- ^ Valid terminal symbols
  -> Rule t
terminal n i = rule n (Terminal i)

-- | Creates a non-terminal production rule.  This is the most
-- flexible way to create non-terminals.  You can even create a
-- non-terminal that depends on itself.

nonTerminal
  :: RuleName
  -> Seq (BranchName, Seq (Rule t))
  -- ^ Branches of the non-terminal production rule.  This 'Seq'
  -- must have at least one element; otherwise, an error will
  -- result.
  -> Rule t
nonTerminal n branches = case Lens.uncons branches of
  Nothing -> error $ "nonTerminal: rule has no branches: " ++ n
  Just (b, bs) -> rule n (NonTerminal (f b) (fmap f bs))
    where
      f = uncurry Branch

-- | Creates a non-terminal production rule where each branch has
-- only one production.  This function ultimately uses
-- 'nonTerminal'.  Each branch is assigned a 'BranchName' that is
--
-- @RULE_NAME'PRODUCTION_NAME@
--
-- where @RULE_NAME@ is the name of the rule itself, and
-- @PRODUCTION_NAME@ is the rule name for what is being produced.
union
  :: RuleName
  -> Seq (Rule t)
  -- ^ List of branches.  There must be at least one branch;
  -- otherwise a compile-time error will occur.
  -> Rule t
union n rs = nonTerminal n (fmap f rs)
  where
    f rule@(Rule branchName _ _)
      = (n ++ '\'' : branchName, Seq.singleton rule)

-- | Creates a production for a sequence of terminals.  Useful for
-- parsing specific words.
terminals
  :: RuleName
  -> Seq t
  -> Rule t
terminals n s = rule n (Terminals s)

-- | Creates a newtype wrapper.
wrap
  :: RuleName
  -> Rule t
  -- ^ The resulting 'Rule' simply wraps this 'Rule'.
  -> Rule t
wrap n r = rule n (Wrap r)


-- | Creates a new non-terminal production rule with only one
-- alternative where each field has a record name.  The name of each
-- record is:
--
-- @_r\'RULE_NAME\'INDEX\'FIELD_TYPE@
--
-- where @RULE_NAME@ is the name of this rule, @INDEX@ is the index number
-- for this field (starting with 0), and @FIELD_TYPE@ is the type of the
-- field itself.
--
-- Currently there is no way to change the names of the record fields.
record
  :: RuleName
  -- ^ The name of this rule, which is used both as the type name and

  -> Seq (Rule t)
  -- ^ The right-hand side of this rule.  This sequence can be empty,
  -- which results in an epsilon production.
  -- the name of the sole data constructor.
  -> Rule t
record n rs = rule n (Record rs)

-- | Creates a rule for a production that optionally produces another
-- rule.  The name for the created 'Rule' is the name of the 'Rule' to
-- which this function is applied, with @'Opt@ appended to the end.
opt
  :: Rule t
  -> Rule t
opt r@(Rule innerNm _ _) = rule n (Opt r)
  where
    n = innerNm ++ "'Opt"

-- | Creates a rule for the production of a sequence of other rules.
-- The name for the created 'Rule' is the name of the 'Rule' to which
-- this function is applied, with @'Star@ appended.
star
  :: Rule t
  -> Rule t
star r@(Rule innerNm _ _) = rule (innerNm ++ "'Star") (Star r)

-- | Creates a rule for a production that appears at least once.  The
-- name for the created 'Rule' is the name of the 'Rule' to which this
-- function is applied, with @'Plus@ appended.
plus
  :: Rule t
  -> Rule t
plus r@(Rule innerNm _ _) = rule (innerNm ++ "'Plus") (Plus r)

-- | Gets all ancestor rules to this 'Rule'.  Includes the current
-- rule if it has not already been seen.
getAncestors :: Rule t -> State (Set RuleName) (Seq (Rule t))
getAncestors r@(Rule name _ ty) = do
  set <- get
  if Set.member name set
    then return Seq.empty
    else do
      put (Set.insert name set)
      case ty of
        Terminal _ -> return (Seq.singleton r)
        NonTerminal b1 bs -> do
          as1 <- branchAncestors b1
          ass <- fmap join . traverse branchAncestors $ bs
          return $ r <| as1 <> ass
        Terminals _ -> return (Seq.singleton r)
        Wrap c -> do
          cs <- getAncestors c
          return $ r <| cs
        Record ls -> do
          cs <- fmap join . traverse getAncestors $ ls
          return $ r <| cs
        Opt c -> do
          cs <- getAncestors c
          return $ r <| cs
        Star c -> do
          cs <- getAncestors c
          return $ r <| cs
        Plus c -> do
          cs <- getAncestors c
          return $ r <| cs
  where
    branchAncestors (Branch _ rs) = fmap join . traverse getAncestors $ rs

-- | Gets all ancestor 'Rule's.  Includes the current 'Rule'.  Skips
-- duplicates.
family
  :: Rule t
  -> Seq (Rule t)
family rule = State.evalState (getAncestors rule) Set.empty

-- | Gets all the ancestor 'Rule's of a sequence of 'Rule'.  Includes
-- each 'Rule' that is in the sequence.  Skips duplicates.
families
  :: Seq (Rule t)
  -> Seq (Rule t)
families
  = join
  . flip State.evalState Set.empty
  . traverse getAncestors
