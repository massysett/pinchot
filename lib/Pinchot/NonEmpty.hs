{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Sequences that always contain at least one element.

module Pinchot.NonEmpty where

import Control.Monad (join, ap)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Control.Lens as Lens

-- | A non-empty sequence.
data NonEmpty a = NonEmpty
  { _front :: a
  -- ^ The first item
  , _rest :: Seq a
  -- ^ All remaining items
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

Lens.makeLenses ''NonEmpty

-- | Convert a 'NonEmpty' to a 'Seq'.
flatten :: NonEmpty a -> Seq a
flatten (NonEmpty a as) = a <| as

instance Monad NonEmpty where
  return a = NonEmpty a Seq.empty
  NonEmpty a as >>= f = NonEmpty (_front r1) rs
    where
      r1 = f a
      rs = _rest r1 `mappend` rss
      rss = join . fmap flatten . fmap f $ as

instance Applicative NonEmpty where
  pure = return
  (<*>) = ap

-- | Converts a non-empty 'Seq' to a 'NonEmpty'; 'Nothing' if the
-- 'Seq' is empty.
seqToNonEmpty :: Seq a -> Maybe (NonEmpty a)
seqToNonEmpty = fmap (uncurry NonEmpty) . Lens.uncons

-- | Prepends a 'Seq' to a 'NonEmpty'.
prependSeq :: Seq a -> NonEmpty a -> NonEmpty a
prependSeq sq (NonEmpty a as) = case Lens.uncons sq of
  Nothing -> NonEmpty a as
  Just (l, ls) -> NonEmpty l (ls `mappend` (a <| as))

-- | Appends a 'Seq' to a 'NonEmpty'.
appendSeq :: NonEmpty a -> Seq a -> NonEmpty a
appendSeq (NonEmpty a as) sq = NonEmpty a (as `mappend` sq)

-- | Associative operation that appends to 'NonEmpty'.
append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append (NonEmpty l1 ls) (NonEmpty r1 rs)
  = NonEmpty l1 (ls `mappend` (r1 <| rs))

-- | Place a single item at the head of the 'NonEmpty'.
singleton :: a -> NonEmpty a
singleton a = NonEmpty a Seq.empty
