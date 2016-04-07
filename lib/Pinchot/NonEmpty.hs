{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Sequences that always contain at least one element.

module Pinchot.NonEmpty where

import Control.Monad (join, ap)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Control.Lens as Lens

data NonEmpty a = NonEmpty
  { _front :: a
  , _rest :: Seq a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

Lens.makeLenses ''NonEmpty

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

seqToNonEmpty :: Seq a -> Maybe (NonEmpty a)
seqToNonEmpty = fmap (uncurry NonEmpty) . Lens.uncons

prependSeq :: Seq a -> NonEmpty a -> NonEmpty a
prependSeq sq (NonEmpty a as) = case Lens.uncons sq of
  Nothing -> NonEmpty a as
  Just (l, ls) -> NonEmpty l (ls `mappend` (a <| as))

appendSeq :: NonEmpty a -> Seq a -> NonEmpty a
appendSeq (NonEmpty a as) sq = NonEmpty a (as `mappend` sq)

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append (NonEmpty l1 ls) (NonEmpty r1 rs)
  = NonEmpty l1 (ls `mappend` (r1 <| rs))

singleton :: a -> NonEmpty a
singleton a = NonEmpty a Seq.empty
