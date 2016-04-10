{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Intervals describe terminal symbols.  Ordinarily you will not
-- need to use this module, as "Pinchot" re-exports the things you
-- usually need.
module Pinchot.Intervals where

import qualified Control.Lens as Lens
import Control.Monad (join)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq, ViewL(EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as Seq
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Groups of terminals.  Create an 'Intervals' using 'include',
-- 'exclude', 'solo' and 'pariah'.  Combine 'Intervals' using
-- 'mappend', which will combine both the included and excluded
-- terminal symbols from each operand.
data Intervals a = Intervals
  { _included :: Seq (a, a)
  -- ^ Each pair @(a, b)@ is an inclusive range of terminal symbols,
  -- in order.  For instance, @('a', 'c')@ includes the characters
  -- @'a'@, @'b'@, and @'c'@.  The 'included' sequence contains all
  -- terminals that are included in the 'Intervals', except for those
  -- that are 'excluded'.
  , _excluded :: Seq (a, a)
  -- ^ Each symbol in 'excluded' is not in the 'Intervals', even if
  -- the symbol is 'included'.
  } deriving (Eq, Ord, Show)

Lens.makeLenses ''Intervals

instance Functor Intervals where
  fmap f (Intervals a b) = Intervals (fmap g a) (fmap g b)
    where
      g (x, y) = (f x, f y)

instance Monoid (Intervals a) where
  mempty = Intervals mempty mempty
  (Intervals x1 y1) `mappend` (Intervals x2 y2)
    = Intervals (x1 <> x2) (y1 <> y2)

-- | Include a range of symbols in the 'Intervals'.  For instance, to
-- include the characters @'a'@, @'b'@, and @'c'@, use @include 'a'
-- 'c'@.  Example: 'Pinchot.Examples.Postal.rLetter'.
include :: a -> a -> Intervals a
include l h = Intervals [(l, h)] []

-- | Exclude a range of symbols in the 'Intervals'.  Each symbol that
-- is 'exclude'd is not included in the 'Intervals', even if it is
-- also 'include'd.
exclude :: a -> a -> Intervals a
exclude l h = Intervals [] [(l, h)]

-- | Include a single symbol.  Example:
-- 'Pinchot.Examples.Postal.rNorth'.
solo :: a -> Intervals a
solo x = Intervals [(x, x)] []

-- | Exclude a single symbol.
pariah :: a -> Intervals a
pariah x = Intervals [] [(x, x)]

-- | Left endpoint.
endLeft :: Ord a => (a, a) -> a
endLeft (a, b) = min a b

-- | Right endpoint.
endRight :: Ord a => (a, a) -> a
endRight (a, b) = max a b

-- | Is this symbol included in the interval?
inInterval :: Ord a => a -> (a, a) -> Bool
inInterval x i = x >= endLeft i && x <= endRight i

-- | Enumerate all members of an interval.
members :: (Ord a, Enum a) => (a, a) -> Seq a
members i = Seq.fromList [endLeft i .. endRight i]

-- | Sort a sequence of intervals.
sortIntervalSeq :: Ord a => Seq (a, a) -> Seq (a, a)
sortIntervalSeq = Seq.sortBy (comparing endLeft <> comparing endRight)

-- | Arrange an interval so the lower bound is first in the pair.
standardizeInterval :: Ord a => (a, a) -> (a, a)
standardizeInterval (a, b) = (min a b, max a b)

-- | Sorts the intervals using 'sortIntervalSeq' and presents them in a
-- regular order using 'flatten'.  The function @standardizeIntervalSeq a@ has
-- the following properties, where @b@ is the result:
--
-- @
-- 'uniqueMembers' a == 'uniqueMembers' b
--
-- let go [] = True
--     go (_:[]) = True
--     go (x:y:xs)
--          | 'endRight' x < 'endLeft' y
--              && 'endRight' x < pred ('endLeft' x)
--              = go (y:xs)
--          | otherwise = False
-- in go b
-- @
--
-- The second property means that adjacent intervals in the list must
-- be separated by at least one point on the number line.

standardizeIntervalSeq :: (Ord a, Enum a) => Seq (a, a) -> Seq (a, a)
standardizeIntervalSeq = flattenIntervalSeq . sortIntervalSeq

-- | Presents the intervals in a standard order, as described in
-- 'standardizeIntervalSeq'.  If the input has already been sorted with
-- 'sortIntervalSeq', the same properties for 'standardizeIntervalSeq' hold for
-- this function.  Otherwise, its properties are undefined.
flattenIntervalSeq :: (Ord a, Enum a) => Seq (a, a) -> Seq (a, a)
flattenIntervalSeq = fmap standardizeInterval . go Nothing
  where
    go mayCurr sq = case (mayCurr, viewl sq) of
      (Nothing, EmptyL) -> []
      (Just i, EmptyL) -> [i]
      (Nothing, x :< xs) -> go (Just x) xs
      (Just curr, x :< xs)
        | endRight curr < endLeft x
            && endRight curr < pred (endLeft x) -> curr <| go (Just x) xs
        | otherwise -> go (Just (endLeft curr,
            max (endRight curr) (endRight x))) xs


{- |
Removes excluded members from a list of 'Interval'.  The
following properties hold:

@

removeProperties
  :: (Ord a, Enum a)
  => Seq (a, a)
  -> Seq (a, a)
  -> [Bool]
removeProperties inc exc =

 let r = removeExcludes inc exc
     allExcluded = concatMap members exc
     allIncluded = concatMap members inc
     allResults = concatMap members r
 in [
   -- intervals remain in original order
   allResults == filter (not . (\`elem\` allExcluded)) allIncluded

 -- Every resulting member was a member of the original include list
 , all (\`elem\` allIncluded) allResults

 -- No resulting member is in the exclude list
 , all (not . (\`elem\` allExcluded)) allResults

 -- Every included member that is not in the exclude list is
 -- in the result
 , all (\x -> x \`elem\` allExcluded || x \`elem\` allResults)
       allIncluded

 ]
@

-}
removeExcludes
  :: (Ord a, Enum a)
  => Seq (a, a)
  -- ^ Included intervals (not necessarily sorted)
  -> Seq (a, a)
  -- ^ Excluded intervals (not necessarily sorted)
  -> Seq (a, a)
removeExcludes inc = foldr remover inc

remover
  :: (Ord a, Enum a)
  => (a, a)
  -- ^ Remove this interval
  -> Seq (a, a)
  -- ^ From this sequence of intervals
  -> Seq (a, a)
remover ivl = join . fmap squash . fmap (removeInterval ivl)
  where
    squash (Nothing, Nothing) = Seq.empty
    squash (Just x, Nothing) = Seq.singleton x
    squash (Nothing, Just x) = Seq.singleton x
    squash (Just x, Just y) = x <| y <| Seq.empty

-- | Removes a single interval from a single other interval.  Returns
-- a sequence of intervals, which always
removeInterval
  :: (Ord a, Enum a)
  => (a, a)
  -- ^ Remove this interval
  -> (a, a)
  -- ^ From this interval
  -> (Maybe (a, a), Maybe (a, a))
removeInterval ivl oldIvl = (onLeft, onRight)
  where
    onLeft
      | endLeft ivl > endLeft oldIvl =
          Just ( endLeft oldIvl
               , min (pred (endLeft ivl)) (endRight oldIvl))
      | otherwise = Nothing
    onRight
      | endRight ivl < endRight oldIvl =
          Just ( max (succ (endRight ivl)) (endLeft oldIvl)
               , endRight oldIvl)
      | otherwise = Nothing

-- | Runs 'standardizeIntervalSeq' on the 'included' and 'excluded'
-- intervals.
standardizeIntervals
  :: (Ord a, Enum a)
  => Intervals a
  -> Intervals a
standardizeIntervals (Intervals i e)
  = Intervals (standardizeIntervalSeq i) (standardizeIntervalSeq e)

-- | Sorts the intervals using 'standardizeIntervalSeq', and then removes the
-- excludes with 'removeExcludes'.
splitIntervals
  :: (Ord a, Enum a)
  => Intervals a
  -> Seq (a, a)
splitIntervals (Intervals is es)
  = removeExcludes (standardizeIntervalSeq is) es

-- | 'True' if the given element is a member of the 'Intervals'.
inIntervals :: (Enum a, Ord a) => Intervals a -> a -> Bool
inIntervals ivls a = any (inInterval a) . splitIntervals $ ivls

liftSeq :: Lift a => Seq a -> ExpQ
liftSeq sq = case viewl sq of
  EmptyL -> varE 'Seq.empty
  x :< xs -> uInfixE (lift x) (varE '(<|)) (liftSeq xs)

instance Lift a => Lift (Intervals a) where
  lift (Intervals inc exc) = [| Intervals $sqInc $sqExc |]
    where
      sqInc = liftSeq inc
      sqExc = liftSeq exc
