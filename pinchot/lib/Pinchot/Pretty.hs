-- | Pretty printing.

module Pinchot.Pretty where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NonEmptySeq(NonEmptySeq))
import qualified Data.Sequence.NonEmpty as NE
import Text.Show.Pretty (PrettyVal, Value)
import qualified Text.Show.Pretty as Pretty

-- | Prettify a 'Seq'.
prettySeq :: (a -> Value) -> Seq a -> Value
prettySeq f
  = Pretty.Con "Data.Sequence.Seq"
  . (:[])
  . Pretty.List
  . fmap f
  . toList

-- | Prettify a 'NonEmptySeq'.
prettyNonEmptySeq :: (a -> Value) -> NonEmptySeq a -> Value
prettyNonEmptySeq f (NonEmptySeq a1 as)
  = Pretty.Rec "Data.Sequence.NonEmpty.NonEmptySeq"
               [("_fore", f a1), ("_aft", prettySeq f as)]
