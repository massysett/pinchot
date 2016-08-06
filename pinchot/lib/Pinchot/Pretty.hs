-- | Pretty printing.

module Pinchot.Pretty where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence.NonEmpty (NonEmptySeq(NonEmptySeq))
import Text.Show.Pretty (Value)
import qualified Text.Show.Pretty as Pretty

-- | Prettify a 'Seq'.
prettySeq :: (a -> Value) -> Seq a -> Value
prettySeq f
  = Pretty.Con "Seq"
  . (:[])
  . Pretty.List
  . fmap f
  . toList

-- | Prettify a 'NonEmptySeq'.
prettyNonEmptySeq :: (a -> Value) -> NonEmptySeq a -> Value
prettyNonEmptySeq f (NonEmptySeq a1 as)
  = Pretty.Rec "NonEmptySeq"
               [("_fore", f a1), ("_aft", prettySeq f as)]

-- | Prettify a 'Maybe'.
prettyMaybe
  :: (a -> Value)
  -> Maybe a
  -> Value
prettyMaybe _ Nothing = Pretty.Con "Nothing" []
prettyMaybe f (Just a) = Pretty.Con "Just" [f a]
