-- | Pretty printing.

module Pinchot.Pretty where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Sequence.NonEmpty (NonEmptySeq(NonEmptySeq))
import Text.Show.Pretty (Value)
import qualified Text.Show.Pretty as Pretty
import qualified Text.Earley as Earley

-- | Prettify a 'Seq'.
prettySeq :: (a -> Value) -> Seq a -> Value
prettySeq f
  = Pretty.Con "Seq"
  . (:[])
  . Pretty.List
  . fmap f
  . toList

prettyList :: (a -> Value) -> [a] -> Value
prettyList f = Pretty.List . fmap f

-- | Prettify a 'NonEmptySeq'.
prettyNonEmptySeq :: (a -> Value) -> NonEmptySeq a -> Value
prettyNonEmptySeq f (NonEmptySeq a1 as)
  = Pretty.Rec "NonEmptySeq"
               [("_fore", f a1), ("_aft", prettySeq f as)]

-- | Prettify a 'NonEmpty'.
prettyNonEmpty :: (a -> Value) -> NonEmpty a -> Value
prettyNonEmpty f (a1 :| as)
  = Pretty.Con ":|" [(f a1), (Pretty.List (fmap f as))]

-- | Prettify a 'Maybe'.
prettyMaybe
  :: (a -> Value)
  -> Maybe a
  -> Value
prettyMaybe _ Nothing = Pretty.Con "Nothing" []
prettyMaybe f (Just a) = Pretty.Con "Just" [f a]

-- | Prettify a 'Report'.
prettyReport
  :: (e -> Value)
  -> (i -> Value)
  -> Earley.Report e i
  -> Value
prettyReport fe fi (Earley.Report p e i) = Pretty.Rec "Report"
  [ ("position", Pretty.prettyVal p)
  , ("expected", Pretty.List (map fe e))
  , ("unconsumed", fi i)
  ]

-- | Prettify the output of 'Pinchot.Locator.locatedFullParses'.
prettyFullParses
  :: (Pretty.PrettyVal p, Pretty.PrettyVal v)
  => ([p], Earley.Report String (Seq v))
  -> Value
prettyFullParses x = Pretty.Tuple
  [ Pretty.prettyVal . fst $ x
  , prettyReport Pretty.prettyVal (prettySeq Pretty.prettyVal)
      . snd $ x
  ]
