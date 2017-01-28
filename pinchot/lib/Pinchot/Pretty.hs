-- | Pretty printing.

module Pinchot.Pretty where

import Data.List.NonEmpty (NonEmpty((:|)))
import Text.Show.Pretty (Value)
import qualified Text.Show.Pretty as Pretty
import qualified Text.Earley as Earley

prettyList :: (a -> Value) -> [a] -> Value
prettyList f = Pretty.List . fmap f

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
  => ([p], Earley.Report String [v])
  -> Value
prettyFullParses x = Pretty.Tuple
  [ Pretty.prettyVal . fst $ x
  , prettyReport Pretty.prettyVal (prettyList Pretty.prettyVal)
      . snd $ x
  ]
