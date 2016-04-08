{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Pinchot.Locator where

import qualified Data.ListLike as ListLike
import Pinchot.Types
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- | Advances the location for 'Char' values.  Tabs advance to the
-- next eight-column tab stop; newlines advance to the next line and
-- reset the column number to 1.  All other characters advance the
-- column by 1.
advanceChar :: Char -> Loc -> Loc
advanceChar c (Loc !lin !col !pos)
  | c == '\n' = Loc (lin + 1) 1 (pos + 1)
  | c == '\t' = Loc lin (col + 8 - ((col - 1) `mod` 8)) (pos + 1)
  | otherwise = Loc lin (col + 1) (pos + 1)

-- | Takes any ListLike value based on 'Char' (@Seq@, @Text@,
-- @String@, etc.) and creates a 'Seq' which pairs each 'Char' with
-- its location.
locations
  :: ListLike.FoldableLL full Char
  => full
  -> Seq (Char, Loc)
locations = fst . ListLike.foldl' f (Seq.empty, Loc 1 1 1)
  where
    f (!sq, !loc) c = (sq |> (c, loc), advanceChar c loc)

-- | Breaks a ListLike into a 'Seq' but does not assign locations.
noLocations
  :: ListLike.FoldableLL full item
  => full
  -> Seq (item, ())
noLocations = ListLike.foldl' f Seq.empty
  where
    f !sq c = sq |> (c, ())
