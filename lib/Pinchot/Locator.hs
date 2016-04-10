{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Pinchot.Locator where

import Pinchot.Types

import qualified Data.ListLike as ListLike
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Text.Earley as Earley

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
-- its location.  Example: 'locatedFullParses'.
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

-- | Obtains all full Earley parses from a given input string, after
-- assigning a location to every 'Char'.  Example:
-- 'Pinchot.Examples.Newman.address'.
locatedFullParses
  :: ListLike.FoldableLL full Char
  => (forall r. Earley.Grammar r (Earley.Prod r String (Char, Loc) (p Char Loc)))
  -- ^ Earley grammar with production that you want to parse.
  -> full
  -- ^ Source text, e.g. 'String', 'Data.Text', etc.
  -> ([p Char Loc], Earley.Report String (Seq (Char, Loc)))
  -- ^ A list of successful parses that when to the end of the
  -- source string, along with the Earley report showing possible
  -- errors.
locatedFullParses g
  = Earley.fullParses (Earley.parser g)
  . locations
