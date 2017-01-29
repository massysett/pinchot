{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Pinchot.Locator where

import Pinchot.Types

import Data.List (mapAccumL)
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

-- | Adds locations to a list of characters.
locations :: Traversable t => t Char -> t (Char, Loc)
locations = snd . mapAccumL f (Loc 1 1 1)
  where
    f loc char = (advanceChar char loc, (char, loc))

-- | Takes a list of tokens and assigns empty locations.
noLocations :: Functor f => f a -> f (a, ())
noLocations = fmap (\a -> (a, ()))

-- | Obtains all full Earley parses from a given input string, after
-- assigning a location to every 'Char'.  Example:
-- 'Pinchot.Examples.Newman.address'.
locatedFullParses
  :: (forall r. Earley.Grammar r (Earley.Prod r String (Char, Loc) (p Char Loc)))
  -- ^ Earley grammar with production that you want to parse.
  -> [Char]
  -- ^ Source text
  -> ([p Char Loc], Earley.Report String [(Char, Loc)])
  -- ^ A list of successful parses that when to the end of the
  -- source string, along with the Earley report showing possible
  -- errors.
locatedFullParses g
  = Earley.fullParses (Earley.parser g)
  . locations
