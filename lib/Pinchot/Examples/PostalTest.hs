{-# LANGUAGE OverloadedStrings, OverloadedLists, RecursiveDo #-}
module Pinchot.Examples.PostalTest where

import Pinchot

import Data.Monoid ((<>))

-- | A grammar for simple U.S. postal addresses.  This example would never
-- hold up to real-world usage but it gives you a flavor of how
-- Pinchot works.
--
-- The grammar is ambiguous.
postal :: Pinchot Char (Rule Char)
postal = mdo
  digit <- terminal "Digit" (include '0' '9') <?> "digit from 0 to 9"
  digits <- list1 digit
  letter <- terminal "Letter" (include 'a' 'z' <> include 'A' 'Z')
    <?> "letter from A to Z"
  north <- terminal "North" (solo 'N')
  south <- terminal "South" (solo 'S')
  east <- terminal "East" (solo 'E')
  west <- terminal "West" (solo 'W')
  direction <- union "Direction" [north, south, east, west]
  street <- terminalSeq "Street" ['S', 't']
  avenue <- terminalSeq "Avenue" ['A', 'v', 'e']
  way <- terminalSeq "Way" ['W', 'a', 'y']
  boulevard <- terminalSeq "Boulevard" ['B', 'l', 'v', 'd']
  suffix <- union "Suffix" [street, avenue, way, boulevard]
  space <- terminal "Space" (solo ' ')
  comma <- terminal "Comma" (solo ',')

  -- You could do this with 'list' but this demonstrates how to write
  -- a recurvsive rule.

  -- TODO not working.  Is probably getting stuck in infinite loop.
  letters <- nonTerminal "Letters"
    [ ("NoLetter", [])
    , ("ConsLetter", [letter, letters])
    ]

  -- Named "PostalWord" to avoid clash with Prelude.Word
  -- TODO not working
  word <- record "PostalWord" [letter, letters]
  preSpacedWord <- record "PreSpacedWord" [space, word]
  preSpacedWords <- list preSpacedWord
  words <- record "Words" [word, preSpacedWords]

  number <- wrap "Number" digits
  streetName <- wrap "StreetName" words
  city <- wrap "City" words
  state <- wrap "State" word
  zipCode <- wrap "ZipCode" digits
  directionSpace <- record "DirectionSpace" [direction, space]
  spaceSuffix <- record "SpaceSuffix" [space, suffix]
  optDirection <- option directionSpace
  optSuffix <- option spaceSuffix
  address <- record "Address"
    [ number, space, optDirection, streetName, optSuffix,
      comma, space, city, comma, space, state, space, zipCode
    ]
  return digit
