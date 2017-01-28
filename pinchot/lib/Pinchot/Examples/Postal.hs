{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module contains a context-free grammar for U.S. postal
-- addresses.  It would never hold up to real-world use but it gives
-- you a good idea of how to write grammars in Pinchot.
--
-- The grammar is ambiguous.
--
-- There are no signatures; the type of every declaration in the
-- module is 'Rule' 'Char'.
module Pinchot.Examples.Postal where

import Pinchot

rDigit = terminal "Digit" [|| \x -> x >= '0' && x <= '9' ||]
  <?> "digit from 0 to 9"

rDigits = plus rDigit

rLetter = terminal "Letter" [|| \x -> (x >= 'a' && x <= 'z')
                                   || (x >= 'A' && x <= 'Z') ||]
  <?> "letter from A to Z"

rNorth = terminal "North" [|| (== 'N') ||]

rSouth = terminal "South" [|| (== 'S') ||]

rEast = terminal "East" [|| (== 'E') ||]

rWest = terminal "West" [|| (== 'W') ||]

rNE = series "NE" "NE"

rNW = series "NW" "NW"

rSW = series "SW" "SW"

rSE = series "SE" "SE"

rDirection = union "Direction"
  [rNorth, rSouth, rEast, rWest, rNE, rNW, rSE, rSW]

rStreet = series "Street" "St"

rAvenue = series "Avenue" "Ave"

rWay = series "Way" "Way"

rBoulevard = series "Boulevard" "Blvd"

rSuffix = union "Suffix" [rStreet, rAvenue, rWay, rBoulevard]

rSpace = terminal "Space" [|| (== ' ') ||]

rComma = terminal "Comma" [|| (== ',') ||]

rNewline = terminal "Newline" [|| (== '\n') ||]

rCommaSpace = record "CommaSpace" [rComma, rSpace]

rSeparator = union "Separator" [rCommaSpace, rNewline]

rLetters = nonTerminal "Letters"
  [ ("NoLetter", [])
  , ("ConsLetter", [rLetter, rLetters])
  ]

-- Named "PostalWord" to avoid clash with Prelude.Word
rPostalWord = record "PostalWord" [rLetter, rLetters]

rPreSpacedWord = record "PreSpacedWord" [rSpace, rPostalWord]

rPreSpacedWords = star rPreSpacedWord

rWords = record "Words" [rPostalWord, rPreSpacedWords]

rNumber = wrap "Number" rDigits

rStreetName = wrap "StreetName" rWords

rCity = wrap "City" rWords

rState = wrap "State" rWords

rZipCode = record "ZipCode" [rDigit, rDigit, rDigit, rDigit, rDigit]

rDirectionSpace = record "DirectionSpace" [rDirection, rSpace]

rSpaceSuffix = record "SpaceSuffix" [rSpace, rSuffix]

rOptDirection = opt rDirectionSpace

rOptSuffix = opt rSpaceSuffix

rOptNewline = opt rNewline

rNameLine = record "NameLine" [rWords, rSeparator]

rStreetLine = record "StreetLine" [rNumber, rSpace, rOptDirection,
  rStreetName, rOptSuffix, rSeparator]

rCityLine = record "CityLine" [rCity, rComma, rSpace, rState,
  rSpace, rZipCode, rOptNewline]

rAddress = record "Address" [rNameLine, rStreetLine, rCityLine]
