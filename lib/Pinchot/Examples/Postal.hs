{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Pinchot.Examples.Postal where

import Pinchot
import Data.Monoid ((<>))

rDigit = terminal "Digit" (include '0' '9') <?> "digit from 0 to 9"

rDigits = plus rDigit

rLetter = terminal "Letter" (include 'a' 'z' <> include 'A' 'Z')
  <?> "letter from A to Z"

rNorth = terminal "North" (solo 'N')

rSouth = terminal "South" (solo 'S')

rEast = terminal "East" (solo 'E')

rWest = terminal "West" (solo 'W')

rDirection = union "Direction" [rNorth, rSouth, rEast, rWest]

rStreet = terminals "Street" ['S', 't']

rAvenue = terminals "Avenue" ['A', 'v', 'e']

rWay = terminals "Way" ['W', 'a', 'y']

rBoulevard = terminals "Boulevard" ['B', 'l', 'v', 'd']

rSuffix = union "Suffix" [rStreet, rAvenue, rWay, rBoulevard]

rSpace = terminal "Space" (solo ' ')

rComma = terminal "Comma" (solo ',')

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

rZipCode = wrap "ZipCode" rWords

rDirectionSpace = record "DirectionSpace" [rDirection, rSpace]

rSpaceSuffix = record "SpaceSuffix" [rSpace, rSuffix]

rOptDirection = opt rDirection

rOptSuffix = opt rSpaceSuffix

rAddress = record "Address" [rNumber, rSpace, rOptDirection, rStreetName,
  rOptSuffix, rComma, rSpace, rCity, rSpace, rState, rSpace,
  rZipCode]
