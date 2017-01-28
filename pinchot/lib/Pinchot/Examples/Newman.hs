-- | This module provides a simple example of use of everything in
-- Pinchot.  'address' parses a postal address from a string and
-- prints a simple report showing some of the elements of the
-- address and their locations.
module Pinchot.Examples.Newman where

import Pinchot
import Pinchot.Pretty

import Pinchot.Examples.Earley
import Pinchot.Examples.SyntaxTrees
import Pinchot.Examples.Terminalize
import Pinchot.Examples.RulesToOptics

import qualified Control.Lens as Lens
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NE
import qualified Text.Earley as Earley
import qualified Text.Show.Pretty as Pretty

-- | Formats a 'Loc' for nice on-screen display.
labelLoc :: Loc -> String
labelLoc (Loc l c p)
  = "(line: " ++ show l ++ " col: " ++ show c ++ " pos: "
  ++ show p ++ ")"

-- | Labels a single field, where the field may or may not appear in
-- a parsed result.
labelOpt :: String -> [(Char, Loc)] -> String
labelOpt l sq
  = l ++ ": " ++ show (fmap fst $ sq)
  ++ " " ++ loc ++ "\n"
  where
    loc = case Lens.uncons sq of
      Nothing -> "(no location)"
      Just ((_, loc), _) -> labelLoc loc

-- | Labels a single field, where the field will always appear in a
-- parsed result.
labelNE :: String -> NonEmpty (Char, Loc) -> String
labelNE l sq
  = l ++ ": " ++ show (toList . fmap fst $ sq)
  ++ " " ++ loc ++ "\n"
  where
    loc = labelLoc . snd . NE.head $ sq

-- | Formats a single 'Address' for nice on-screen display.
showAddress :: Address Char Loc -> String
showAddress a = name ++ street ++ city
  where
    name = labelNE "Name" . t'Words
      . _r'NameLine'0'Words . _r'Address'0'NameLine $ a
    street = number ++ pre ++ streetName ++ suf
      where
        number = labelNE "Number" . t'Number . _r'StreetLine'0'Number
          . _r'Address'1'StreetLine $ a

        pre = labelOpt "Direction prefix"
          . maybe [] NE.toList
          . Lens.preview (r'Address'1'StreetLine
                          . r'StreetLine'2'DirectionSpace'Opt
                          . Lens._Wrapped'
                          . Lens._Just
                          . r'DirectionSpace'0'Direction
                          . Lens.to t'Direction)
          $ a

        streetName = labelNE "Street"
          . t'StreetName
          . _r'StreetLine'3'StreetName
          . _r'Address'1'StreetLine
          $ a

        suf = labelOpt "Street suffix"
          . maybe [] NE.toList
          . Lens.preview (r'Address'1'StreetLine
                          . r'StreetLine'4'SpaceSuffix'Opt
                          . Lens._Wrapped'
                          . Lens._Just
                          . r'SpaceSuffix'1'Suffix
                          . Lens.to t'Suffix)
          $ a
    city = cty ++ st ++ zip
      where
        cty = labelNE "City"
          . t'City
          . _r'CityLine'0'City
          . _r'Address'2'CityLine
          $ a

        st = labelNE "State"
          . t'State
          . _r'CityLine'3'State
          . _r'Address'2'CityLine
          $ a

        zip = labelNE "Zip"
          . t'ZipCode
          . _r'CityLine'5'ZipCode
          . _r'Address'2'CityLine
          $ a

-- | Formats successful 'Address' parses and the 'Earley.Report' for
-- nice on-screen display.
showParseResult
  :: ([Address Char Loc], Earley.Report String [(Char, Loc)])
  -> String
showParseResult (addresses, report) = addresses' ++ "\n" ++ report'
  where
    addresses' = ("Full parses:\n\n" ++)
      . concat . intersperse "---\n" . map showAddress
      $ addresses
    report' = ("Earley report:\n\n" ++) . show
      $ report { Earley.unconsumed = fmap fst
      . Earley.unconsumed $ report }

-- | Parse an address and print the resulting report.  Good for use
-- in GHCi.
address :: String -> IO ()
address = putStrLn . showParseResult . locatedFullParses addressGrammar

-- | Like 'address' but uses 'Pretty.valToStr'.
addressPretty :: String -> IO ()
addressPretty = putStrLn . Pretty.valToStr
  . prettyFullParses . locatedFullParses addressGrammar

-- | Read an address from a file and print the resulting report.
-- Good for use in GHCi.
addressFromFile
  :: String
  -- ^ Filename
  -> IO ()
addressFromFile fn = readFile fn >>= address
