module Day1 where

import Prelude
import Data.Array (catMaybes, foldl, head, last, length, snoc)
import Data.Array as Array
import Data.Int (decimal, fromString, toStringAs)
import Data.Map (delete, fromFoldable, keys, lookup, Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Utils as U


-- Digit mapping
digits :: Map String Int
digits = fromFoldable 
  [ Tuple "one" 1,
    Tuple "two" 2,
    Tuple "three" 3,
    Tuple "four" 4,
    Tuple "five" 5,
    Tuple "six" 6,
    Tuple "seven" 7,
    Tuple "eight" 8,
    Tuple "nine" 9
  ]

-- Map of combinations
-- that can occur in our input
combinedDigits :: Map String Int
combinedDigits = fromFoldable 
  [ Tuple "oneight" 18
  , Tuple "twone" 21
  , Tuple "threeight" 38
  , Tuple "fiveight" 58
  , Tuple "sevenine" 79
  , Tuple "eightwo" 82
  , Tuple "eighthree" 83
  , Tuple "nineight" 98
  ]


-- Splits a string into a list of "chars"
-- Checks only for digits and returns an array of the result
getDigitsFromString :: String -> Array Int
getDigitsFromString s = catMaybes $ map fromString charArr
  where
    charArr = split (Pattern "") s

-- Parses a string for words and numbers 
-- converts them to numbers, e.g. onetwox7threepx -> [1,2,7,3]
-- Respects order (one3one = [1,3,1])
getDigitsFromString2 :: String -> Array Int
getDigitsFromString2 s = do
  let
    combinedStr = replaceDigitsInString s combinedDigits
  getDigitsFromString $ replaceDigitsInString combinedStr digits
  where
    replaceDigitsInString :: String -> Map String Int -> String
    replaceDigitsInString str mp = do
      let
        key = head $ Array.fromFoldable $ keys mp
      case key of
        Just k -> do
          let
            lk = lookup k mp
          case lk of
            Just value -> do
              let
                valueStr = toStringAs decimal value
                newStr = 
                  replaceAll 
                    (Pattern k)
                    (Replacement valueStr)
                    str
                updatedMap = delete k mp
              replaceDigitsInString newStr updatedMap
            Nothing -> str
        Nothing -> str

-- Get a two digit number
-- from the first and the last number of an array.
-- If there is only one number 
-- the same number is used for the last number e.g. [1] -> 11
-- Empty list results in 0
getTwoDigitNumber :: Array Int -> Int
getTwoDigitNumber arr =
  case length arr of
    0 -> 0
    1 -> getTwoDigitNumber $ snoc arr fst
    _ -> fromMaybe 0 $ fromString (show fst <> show lst)
  where
    fst = fromMaybe 0 $ head arr
    lst = fromMaybe 0 $ last arr

day1 :: Effect Unit
day1 = do
  log "DAY 1"
  content <- U.readFile "./inputs/day1.txt"
  let 
    allLines = lines content
    allDigitLines = map getDigitsFromString allLines
    twoDigits = map getTwoDigitNumber allDigitLines

  log "Phase 1 answer:"
  logShow $ foldl (+) 0 twoDigits
  
  let
    allDigitsFromWords = map getDigitsFromString2 allLines
    twoDigitsFromWords = map getTwoDigitNumber allDigitsFromWords
  
  log "Phase 2 answer:"
  logShow $ foldl (+) 0 twoDigitsFromWords
