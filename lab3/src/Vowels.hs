module Vowels
  ( isVowel
  , alphabetIndex
  , vowelWordsByInitial
  ) where

import Data.Char (toLower, toUpper)
import Data.List (elemIndex, groupBy, nubBy, sortOn)
import Data.Maybe (fromMaybe)
import Prelude hiding (Word)
import Types

ukrainianAlphabet :: String
ukrainianAlphabet = "абвгґдеєжзиіїйклмнопрстуфхцчшщьюя"

latinAlphabet :: String
latinAlphabet = "abcdefghijklmnopqrstuvwxyz"

vowels :: String
vowels = "аеєиіїоуюя" ++ "aeiou"

isVowel :: Char -> Bool
isVowel c = toLower c `elem` vowels

alphabetIndex :: Char -> Int
alphabetIndex c =
  fromMaybe (1000 + fromEnum lower) $
    ((+ 100) <$> elemIndex lower ukrainianAlphabet) `orElse` elemIndex lower latinAlphabet
  where
    lower = toLower c
    orElse (Just x) _ = Just x
    orElse Nothing y = y

startsWithVowel :: Word -> Bool
startsWithVowel = maybe False isVowel . firstLetter

initialKey :: String -> Int
initialKey = alphabetIndex . head

vowelWordsByInitial :: Text -> [(Char, [String])]
vowelWordsByInitial text =
  [ (toUpper (head (head group)), group)
  | group <- groupBy (\a b -> initialKey a == initialKey b) sorted
  ]
  where
    found = map (map toLower . wordText) (filter startsWithVowel (textWords text))
    unique = nubBy (==) found
    sorted = sortOn initialKey unique
