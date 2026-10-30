module Lexer
  ( squeezeSpaces
  , parseText
  ) where

import Data.Char (isAlpha, isDigit, isSpace)
import Prelude hiding (Word)
import Types

squeezeSpaces :: String -> String
squeezeSpaces = unlines . map squeezeLine . lines
  where
    squeezeLine = unwords . words . map (\c -> if c == '\t' then ' ' else c)

isApostrophe :: Char -> Bool
isApostrophe c = c `elem` "'’ʼ"

punctuation :: Char -> Punctuation
punctuation c = case c of
  '.' -> Period
  ',' -> Comma
  ':' -> Colon
  ';' -> Semicolon
  '?' -> Question
  '!' -> Exclamation
  '—' -> Dash
  '–' -> Dash
  _ | c `elem` "()[]{}" -> Bracket c
    | c `elem` "\"«»“”" -> Quote c
    | otherwise -> OtherMark c

endsSentence :: Punctuation -> Bool
endsSentence p = p `elem` [Period, Question, Exclamation]

readWord :: String -> ([Symbol], String)
readWord (c : rest)
  | isAlpha c = cons (Letter c) (readWord rest)
  | isDigit c = cons (Digit c) (readWord rest)
readWord (c : d : rest)
  | isApostrophe c && isAlpha d = cons (Apostrophe c) (readWord (d : rest))
  | c == '-' && isAlpha d = cons Hyphen (readWord (d : rest))
readWord rest = ([], rest)

cons :: a -> ([a], b) -> ([a], b)
cons x (xs, r) = (x : xs, r)

elements :: String -> [Element]
elements [] = []
elements s@(c : rest)
  | isSpace c = elements rest
  | isAlpha c = let (w, r) = readWord s in WordElement (Word w) : elements r
  | isDigit c = let (d, r) = span isDigit s in NumberElement d : elements r
  | otherwise = MarkElement (punctuation c) : elements rest

sentences :: [Element] -> [Sentence]
sentences [] = []
sentences xs = case break isEnd xs of
  (body, []) -> [Sentence body]
  (body, rest) ->
    let (marks, after) = span isEnd rest
     in Sentence (body ++ marks) : sentences after
  where
    isEnd (MarkElement p) = endsSentence p
    isEnd _ = False

parseText :: String -> Text
parseText = Text . sentences . elements . squeezeSpaces
