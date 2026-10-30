module Types
  ( Symbol (..)
  , Word (..)
  , Punctuation (..)
  , Element (..)
  , Sentence (..)
  , Text (..)
  , wordText
  , firstLetter
  , textWords
  ) where

import Prelude hiding (Word)

data Symbol
  = Letter Char
  | Digit Char
  | Apostrophe Char
  | Hyphen
  deriving (Eq, Show)

newtype Word = Word [Symbol]
  deriving (Eq, Show)

data Punctuation
  = Period
  | Comma
  | Colon
  | Semicolon
  | Question
  | Exclamation
  | Dash
  | Bracket Char
  | Quote Char
  | OtherMark Char
  deriving (Eq, Show)

data Element
  = WordElement Word
  | NumberElement String
  | MarkElement Punctuation
  deriving (Eq, Show)

newtype Sentence = Sentence [Element]
  deriving (Eq, Show)

newtype Text = Text [Sentence]
  deriving (Eq, Show)

symbolChar :: Symbol -> Char
symbolChar (Letter c) = c
symbolChar (Digit c) = c
symbolChar (Apostrophe c) = c
symbolChar Hyphen = '-'

wordText :: Word -> String
wordText (Word symbols) = map symbolChar symbols

firstLetter :: Word -> Maybe Char
firstLetter (Word (Letter c : _)) = Just c
firstLetter _ = Nothing

textWords :: Text -> [Word]
textWords (Text sentences) = [w | Sentence elements <- sentences, WordElement w <- elements]
