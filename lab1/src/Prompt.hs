module Prompt
  ( line
  , lineOr
  , optional
  , number
  , numberOr
  , numberIn
  , time
  , timeOr
  , date
  , dateOr
  , yesNo
  ) where

import Data.Char (isSpace)
import Data.Time (Day, TimeOfDay, defaultTimeLocale, parseTimeM)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

raw :: String -> IO String
raw label = do
  putStr label
  hFlush stdout
  dropWhile isSpace . reverse . dropWhile isSpace . reverse <$> getLine

retry :: String -> IO a -> IO a
retry message action = putStrLn ("  " ++ message) >> action

line :: String -> IO String
line label = do
  s <- raw (label ++ ": ")
  if null s then retry "Поле не може бути порожнім." (line label) else pure s

lineOr :: String -> String -> IO String
lineOr label current = do
  s <- raw (label ++ " [" ++ current ++ "]: ")
  pure (if null s then current else s)

optional :: String -> Maybe String -> IO (Maybe String)
optional label current = do
  s <- raw (label ++ " [" ++ maybe "—" id current ++ "] (\"-\" — очистити): ")
  pure (case s of
          "" -> current
          "-" -> Nothing
          v -> Just v)

parsed :: (String -> Maybe a) -> String -> String -> Maybe String -> IO a
parsed parse message label current = do
  s <- maybe (line label) (lineOr label) current
  maybe (retry message (parsed parse message label current)) pure (parse s)

number :: String -> IO Int
number label = parsed readMaybe "Потрібне ціле число." label Nothing

numberOr :: String -> Int -> IO Int
numberOr label current = parsed readMaybe "Потрібне ціле число." label (Just (show current))

numberIn :: String -> Int -> Int -> Maybe Int -> IO Int
numberIn label lo hi current = do
  n <- maybe (number full) (numberOr full) current
  if n >= lo && n <= hi then pure n else retry "Значення поза межами." (numberIn label lo hi current)
  where
    full = label ++ " (" ++ show lo ++ "–" ++ show hi ++ ")"

parseTime :: String -> Maybe TimeOfDay
parseTime = parseTimeM True defaultTimeLocale "%H:%M"

time :: String -> IO TimeOfDay
time label = parsed parseTime "Формат часу: ГГ:ХХ." (label ++ " (ГГ:ХХ)") Nothing

timeOr :: String -> String -> IO TimeOfDay
timeOr label current = parsed parseTime "Формат часу: ГГ:ХХ." (label ++ " (ГГ:ХХ)") (Just current)

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

date :: String -> IO Day
date label = parsed parseDate "Формат дати: РРРР-ММ-ДД." (label ++ " (РРРР-ММ-ДД)") Nothing

dateOr :: String -> Day -> IO Day
dateOr label current = parsed parseDate "Формат дати: РРРР-ММ-ДД." (label ++ " (РРРР-ММ-ДД)") (Just (show current))

yesNo :: String -> IO Bool
yesNo label = do
  s <- raw (label ++ " (так/ні): ")
  case s of
    "так" -> pure True
    "ні" -> pure False
    _ -> retry "Введіть «так» або «ні»." (yesNo label)
