{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Database.MySQL.Simple (Connection, close)
import Connection (connectDb)
import Display
import Form
import Model
import Prompt
import Reports
import Storage
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stdin, stdout, utf8)
import Text.Printf (printf)

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  hSetBuffering stdout NoBuffering
  conn <- connectDb
  mainMenu conn
  close conn

choose :: String -> [String] -> IO Int
choose header options = do
  putStrLn ("\n=== " ++ header ++ " ===")
  mapM_ (\(i, o) -> printf "%2d. %s\n" i o) (zip [1 :: Int ..] options)
  putStrLn " 0. Назад / вихід"
  numberIn "Пункт" 0 (length options) Nothing

guarded :: IO () -> IO ()
guarded action = do
  result <- try action
  either (\(e :: SomeException) -> putStrLn ("Помилка: " ++ displayException e)) pure result

mainMenu :: Connection -> IO ()
mainMenu conn = do
  n <- choose "Розклад роботи дисплейних класів факультету"
    [ "Розклад аудиторії"
    , "Розклад викладача"
    , "Розклад групи"
    , "Стан аудиторій на заданий день і час"
    , "Завантаженість дисплейних класів"
    , "Завантаженість робочих місць"
    , "Дисплейні класи"
    , "Робочі місця"
    , "Викладачі"
    , "Користувачі"
    , "Планові заняття"
    , "Час вільного доступу"
    , "Сеанси роботи на робочих місцях"
    ]
  let continue action = guarded action >> mainMenu conn
  case n of
    1 -> continue $ do
      showAll =<< (listAll conn :: IO [Classroom])
      roomTimetable conn =<< number "Код аудиторії"
    2 -> continue (teacherTimetable conn =<< line "Фрагмент ПІБ викладача")
    3 -> continue (groupTimetable conn =<< line "Група")
    4 -> continue $ do
      day <- numberIn "День тижня" 1 7 Nothing
      moment <- time "Час"
      statusAt conn day moment
    5 -> continue (roomLoad conn)
    6 -> continue (stationLoad conn)
    7 -> manage @Classroom conn "Дисплейні класи" >> mainMenu conn
    8 -> manage @Workstation conn "Робочі місця" >> mainMenu conn
    9 -> manage @Teacher conn "Викладачі" >> mainMenu conn
    10 -> manage @User conn "Користувачі" >> mainMenu conn
    11 -> manage @Lesson conn "Планові заняття" >> mainMenu conn
    12 -> manage @OpenHours conn "Час вільного доступу" >> mainMenu conn
    13 -> manage @Session conn "Сеанси роботи" >> mainMenu conn
    _ -> putStrLn "Роботу завершено."

class Conflicts a where
  conflicts :: Connection -> a -> IO Bool
  conflicts _ _ = pure True

instance Conflicts Classroom
instance Conflicts Workstation
instance Conflicts Teacher
instance Conflicts User
instance Conflicts OpenHours
instance Conflicts Session

instance Conflicts Lesson where
  conflicts conn l = do
    found <- conflictsFor conn l
    if null found
      then pure True
      else do
        putStrLn "Увага: заняття перетинається за часом (та сама аудиторія або викладач):"
        mapM_ (\(_, a, b, room, subject, grp, teacher) ->
                 printf "  %s–%s ауд. %s %s (%s), %s\n" (hhmm a) (hhmm b) room subject grp teacher) found
        yesNo "Все одно зберегти"

save :: forall a. (Form a, Conflicts a) => Connection -> a -> IO Bool
save conn x = case check x of
  Just problem -> putStrLn problem >> pure False
  Nothing -> conflicts conn x

manage :: forall a. (Record a, Form a, Display a, Conflicts a) => Connection -> String -> IO ()
manage conn header = do
  n <- choose header ["Переглянути всі", "Додати", "Змінити", "Видалити"]
  let again action = guarded action >> manage @a conn header
  case n of
    1 -> again (showAll =<< (listAll conn :: IO [a]))
    2 -> again $ do
      x <- fill :: IO a
      ok <- save conn x
      if ok
        then create conn x >>= \k -> putStrLn (formTitle x ++ ": додано запис #" ++ show k)
        else putStrLn "Запис не збережено."
    3 -> again $ do
      k <- number "Код запису"
      found <- findOne conn k
      case found of
        Nothing -> putStrLn "Запис не знайдено."
        Just (x :: a) -> do
          putStrLn ("Поточні дані: " ++ display x)
          y <- refill x
          ok <- save conn y
          if ok then void (modify conn y) >> putStrLn "Зміни збережено." else putStrLn "Зміни не збережено."
    4 -> again $ do
      k <- number "Код запису"
      sure <- yesNo "Видалити запис"
      if sure
        then remove @a conn k >>= \c -> putStrLn (if c > 0 then "Видалено." else "Запис не знайдено.")
        else putStrLn "Скасовано."
    _ -> pure ()
