{-# LANGUAGE OverloadedStrings #-}

module Reports
  ( roomTimetable
  , teacherTimetable
  , groupTimetable
  , statusAt
  , conflictsFor
  , roomLoad
  , stationLoad
  ) where

import Control.Monad (forM_)
import Data.Time (Day, TimeOfDay)
import Database.MySQL.Simple
import Display (hhmm)
import Model
import Text.Printf (printf)

type LessonRow = (Int, TimeOfDay, TimeOfDay, String, String, String, String)

printLessons :: [LessonRow] -> IO ()
printLessons [] = putStrLn "  (занять немає)"
printLessons rows = forM_ rows $ \(day, a, b, room, subject, grp, teacher) ->
  printf "  %-10s %s–%s  ауд. %-4s %-30s %-8s %s\n" (weekdayName day) (hhmm a) (hhmm b) room subject grp teacher

lessonSelect :: Query
lessonSelect =
  "SELECT l.weekday, l.starts_at, l.ends_at, c.number, l.subject, l.group_name, t.full_name \
  \FROM lessons l JOIN classrooms c ON c.id = l.classroom_id JOIN teachers t ON t.id = l.teacher_id "

roomTimetable :: Connection -> Int -> IO ()
roomTimetable conn room = do
  putStrLn "Планові заняття:"
  printLessons =<< query conn (lessonSelect <> "WHERE l.classroom_id = ? ORDER BY l.weekday, l.starts_at") (Only room)
  putStrLn "Вільний доступ:"
  open <- query conn "SELECT weekday, starts_at, ends_at FROM open_hours WHERE classroom_id = ? \
                     \ORDER BY weekday, starts_at" (Only room)
  if null open
    then putStrLn "  (не передбачено)"
    else forM_ open $ \(day, a, b) -> printf "  %-10s %s–%s\n" (weekdayName day) (hhmm a) (hhmm (b :: TimeOfDay))

teacherTimetable :: Connection -> String -> IO ()
teacherTimetable conn name =
  printLessons =<< query conn (lessonSelect <> "WHERE t.full_name LIKE ? ORDER BY l.weekday, l.starts_at")
                         (Only ("%" ++ name ++ "%"))

groupTimetable :: Connection -> String -> IO ()
groupTimetable conn grp =
  printLessons =<< query conn (lessonSelect <> "WHERE l.group_name LIKE ? ORDER BY l.weekday, l.starts_at")
                         (Only ("%" ++ grp ++ "%"))

statusAt :: Connection -> Int -> TimeOfDay -> IO ()
statusAt conn day moment = do
  rooms <- query_ conn "SELECT id, number FROM classrooms ORDER BY number" :: IO [(Int, String)]
  forM_ rooms $ \(rid, number) -> do
    busy <- query conn "SELECT subject, group_name, ends_at FROM lessons \
                       \WHERE classroom_id = ? AND weekday = ? AND starts_at <= ? AND ends_at > ?"
                       (rid, day, moment, moment)
    open <- query conn "SELECT ends_at FROM open_hours \
                       \WHERE classroom_id = ? AND weekday = ? AND starts_at <= ? AND ends_at > ?"
                       (rid, day, moment, moment)
    let status = case (busy, open) of
          ((subject, grp, end) : _, _) -> "заняття «" ++ subject ++ "», " ++ grp ++ ", до " ++ hhmm end
          ([], Only end : _) -> "вільний доступ до " ++ hhmm end
          _ -> "зачинено"
    printf "  ауд. %-4s %s\n" number (status :: String)

conflictsFor :: Connection -> Lesson -> IO [LessonRow]
conflictsFor conn l =
  query conn (lessonSelect <> "WHERE l.id <> ? AND l.weekday = ? AND l.starts_at < ? AND l.ends_at > ? \
                              \AND (l.classroom_id = ? OR l.teacher_id = ?)")
    (lessonId l, lessonWeekday l, lessonEnd l, lessonStart l, lessonRoomId l, lessonTeacherId l)

roomLoad :: Connection -> IO ()
roomLoad conn = do
  rows <- query_ conn
    "SELECT c.number, c.seats, \
    \  (SELECT COUNT(*) FROM workstations w WHERE w.classroom_id = c.id AND w.is_working = 1), \
    \  (SELECT COALESCE(SUM(TIME_TO_SEC(TIMEDIFF(x.ends_at, x.starts_at))), 0) / 3600 FROM lessons x WHERE x.classroom_id = c.id), \
    \  (SELECT COALESCE(SUM(TIME_TO_SEC(TIMEDIFF(x.ends_at, x.starts_at))), 0) / 3600 FROM open_hours x WHERE x.classroom_id = c.id) \
    \FROM classrooms c ORDER BY c.number"
  printf "  %-6s %6s %10s %12s %14s %14s\n" ("Ауд." :: String) ("Місць" :: String) ("Справних" :: String)
    ("Заняття, год" :: String) ("Вільн. дост." :: String) ("Завантаж., %" :: String)
  forM_ rows $ \(number, seats, working, lessonsH, openH) ->
    printf "  %-6s %6d %10d %12.1f %14.1f %14.1f\n" (number :: String) (seats :: Int) (working :: Int)
      (lessonsH :: Double) (openH :: Double) (lessonsH / weekHours * 100)
  printf "  (завантаженість = години занять / %.0f год робочого тижня: пн–сб, 08:00–20:00)\n" weekHours
  where
    weekHours = 72 :: Double

stationLoad :: Connection -> IO ()
stationLoad conn = do
  rows <- query_ conn
    "SELECT w.inventory_no, c.number, w.is_working, COUNT(x.id), \
    \  COALESCE(SUM(TIME_TO_SEC(TIMEDIFF(x.ends_at, x.starts_at))), 0) / 3600, MAX(x.session_date) \
    \FROM workstations w JOIN classrooms c ON c.id = w.classroom_id \
    \LEFT JOIN sessions x ON x.workstation_id = w.id \
    \GROUP BY w.id, w.inventory_no, c.number, w.is_working ORDER BY 5 DESC"
  printf "  %-12s %-6s %-10s %8s %8s  %s\n" ("Місце" :: String) ("Ауд." :: String) ("Стан" :: String)
    ("Сеансів" :: String) ("Годин" :: String) ("Останній сеанс" :: String)
  forM_ rows $ \(inv, number, working, count, hours, lastDay) ->
    printf "  %-12s %-6s %-10s %8d %8.1f  %s\n" (inv :: String) (number :: String)
      (if working then "справне" else "несправне" :: String) (count :: Int) (hours :: Double)
      (maybe "—" show (lastDay :: Maybe Day))
