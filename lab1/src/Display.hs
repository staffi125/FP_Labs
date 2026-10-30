module Display
  ( Display (..)
  , showAll
  , hhmm
  ) where

import Data.Time (TimeOfDay (..))
import Model
import Text.Printf (printf)

class Display a where
  display :: a -> String

hhmm :: TimeOfDay -> String
hhmm t = printf "%02d:%02d" (todHour t) (todMin t)

interval :: TimeOfDay -> TimeOfDay -> String
interval a b = hhmm a ++ "–" ++ hhmm b

instance Display Classroom where
  display r =
    printf "#%d  ауд. %s (%s), місць: %d%s" (roomId r) (roomNumber r) (roomBuilding r) (roomSeats r)
      (maybe "" ("; " ++) (roomNote r))

instance Display Workstation where
  display w =
    printf "#%d  %s в аудиторії #%d: %s, %d ГБ, %s — %s" (stationId w) (stationInventory w) (stationRoomId w)
      (stationCpu w) (stationRam w) (stationOs w) (if stationWorking w then "справне" else "несправне")

instance Display Teacher where
  display t = printf "#%d  %s (%s)%s" (teacherId t) (teacherName t) (teacherDepartment t)
    (maybe "" (", " ++) (teacherEmail t))

instance Display User where
  display u = printf "#%d  %s — %s, %s%s" (userId u) (userName u) (userCategory u) (userGroup u)
    (maybe "" (", " ++) (userEmail u))

instance Display Lesson where
  display l =
    printf "#%d  %s %s  ауд. #%d  %s, група %s, викладач #%d" (lessonId l) (weekdayName (lessonWeekday l))
      (interval (lessonStart l) (lessonEnd l)) (lessonRoomId l) (lessonSubject l) (lessonGroup l) (lessonTeacherId l)

instance Display OpenHours where
  display o =
    printf "#%d  ауд. #%d  %s %s" (openId o) (openRoomId o) (weekdayName (openWeekday o))
      (interval (openStart o) (openEnd o))

instance Display Session where
  display s =
    printf "#%d  місце #%d, користувач #%d, %s %s" (sessionId s) (sessionStationId s) (sessionUserId s)
      (show (sessionDate s)) (interval (sessionStart s) (sessionEnd s))

showAll :: Display a => [a] -> IO ()
showAll [] = putStrLn "  (записів немає)"
showAll xs = mapM_ (putStrLn . ("  " ++) . display) xs
