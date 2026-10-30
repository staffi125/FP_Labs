module Model
  ( Classroom (..)
  , Workstation (..)
  , Teacher (..)
  , User (..)
  , Lesson (..)
  , OpenHours (..)
  , Session (..)
  , weekdayName
  ) where

import Data.Time (Day, TimeOfDay)
import Database.MySQL.Simple.QueryParams (QueryParams (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))

data Classroom = Classroom
  { roomId       :: Int
  , roomNumber   :: String
  , roomBuilding :: String
  , roomSeats    :: Int
  , roomNote     :: Maybe String
  }

data Workstation = Workstation
  { stationId        :: Int
  , stationRoomId    :: Int
  , stationInventory :: String
  , stationCpu       :: String
  , stationRam       :: Int
  , stationOs        :: String
  , stationWorking   :: Bool
  }

data Teacher = Teacher
  { teacherId         :: Int
  , teacherName       :: String
  , teacherDepartment :: String
  , teacherEmail      :: Maybe String
  }

data User = User
  { userId       :: Int
  , userName     :: String
  , userCategory :: String
  , userGroup    :: String
  , userEmail    :: Maybe String
  }

data Lesson = Lesson
  { lessonId        :: Int
  , lessonRoomId    :: Int
  , lessonTeacherId :: Int
  , lessonSubject   :: String
  , lessonGroup     :: String
  , lessonWeekday   :: Int
  , lessonStart     :: TimeOfDay
  , lessonEnd       :: TimeOfDay
  }

data OpenHours = OpenHours
  { openId      :: Int
  , openRoomId  :: Int
  , openWeekday :: Int
  , openStart   :: TimeOfDay
  , openEnd     :: TimeOfDay
  }

data Session = Session
  { sessionId        :: Int
  , sessionStationId :: Int
  , sessionUserId    :: Int
  , sessionDate      :: Day
  , sessionStart     :: TimeOfDay
  , sessionEnd       :: TimeOfDay
  }

weekdayName :: Int -> String
weekdayName d = case d of
  1 -> "понеділок"
  2 -> "вівторок"
  3 -> "середа"
  4 -> "четвер"
  5 -> "п'ятниця"
  6 -> "субота"
  7 -> "неділя"
  _ -> "?"

instance QueryResults Classroom where
  convertResults fs vs = let (a, b, c, d, e) = convertResults fs vs in Classroom a b c d e

instance QueryParams Classroom where
  renderParams r = renderParams (roomNumber r, roomBuilding r, roomSeats r, roomNote r)

instance QueryResults Workstation where
  convertResults fs vs = let (a, b, c, d, e, f, g) = convertResults fs vs in Workstation a b c d e f g

instance QueryParams Workstation where
  renderParams w =
    renderParams (stationRoomId w, stationInventory w, stationCpu w, stationRam w, stationOs w, stationWorking w)

instance QueryResults Teacher where
  convertResults fs vs = let (a, b, c, d) = convertResults fs vs in Teacher a b c d

instance QueryParams Teacher where
  renderParams t = renderParams (teacherName t, teacherDepartment t, teacherEmail t)

instance QueryResults User where
  convertResults fs vs = let (a, b, c, d, e) = convertResults fs vs in User a b c d e

instance QueryParams User where
  renderParams u = renderParams (userName u, userCategory u, userGroup u, userEmail u)

instance QueryResults Lesson where
  convertResults fs vs = let (a, b, c, d, e, f, g, h) = convertResults fs vs in Lesson a b c d e f g h

instance QueryParams Lesson where
  renderParams l =
    renderParams ( lessonRoomId l, lessonTeacherId l, lessonSubject l, lessonGroup l
                 , lessonWeekday l, lessonStart l, lessonEnd l )

instance QueryResults OpenHours where
  convertResults fs vs = let (a, b, c, d, e) = convertResults fs vs in OpenHours a b c d e

instance QueryParams OpenHours where
  renderParams o = renderParams (openRoomId o, openWeekday o, openStart o, openEnd o)

instance QueryResults Session where
  convertResults fs vs = let (a, b, c, d, e, f) = convertResults fs vs in Session a b c d e f

instance QueryParams Session where
  renderParams s =
    renderParams (sessionStationId s, sessionUserId s, sessionDate s, sessionStart s, sessionEnd s)
