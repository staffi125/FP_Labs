module Form
  ( Form (..)
  ) where

import Data.Time (TimeOfDay, getCurrentTime, utctDay)
import Display (hhmm)
import Model
import Prompt

class Form a where
  formTitle :: a -> String
  fill      :: IO a
  refill    :: a -> IO a
  check     :: a -> Maybe String
  check _ = Nothing

timeRange :: TimeOfDay -> TimeOfDay -> Maybe String
timeRange a b
  | a < b = Nothing
  | otherwise = Just "Час початку має бути раніше за час завершення."

instance Form Classroom where
  formTitle _ = "Дисплейний клас"
  check r = if roomSeats r > 0 then Nothing else Just "Кількість місць має бути додатною."
  fill = Classroom 0 <$> line "Номер аудиторії" <*> line "Корпус" <*> number "Кількість місць"
                     <*> optional "Примітка" Nothing
  refill r = Classroom (roomId r)
    <$> lineOr "Номер аудиторії" (roomNumber r)
    <*> lineOr "Корпус" (roomBuilding r)
    <*> numberOr "Кількість місць" (roomSeats r)
    <*> optional "Примітка" (roomNote r)

instance Form Workstation where
  formTitle _ = "Робоче місце"
  fill = Workstation 0 <$> number "Код аудиторії" <*> line "Інвентарний номер" <*> line "Процесор"
                       <*> number "ОЗП, ГБ" <*> line "ОС" <*> yesNo "Справне"
  refill w = Workstation (stationId w)
    <$> numberOr "Код аудиторії" (stationRoomId w)
    <*> lineOr "Інвентарний номер" (stationInventory w)
    <*> lineOr "Процесор" (stationCpu w)
    <*> numberOr "ОЗП, ГБ" (stationRam w)
    <*> lineOr "ОС" (stationOs w)
    <*> yesNo "Справне"

instance Form Teacher where
  formTitle _ = "Викладач"
  fill = Teacher 0 <$> line "ПІБ" <*> line "Кафедра" <*> optional "E-mail" Nothing
  refill t = Teacher (teacherId t)
    <$> lineOr "ПІБ" (teacherName t)
    <*> lineOr "Кафедра" (teacherDepartment t)
    <*> optional "E-mail" (teacherEmail t)

instance Form User where
  formTitle _ = "Користувач"
  fill = User 0 <$> line "ПІБ" <*> line "Категорія (студент/викладач/лаборант)" <*> line "Група / підрозділ"
                <*> optional "E-mail" Nothing
  refill u = User (userId u)
    <$> lineOr "ПІБ" (userName u)
    <*> lineOr "Категорія" (userCategory u)
    <*> lineOr "Група / підрозділ" (userGroup u)
    <*> optional "E-mail" (userEmail u)

instance Form Lesson where
  formTitle _ = "Планове заняття"
  check l = timeRange (lessonStart l) (lessonEnd l)
  fill = Lesson 0 <$> number "Код аудиторії" <*> number "Код викладача" <*> line "Дисципліна"
                  <*> line "Група" <*> numberIn "День тижня" 1 7 Nothing <*> time "Початок" <*> time "Кінець"
  refill l = Lesson (lessonId l)
    <$> numberOr "Код аудиторії" (lessonRoomId l)
    <*> numberOr "Код викладача" (lessonTeacherId l)
    <*> lineOr "Дисципліна" (lessonSubject l)
    <*> lineOr "Група" (lessonGroup l)
    <*> numberIn "День тижня" 1 7 (Just (lessonWeekday l))
    <*> timeOr "Початок" (hhmm (lessonStart l))
    <*> timeOr "Кінець" (hhmm (lessonEnd l))

instance Form OpenHours where
  formTitle _ = "Час вільного доступу"
  check o = timeRange (openStart o) (openEnd o)
  fill = OpenHours 0 <$> number "Код аудиторії" <*> numberIn "День тижня" 1 7 Nothing
                     <*> time "Початок" <*> time "Кінець"
  refill o = OpenHours (openId o)
    <$> numberOr "Код аудиторії" (openRoomId o)
    <*> numberIn "День тижня" 1 7 (Just (openWeekday o))
    <*> timeOr "Початок" (hhmm (openStart o))
    <*> timeOr "Кінець" (hhmm (openEnd o))

instance Form Session where
  formTitle _ = "Сеанс роботи"
  check s = timeRange (sessionStart s) (sessionEnd s)
  fill = do
    today <- utctDay <$> getCurrentTime
    Session 0 <$> number "Код робочого місця" <*> number "Код користувача" <*> dateOr "Дата" today
              <*> time "Початок" <*> time "Кінець"
  refill s = Session (sessionId s)
    <$> numberOr "Код робочого місця" (sessionStationId s)
    <*> numberOr "Код користувача" (sessionUserId s)
    <*> dateOr "Дата" (sessionDate s)
    <*> timeOr "Початок" (hhmm (sessionStart s))
    <*> timeOr "Кінець" (hhmm (sessionEnd s))
