{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storage
  ( Record (..)
  , listAll
  , findOne
  , create
  , modify
  , remove
  ) where

import Data.Int (Int64)
import Data.List (intercalate)
import Data.String (fromString)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams (QueryParams)
import Database.MySQL.Simple.QueryResults (QueryResults)
import Model

class (QueryResults a, QueryParams a) => Record a where
  table   :: String
  fields  :: [String]
  ordering :: String
  ordering = "id"
  key     :: a -> Int

selectAll :: forall a. Record a => String
selectAll = "SELECT " ++ intercalate ", " ("id" : fields @a) ++ " FROM " ++ table @a

listAll :: forall a. Record a => Connection -> IO [a]
listAll conn = query_ conn (fromString (selectAll @a ++ " ORDER BY " ++ ordering @a))

findOne :: forall a. Record a => Connection -> Int -> IO (Maybe a)
findOne conn k = do
  rows <- query conn (fromString (selectAll @a ++ " WHERE id = ?")) (Only k)
  pure (case rows of
          r : _ -> Just r
          [] -> Nothing)

create :: forall a. Record a => Connection -> a -> IO Int
create conn x = do
  let marks = intercalate ", " (map (const "?") (fields @a))
      sql = "INSERT INTO " ++ table @a ++ " (" ++ intercalate ", " (fields @a) ++ ") VALUES (" ++ marks ++ ")"
  _ <- execute conn (fromString sql) x
  fromIntegral <$> insertID conn

modify :: forall a. Record a => Connection -> a -> IO Int64
modify conn x =
  execute conn (fromString ("UPDATE " ++ table @a ++ " SET " ++ assignments ++ " WHERE id = " ++ show (key x))) x
  where
    assignments = intercalate ", " (map (++ " = ?") (fields @a))

remove :: forall a. Record a => Connection -> Int -> IO Int64
remove conn k = execute conn (fromString ("DELETE FROM " ++ table @a ++ " WHERE id = ?")) (Only k)

instance Record Classroom where
  table = "classrooms"
  fields = ["number", "building", "seats", "note"]
  ordering = "number"
  key = roomId

instance Record Workstation where
  table = "workstations"
  fields = ["classroom_id", "inventory_no", "cpu", "ram_gb", "os", "is_working"]
  ordering = "classroom_id, inventory_no"
  key = stationId

instance Record Teacher where
  table = "teachers"
  fields = ["full_name", "department", "email"]
  ordering = "full_name"
  key = teacherId

instance Record User where
  table = "users"
  fields = ["full_name", "category", "group_name", "email"]
  ordering = "full_name"
  key = userId

instance Record Lesson where
  table = "lessons"
  fields = ["classroom_id", "teacher_id", "subject", "group_name", "weekday", "starts_at", "ends_at"]
  ordering = "weekday, starts_at"
  key = lessonId

instance Record OpenHours where
  table = "open_hours"
  fields = ["classroom_id", "weekday", "starts_at", "ends_at"]
  ordering = "classroom_id, weekday, starts_at"
  key = openId

instance Record Session where
  table = "sessions"
  fields = ["workstation_id", "user_id", "session_date", "starts_at", "ends_at"]
  ordering = "session_date, starts_at"
  key = sessionId
