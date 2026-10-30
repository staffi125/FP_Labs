module Connection
  ( connectDb
  ) where

import Data.Maybe (fromMaybe)
import Database.MySQL.Base.Types (Option (CharsetName))
import Database.MySQL.Simple
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

connectDb :: IO Connection
connectDb = do
  let setting name def = fromMaybe def <$> lookupEnv name
  host <- setting "DB_HOST" "localhost"
  port <- setting "DB_PORT" "3306"
  user <- setting "DB_USER" "root"
  password <- setting "DB_PASSWORD" ""
  database <- setting "DB_NAME" "display_classes"
  connect defaultConnectInfo
    { connectHost = host
    , connectPort = fromMaybe 3306 (readMaybe port)
    , connectUser = user
    , connectPassword = password
    , connectDatabase = database
    , connectOptions = [CharsetName "utf8mb4"]
    }
