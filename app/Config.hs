module Config where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.Maybe                 (fromMaybe)
import           Database.Ladder            (Handle (..))
import qualified Database.PostgreSQL.Simple as Postgres
import           System.Environment         (lookupEnv)

data Config = Config { connString :: ByteString } deriving (Show)

defaultConfig :: IO Config
defaultConfig = do
  user <- fromMaybe "ladder" <$> lookupEnv "POSTGRES_USER"
  pass <- fromMaybe "ladder" <$> lookupEnv "POSTGRES_PASSWORD"
  host <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
  port <- (read :: String -> Int) . fromMaybe "5432" <$> lookupEnv "POSTGRES_PORT"
  name <- fromMaybe "ladder" <$> lookupEnv "POSTGRES_NAME"
  return . Config $
    pack $
    "postgresql://" ++ user ++
    ":" ++
    pass ++
    "@" ++
    host ++
    ":" ++
    show port ++
    "/" ++
    name

defaultHandle :: IO Handle
defaultHandle = do
  cs <- connString <$> defaultConfig
  Handle <$> Postgres.connectPostgreSQL cs
