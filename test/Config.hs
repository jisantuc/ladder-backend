module Config (Config (..), defaultHandle, defaultConfig) where

import           Crypto.JOSE.JWK
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.Maybe                 (fromMaybe)
import           Database.Ladder            (Handle (..))
import qualified Database.PostgreSQL.Simple as Postgres
import           Servant.Auth.Server
import           System.Environment         (lookupEnv)

data Config = Config { connString :: ByteString
                     , jwk        :: JWK} deriving (Show)


defaultConfig :: IO Config
defaultConfig = do
  -- db settings
  user <- fromMaybe "ladder" <$> lookupEnv "POSTGRES_USER"
  pass <- fromMaybe "ladder" <$> lookupEnv "POSTGRES_PASSWORD"
  host <- fromMaybe "localhost" <$> lookupEnv "POSTGRES_HOST"
  port <- (read :: String -> Int) . fromMaybe "5432" <$> lookupEnv "POSTGRES_PORT"
  name <- fromMaybe "ladder_test" <$> lookupEnv "POSTGRES_NAME"
  connString <- return . pack $
    "postgresql://" ++ user ++
    ":" ++
    pass ++
    "@" ++
    host ++
    ":" ++
    show port ++
    "/" ++
    name
  -- JWT settings
  jwk <- generateKey
  return $ Config connString jwk


defaultHandle :: IO Handle
defaultHandle = do
  cs <-  connString <$> defaultConfig
  Handle <$> Postgres.connectPostgreSQL cs
