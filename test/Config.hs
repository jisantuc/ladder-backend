module Config (defaultHandle) where

import           Data.ByteString            (ByteString)
import           Database.Ladder            (Handle (..))
import qualified Database.PostgreSQL.Simple as Postgres

connString :: ByteString
connString = "postgresql://ladder:ladder@localhost:5432/ladder_test"

defaultHandle :: IO Handle
defaultHandle = do
  conn <- Postgres.connectPostgreSQL connString
  return $ Handle conn
