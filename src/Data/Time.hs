module Data.Time ( DayOfWeek
                 , Session
                 , SqlTime(..)
                 , now ) where

import           Data.Time.Clock                      (getCurrentTime)
import           Data.Time.LocalTime                  (utc, utcToLocalTime)
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import           Database.PostgreSQL.Simple.Time      (LocalTimestamp,
                                                       Unbounded (..),
                                                       localTimestampToBuilder,
                                                       parseLocalTimestamp)
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

data DayOfWeek = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

data Session = Spring
  | Summer
  | Fall

newtype SqlTime = SqlTime LocalTimestamp deriving (Eq, Show)

instance Postgres.ToField SqlTime where
  toField (SqlTime t) = Postgres.Plain . Postgres.inQuotes . localTimestampToBuilder $ t
instance Postgres.FromField SqlTime where
  fromField f v = SqlTime <$> Postgres.fromField f v

now :: IO SqlTime
now = SqlTime . Finite <$> (utcToLocalTime utc <$> getCurrentTime)
