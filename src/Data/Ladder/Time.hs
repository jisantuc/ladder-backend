module Data.Ladder.Time ( DayOfWeek (..)
                        , DaysOfWeek (..)
                        , Session(..)
                        , SqlTime(..)
                        , allDaysOfWeek
                        , dowFromString
                        , dowComplement
                        , now
                        , toUTCTime ) where

import           Data.Aeson
import qualified Data.ByteString.Char8                as B
import           Data.Foldable                        (toList)
import Data.Monoid ((<>))
import qualified Data.Text                            as T
import           Data.Time.Clock                      (UTCTime, getCurrentTime)
import           Data.Time.LocalTime                  (ZonedTime, utc,
                                                       utcToZonedTime,
                                                       zonedTimeToLocalTime,
                                                       zonedTimeToUTC)
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import           Database.PostgreSQL.Simple.Time      (Unbounded (..),
                                                       ZonedTimestamp,
                                                       zonedTimestampToBuilder)
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Database.PostgreSQL.Simple.Types     as Postgres
import           GHC.Generics                         (Generic)
import           Servant

newtype DaysOfWeek = DaysOfWeek (Postgres.PGArray DayOfWeek) deriving (Eq, Show, Generic)

instance Postgres.ToField DaysOfWeek where
  toField (DaysOfWeek dows) = Postgres.toField dows
instance Postgres.FromField DaysOfWeek where
  fromField f v =
    DaysOfWeek <$> (Postgres.fromField f v)

instance ToJSON DaysOfWeek where
  toJSON (DaysOfWeek (Postgres.PGArray dows)) = toJSON dows

instance FromJSON DaysOfWeek where
  parseJSON v =
    DaysOfWeek . Postgres.PGArray <$> (parseJSON v)

data DayOfWeek = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday deriving (Eq, Show, Enum, Generic)

dowFromString :: String -> DayOfWeek
dowFromString "Monday"    = Monday
dowFromString "Tuesday"   = Tuesday
dowFromString "Wednesday" = Wednesday
dowFromString "Thursday"  = Thursday
dowFromString "Friday"    = Friday
dowFromString "Saturday"  = Saturday
dowFromString "Sunday"    = Sunday
dowFromString s           = error $ "Not a valid day of week: " ++ s

dowFromStringE :: T.Text -> Either T.Text DayOfWeek
dowFromStringE = pure . dowFromString . T.unpack

instance ToJSON DayOfWeek where
  toJSON dow = toJSON $ show dow
instance FromJSON DayOfWeek where
  parseJSON (String v) = pure $ dowFromString (T.unpack v)

instance Postgres.ToField DayOfWeek where
  toField dow = Postgres.Escape . B.pack $ show dow

instance Postgres.FromField DayOfWeek where
  fromField f v =
    case B.unpack <$> v of
      Nothing -> Postgres.returnError Postgres.UnexpectedNull f ""
      Just dat ->
        pure $ dowFromString dat

instance FromHttpApiData [DayOfWeek] where
  parseQueryParam t =
    case T.breakOn "," t of
      (v, "") ->
        pure <$> dowFromStringE v
      (v, w) ->
        (++) <$> (pure <$> dowFromStringE v) <*> parseQueryParam w


allDaysOfWeek :: [DayOfWeek]
allDaysOfWeek = [Monday .. Sunday]

dowComplement :: [DayOfWeek] -> [DayOfWeek]
dowComplement days =
  filter (not . (\x -> elem x days)) [Monday .. Sunday]

data Session = Spring
  | Summer
  | Fall deriving (Eq, Show)

sessFromString :: String -> Session
sessFromString "Spring" = Spring
sessFromString "Summer" = Summer
sessFromString "Fall"   = Fall
sessFromString s        = error $ "Not a valid session: " ++ s

instance Postgres.ToField Session where
  toField sess = Postgres.Escape . B.pack $ show sess

instance Postgres.FromField Session where
  fromField f v =
    case B.unpack <$> v of
      Nothing -> Postgres.returnError Postgres.UnexpectedNull f ""
      Just dat ->
        pure $ sessFromString dat

newtype SqlTime = SqlTime ZonedTimestamp deriving (Show)

instance Postgres.ToField SqlTime where
  toField (SqlTime t) = Postgres.Plain . Postgres.inQuotes . zonedTimestampToBuilder $ t
instance Postgres.FromField SqlTime where
  fromField f v = SqlTime <$> Postgres.fromField f v
instance Eq SqlTime where
  (/=) (SqlTime (Finite t1)) (SqlTime (Finite t2)) =
    zonedTimeToUTC t1 /= zonedTimeToUTC t2

now :: IO SqlTime
now = SqlTime . Finite <$> (utcToZonedTime utc <$> getCurrentTime)

toUTCTime :: SqlTime -> Maybe UTCTime
toUTCTime (SqlTime (Finite t)) = zonedTimeToUTC <$> Just t
toUTCTime (SqlTime _)          = Nothing
