module Data.Ladder.Venue (Venue (..), venueToUpdate) where

import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Ladder.Time                   as Time
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import qualified Database.PostgreSQL.Simple.Types   as Postgres
import           GHC.Generics                       (Generic)

data Venue = Venue { venueID      :: UUID
                   , name         :: String
                   , phone        :: String
                   , address      :: String
                   , leagueNights :: Time.DaysOfWeek
                   , cost         :: Maybe Double } deriving (Eq, Show, Generic)

instance Postgres.ToRow Venue
instance Postgres.FromRow Venue

instance ToJSON Venue
instance FromJSON Venue

data VenueUpdate = VenueUpdate { _name :: String
                               , _phone :: String
                               , _address :: String
                               , _leagueNights :: Time.DaysOfWeek
                               , _cost :: Maybe Double
                               , _venueID :: UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow VenueUpdate

venueToUpdate :: Venue -> VenueUpdate
venueToUpdate venue = VenueUpdate { _name = name venue
                                  , _phone = phone venue
                                  , _address = address venue
                                  , _leagueNights = leagueNights venue
                                  , _cost = cost venue
                                  , _venueID = venueID venue }
