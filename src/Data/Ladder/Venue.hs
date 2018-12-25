module Data.Ladder.Venue (Venue (..)) where

import qualified Data.Ladder.Time                   as Time
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import qualified Database.PostgreSQL.Simple.Types   as Postgres
import           GHC.Generics                       (Generic)

data Venue = Venue { venueID      :: UUID
                   , name         :: String
                   , phone        :: String
                   , leagueNights :: Postgres.PGArray Time.DayOfWeek
                   , cost         :: Maybe Double } deriving (Eq, Show, Generic)

instance Postgres.ToRow Venue
instance Postgres.FromRow Venue

data VenueUpdate = VenueUpdate { _name :: String
                               , _phone :: String
                               , _leagueNights :: Postgres.PGArray Time.DayOfWeek
                               , _cost :: Maybe Double
                               , _venueID :: UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow VenueUpdate

venueToUpdate :: Venue -> VenueUpdate
venueToUpdate venue = VenueUpdate { _name = name venue
                                  , _phone = phone venue
                                  , _leagueNights = leagueNights venue
                                  , _cost = cost venue
                                  , _venueID = venueID venue }
