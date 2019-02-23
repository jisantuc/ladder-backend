module Data.Ladder.Matchup ( Matchup (..), VenueFilter (..) ) where

import           Data.Aeson
import qualified Data.Ladder.Time                   as Time
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)

data Matchup = Matchup { matchupID :: UUID
                       , player1   :: UUID
                       , player2   :: UUID
                       , week      :: Int
                       , season    :: UUID
                       , date      :: Maybe Time.SqlTime
                       , venue     :: Maybe UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow Matchup
instance Postgres.FromRow Matchup
instance ToJSON Matchup
instance FromJSON Matchup

data VenueFilter = VenueFilter { dayOfWeek :: Time.SqlTime
                               , venueID   :: UUID} deriving (Eq, Show, Generic)

instance Postgres.ToRow VenueFilter
