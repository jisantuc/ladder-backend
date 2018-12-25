module Data.Ladder.Venue (Venue (..)) where

import qualified Data.Ladder.Time                 as Time
import           Data.UUID                        (UUID)
import qualified Database.PostgreSQL.Simple.Types as Postgres

data Venue = Venue { venueID      :: UUID
                   , name         :: String
                   , phone        :: String
                   , leagueNights :: Postgres.PGArray Time.DayOfWeek
                   , cost         :: Maybe Double }
