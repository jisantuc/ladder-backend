module Data.Ladder.Season (Season(..)) where

import qualified Data.Ladder.Time                   as Time
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)

data Season = Season { seasonID :: UUID
                     , year     :: Int
                     , session  :: Time.Session } deriving (Eq, Show, Generic)

instance Postgres.FromRow Season
instance Postgres.ToRow Season
