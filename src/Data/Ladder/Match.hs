module Data.Ladder.Match ( Match ) where

import qualified Data.Ladder.Time                   as Time
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)


data Match = Match { matchID     :: UUID
                   , matchup     :: UUID
                   , startTime   :: Time.SqlTime
                   , recorded    :: Time.SqlTime
                   , player1Wins :: Int
                   , player2Wins :: Int
                   , validated   :: Bool
                   , submittedBy :: UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow Match
instance Postgres.FromRow Match
