module Data.Ladder.ProposedMatch ( ProposedMatch (..) ) where

import Data.UUID (UUID)
import qualified Data.Ladder.Time as Time
import GHC.Generics (Generic)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres

data ProposedMatch = ProposedMatch { proposedMatchID :: UUID
                                   , matchup :: UUID
                                   , proposedBy :: UUID
                                   , venue :: UUID
                                   , matchTime :: Time.SqlTime } deriving (Eq, Show, Generic)

instance Postgres.ToRow ProposedMatch
instance Postgres.FromRow ProposedMatch
