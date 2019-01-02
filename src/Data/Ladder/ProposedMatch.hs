module Data.Ladder.ProposedMatch ( ProposedMatch (..) ) where

import qualified Data.Ladder.Time                   as Time
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)

data ProposedMatch = ProposedMatch { proposedMatchID :: UUID
                                   , matchup         :: UUID
                                   , proposedBy      :: UUID
                                   , venue           :: UUID
                                   , matchTime       :: Time.SqlTime } deriving (Eq, Show, Generic)

instance Postgres.ToRow ProposedMatch
instance Postgres.FromRow ProposedMatch
