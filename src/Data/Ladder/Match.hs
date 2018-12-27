module Data.Ladder.Match ( Match (..)
                         , MatchWithRelated (..)
                         , matchToUpdate ) where

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

data MatchWithRelated = MatchWithRelated { _matchID     :: UUID
                                         , _startTime   :: Time.SqlTime
                                         , _recorded    :: Time.SqlTime
                                         , _player1Wins :: Int
                                         , _player2Wins :: Int
                                         , _validated   :: Bool
                                         , _submittedBy :: UUID
                                         , season       :: UUID
                                         , week         :: Int
                                         , matchupDate  :: Maybe Time.SqlTime
                                         , matchupVenue :: Maybe UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow MatchWithRelated
instance Postgres.FromRow MatchWithRelated

data MatchUpdate = MatchUpdate { newPlayer1Wins :: Int
                               , newPlayer2Wins :: Int
                               , _matchID'      :: UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow MatchUpdate

matchToUpdate :: Match -> MatchUpdate
matchToUpdate match = MatchUpdate { newPlayer1Wins = player1Wins match
                                  , newPlayer2Wins = player2Wins match
                                  , _matchID' = matchID match }
