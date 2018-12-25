module Data.Ladder.Player ( Player (..), playerToUpdate ) where

import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)

data Player = Player { playerID         :: UUID
                     , email            :: String
                     , firstName        :: String
                     , lastName         :: String
                     , acceptingMatches :: Bool } deriving (Eq, Show, Generic)

instance Postgres.FromRow Player
instance Postgres.ToRow Player

{- Player records for update queries

The fields need to be in a different order since id isn't updatable but is present in the
filters.
-}
data PlayerUpdate = PlayerUpdate { _email     :: String
                                 , _first     :: String
                                 , _last      :: String
                                 , _accepting :: Bool
                                 , _playerID  :: UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow PlayerUpdate

playerToUpdate :: Player -> PlayerUpdate
playerToUpdate player = PlayerUpdate { _email = email player
                               , _first = firstName player
                               , _last = lastName player
                               , _accepting = acceptingMatches player
                               , _playerID = playerID player }
