module Data.Ladder.Player ( Player (..), playerToUpdate, makePlayer ) where

import           Data.Aeson
import           Data.UUID                          (UUID)
import qualified Data.UUID.V4                       as UUIDv4
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)
import           Servant.Auth.Server

data Player = Player { playerID         :: UUID
                     , email            :: String
                     , firstName        :: String
                     , lastName         :: String
                     , acceptingMatches :: Bool } deriving (Eq, Show, Generic)

instance Postgres.FromRow Player
instance Postgres.ToRow Player
instance ToJSON Player
instance FromJSON Player
instance ToJWT Player
instance FromJWT Player

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

makePlayer :: String -> String -> String -> IO Player
makePlayer e f l =
  (\x -> Player x e f l False) <$> UUIDv4.nextRandom
