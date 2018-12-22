module Data.Player ( Player ) where

import           Data.UUID (UUID)

data Player = Player { playerID  :: UUID
                     , email     :: String
                     , firstName :: String
                     , lastName  :: String
                     , active    :: Bool }
