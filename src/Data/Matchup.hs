module Data.Matchup ( Matchup ) where

import           Data.UUID (UUID)

data Matchup = Matchup { matchupID      :: UUID
                       , player1        :: UUID
                       , player2        :: UUID
                       , week           :: Int
                       , season         :: UUID }
