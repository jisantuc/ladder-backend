module Data.Matchup ( Matchup ) where

import           Data.UUID (UUID)

data Matchup = Matchup { matchupID      :: UUID
                       , player1        :: UUID
                       , player2        :: UUID
                       , player1MustWin :: Int
                       , week           :: Int
                       , season         :: UUID }
