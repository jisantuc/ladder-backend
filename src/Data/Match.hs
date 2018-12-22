module Data.Match ( Match ) where

import qualified Data.Time as Time
import           Data.UUID (UUID)

data Match = Match { matchID     :: UUID
                   , matchup     :: UUID
                   , venue       :: UUID
                   , date        :: Time.SqlTime
                   , recorded    :: Time.SqlTime
                   , player1Wins :: Int
                   , player2Wins :: Int }
