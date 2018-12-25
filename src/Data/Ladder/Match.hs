module Data.Ladder.Match ( Match ) where

import qualified Data.Ladder.Time as Time
import           Data.UUID        (UUID)

data Match = Match { matchID     :: UUID
                   , matchup     :: UUID
                   , startTime   :: Time.SqlTime
                   , recorded    :: Time.SqlTime
                   , player1Wins :: Int
                   , player2Wins :: Int }
