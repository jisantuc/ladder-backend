module Data.Ladder.Season () where

import qualified Data.Ladder.Time as Time
import           Data.UUID        (UUID)

data Season = Season { seasonId :: UUID
                     , year     :: Int
                     , session  :: Time.Session }
