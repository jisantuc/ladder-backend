module Data.Season () where

import qualified Data.Time as Time
import           Data.UUID (UUID)

data Season = Season { seasonId :: UUID
                     , year     :: Int
                     , session  :: Time.Session }
