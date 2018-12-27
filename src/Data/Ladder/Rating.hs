module Data.Ladder.Rating (Rating (..)) where

import           Data.UUID    (UUID)
import           GHC.Generics (Generic)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres

data Rating = Rating { ratingID :: UUID
                     , season   :: UUID
                     , week     :: Int
                     , rating   :: Double
                     , player   :: UUID } deriving (Eq, Show, Generic)

instance Postgres.ToRow Rating
instance Postgres.FromRow Rating
