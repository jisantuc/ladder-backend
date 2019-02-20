module Data.Ladder.Rating (Rating (..)) where

import           Data.Aeson
import           Data.UUID                          (UUID)
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import qualified Database.PostgreSQL.Simple.ToRow   as Postgres
import           GHC.Generics                       (Generic)

data Rating = Rating { ratingID :: UUID
                     , season   :: UUID
                     , week     :: Int
                     , rating   :: Double
                     , player   :: UUID } deriving (Eq, Show, Generic)

instance ToJSON Rating
instance FromJSON Rating

instance Postgres.ToRow Rating
instance Postgres.FromRow Rating
