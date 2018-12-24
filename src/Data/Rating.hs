module Rating (Rating) where

import           Data.UUID (UUID)

data Rating = Rating { ratingID :: UUID
                     , season :: UUID
                     , week :: Int
                     , rating :: Double
                     , player :: UUID }
