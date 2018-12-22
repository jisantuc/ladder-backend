module Rating (Rating) where

import           Data.UUID (UUID)

data Rating = Rating { ratingID :: UUID
                     , season :: UUID
                     , week :: Int
                     , value :: Double
                     , player :: UUID }
