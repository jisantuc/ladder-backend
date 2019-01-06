module Data.Ladder.ClientInfo (ClientInfo (..))where

import           Data.Aeson
import           GHC.Generics (Generic)

data ClientInfo = ClientInfo { email :: String
                             , pass  :: String } deriving (Eq, Show, Generic)

instance FromJSON ClientInfo
