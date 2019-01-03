module Server.Token (TokenAPI (..), tokenServer) where

import           Config
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Aeson
import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy       (toStrict)
import           Data.Ladder.ClientInfo     (ClientInfo (..))
import           Data.Ladder.Player         (Player (..))
import           Database.Ladder.ClientInfo
import           GHC.Generics               (Generic)
import           Servant
import qualified Servant.Auth.Server        as SAS
import           Servant.Server


data TokenResp = TokenResp { key :: String } deriving (Eq, Show, Generic)
instance ToJSON TokenResp

type TokenAPI =
  "token"
  :> ReqBody '[JSON] ClientInfo
  :> Post '[JSON] TokenResp

tokenPostHandler :: ClientInfo -> Handler TokenResp
tokenPostHandler clientInfo = do
  handle <- liftIO defaultHandle
  key <- liftIO $ jwk <$> defaultConfig
  jwtSettings <- pure $ SAS.defaultJWTSettings key
  jwtResult <- liftIO $ getJWT handle jwtSettings clientInfo
  case jwtResult of
    Right key ->
      pure $ TokenResp (unpack . toStrict $ key)
    Left _ ->
      error "Could not get JWT from user and pass"

tokenServer :: Server TokenAPI
tokenServer = tokenPostHandler
