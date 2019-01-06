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


data TokenResp =
  TokenResp { key :: String }
  | TokenError { msg :: String } deriving (Eq, Show, Generic)
instance ToJSON TokenResp

type TokenAPI =
  "token"
  :> ReqBody '[JSON] ClientInfo
  :> Post '[JSON] TokenResp

tokenPostHandler :: SAS.JWTSettings -> ClientInfo -> Handler TokenResp
tokenPostHandler jwtSettings clientInfo = do
  handle <- liftIO defaultHandle
  jwtResult <- liftIO $ getJWT handle jwtSettings clientInfo
  case jwtResult of
    Right key ->
      pure $ TokenResp (unpack . toStrict $ key)
    Left _ ->
      pure $ TokenError "Could not get JWT from user and pass"

tokenServer :: SAS.JWTSettings -> Server TokenAPI
tokenServer settings = tokenPostHandler settings
