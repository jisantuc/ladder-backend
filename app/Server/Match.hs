module Server.Match (matchServer, MatchAPI) where

import           Config                 (defaultHandle)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Ladder.Match
import           Data.Ladder.Player     (Player)
import           Data.Maybe             (listToMaybe)
import           Database.Ladder.Match
import           Servant
import qualified Servant.Auth.Server    as SAS
import           Servant.Server

import           Data.UUID              (UUID)

type MatchAPI =
  SAS.Auth '[SAS.JWT] Player :> "matches" :> (
  Capture "matchID" UUID :> Get '[JSON] (Maybe MatchWithRelated)
  )

matchDetailHandler :: UUID -> Handler (Maybe MatchWithRelated)
matchDetailHandler id = liftIO $ do
  handle <- defaultHandle
  listToMaybe <$> getMatch handle id

matchServer :: Server MatchAPI
matchServer (SAS.Authenticated p) =
  matchDetailHandler
