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
  Get '[JSON] [MatchWithRelated]
  :<|> Capture "matchID" UUID :> Get '[JSON] (Maybe MatchWithRelated)
  :<|> Capture "matchID" UUID :> ReqBody '[JSON] Match :> Put '[JSON] NoContent
  :<|> ReqBody '[JSON] Match :> Post '[JSON] (Maybe Match)
  )

matchDetailHandler :: UUID -> Handler (Maybe MatchWithRelated)
matchDetailHandler id = liftIO $ do
  handle <- defaultHandle
  listToMaybe <$> getMatch handle id

matchListHandler :: Player -> Handler [MatchWithRelated]
matchListHandler player = liftIO $ do
  handle <- defaultHandle
  listMatches handle player

matchUpdateHandler :: UUID -> Match -> Handler NoContent
matchUpdateHandler id toUpdate = liftIO $ do
  handle <- defaultHandle
  if (id == matchID toUpdate) then
    (\_ -> NoContent) <$> updateMatch handle toUpdate
  else
    pure NoContent

matchCreateHandler :: Match -> Handler (Maybe Match)
matchCreateHandler match = liftIO $ do
  handle <- defaultHandle
  either (\_ -> Nothing) listToMaybe <$> submitMatch handle match


matchServer :: Server MatchAPI
matchServer (SAS.Authenticated p) =
  matchListHandler p
  :<|> matchDetailHandler
  :<|> matchUpdateHandler
  :<|> matchCreateHandler
