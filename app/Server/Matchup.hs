module Server.Matchup (matchupServer, MatchupAPI) where

import           Config                  (defaultHandle)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Ladder.Matchup
import           Data.Ladder.Player      (Player)
import           Data.Maybe              (listToMaybe)
import           Database.Ladder.Matchup
import           Servant
import qualified Servant.Auth.Server     as SAS
import           Servant.Server

import           Data.UUID               (UUID)

type MatchupAPI =
  SAS.Auth '[SAS.JWT] Player :> "matchups" :> (
  Get '[JSON] (Maybe Matchup)
  )

matchupListHandler :: Player -> Handler (Maybe Matchup)
matchupListHandler player = liftIO $ do
  handle <- defaultHandle
  listToMaybe <$> listUnscheduledMatchups handle player

matchupServer :: Server MatchupAPI
matchupServer (SAS.Authenticated p) =
  matchupListHandler p
