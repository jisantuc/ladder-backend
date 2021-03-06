module Server.Player (playerServer, PlayerAPI) where

import           Config                 (defaultHandle)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Ladder.Player     (Player (..), playerID)
import qualified Data.Ladder.Rating     as Rating
import           Data.Maybe             (listToMaybe)
import           Data.UUID              (UUID)
import           Database.Ladder.Player
import           Database.Ladder.Rating (listRecentPlayerRatings)
import           Servant
import qualified Servant.Auth.Server    as SAS
import           Servant.Server

type PlayerAPI =
  SAS.Auth '[SAS.JWT] Player :> "players" :> (
  "me" :> Get '[JSON] (Maybe Player)
  :<|> "me" :> "recent-ratings" :> Capture "page-size" Int :> Get '[JSON] [Rating.Rating]
  :<|> "me" :> "active" :> Put '[JSON] NoContent
  )

playerMeHandler :: UUID -> Handler (Maybe Player)
playerMeHandler id = liftIO $ do
  handle <- defaultHandle
  listToMaybe <$> getPlayer handle id

recentRatingsHandler :: UUID -> Int -> Handler [Rating.Rating]
recentRatingsHandler id pageSize = liftIO $ do
  handle <- defaultHandle
  listRecentPlayerRatings handle id pageSize

togglePlayerActive :: UUID -> Handler NoContent
togglePlayerActive id = liftIO $ do
  handle <- defaultHandle
  playerO <- listToMaybe <$> getPlayer handle id
  _ <- (\player -> updatePlayer handle (player { acceptingMatches = not $ acceptingMatches player}))
    `traverse` playerO
  pure NoContent

playerServer :: Server PlayerAPI
playerServer (SAS.Authenticated user) =
  let
    pid = playerID user
  in
    playerMeHandler pid
    :<|> recentRatingsHandler pid
    :<|> togglePlayerActive pid
playerServer _ = SAS.throwAll err401
