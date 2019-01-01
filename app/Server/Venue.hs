module Server.Venue where

import Config (defaultHandle)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Ladder.Venue
import Data.Maybe (fromMaybe)
import Database.Ladder.Venue
import qualified Data.Ladder.Time as Time
import qualified Database.PostgreSQL.Simple.Types as Postgres
import Servant.Server
import Servant

import Data.UUID (UUID)

import Debug.Trace

type VenueAPI =
  "venues" :> QueryParam "freeNights" [Time.DayOfWeek] :> Get '[JSON] [Venue]
  -- :<|> "venues" :> Capture "venueID" UUID :> Get '[JSON] Venue
  -- :<|> "venues" :> Put '[JSON] Int
  -- :<|> "venues" :> Delete '[JSON] Int
  -- :<|> "venues" :> PostCreated '[JSON] [Venue]

venueListHandler :: Maybe [Time.DayOfWeek] -> Handler [Venue]
venueListHandler maybeDaysOfWeek = do
  handle <- liftIO defaultHandle
  liftIO $ listVenues handle freeNights
  where
    freeNights =
      Postgres.PGArray $ fromMaybe Time.allDaysOfWeek maybeDaysOfWeek

venueServer :: Server VenueAPI
venueServer = venueListHandler
