module Server.Venue (venueServer, VenueAPI) where

import           Config                           (defaultHandle)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Int                         (Int64)
import           Data.Ladder.Player               (Player)
import qualified Data.Ladder.Time                 as Time
import           Data.Ladder.Venue
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Database.Ladder.Venue
import qualified Database.PostgreSQL.Simple.Types as Postgres
import           Servant
import qualified Servant.Auth.Server              as SAS
import           Servant.Server


import           Data.UUID                        (UUID)

import           Debug.Trace

type VenueAPI =
  SAS.Auth '[SAS.JWT] Player :>
  ("venues" :> QueryParam "freeNights" [Time.DayOfWeek] :> Get '[JSON] [Venue]
  :<|> "venues" :> Capture "venueID" UUID :> Get '[JSON] (Maybe Venue)
  :<|> "venues" :> Capture "venueID" UUID :> ReqBody '[JSON] Venue :> Put '[JSON] Int64
  :<|> "venues" :> ReqBody '[JSON] Venue :> PostCreated '[JSON] (Maybe Venue)
  )

venueCreateHandler :: Venue -> Handler (Maybe Venue)
venueCreateHandler venue = liftIO $
  do
    handle <- defaultHandle
    listToMaybe <$> createVenue handle venue

venueUpdateHandler :: UUID -> Venue -> Handler Int64
venueUpdateHandler venueID updatey =
  let
    -- don't allow updating a venue different from the one being PUT
    sanitized = updatey { venueID = venueID}
  in
    liftIO $
    do
      handle <- defaultHandle
      updateVenue handle sanitized

venueDetailHandler :: UUID -> Handler (Maybe Venue)
venueDetailHandler venueID = liftIO $
  do
    handle <- liftIO defaultHandle
    listToMaybe <$> getVenue handle venueID

venueListHandler :: Maybe [Time.DayOfWeek] -> Handler [Venue]
venueListHandler maybeDaysOfWeek = do
  handle <- liftIO defaultHandle
  liftIO $ listVenues handle freeNights
  where
    freeNights =
      Postgres.PGArray $ fromMaybe Time.allDaysOfWeek maybeDaysOfWeek

venueServer :: Server VenueAPI
venueServer (SAS.Authenticated _) =
  venueListHandler
  :<|> venueDetailHandler
  :<|> venueUpdateHandler
  :<|> venueCreateHandler
venueServer _                     = SAS.throwAll err401
