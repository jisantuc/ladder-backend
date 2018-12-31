module Server where

import Data.Ladder.Time
import Server.Venue
import Servant

api :: Proxy VenueAPI
api = Proxy

application :: Application
application = serve api venueServer
