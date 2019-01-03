module Server where

import           Config
import           Data.Ladder.Time
import           Servant
import           Servant.Auth.Server
import           Server.Token
import           Server.Venue

type LadderAPI =
  VenueAPI :<|> TokenAPI

api :: Proxy LadderAPI
api = Proxy

server :: Server LadderAPI
server =
  venueServer :<|>
  tokenServer

application :: IO Application
application = do
  key <- jwk <$> defaultConfig
  jwtSettings <- pure $ defaultJWTSettings key
  cfg <- pure $  jwtSettings :. defaultCookieSettings :. EmptyContext
  return $ serveWithContext api cfg server
