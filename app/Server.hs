module Server where

import           Config
import           Data.Ladder.Time
import           Servant
import           Servant.Auth.Server
import           Server.Match
import           Server.Matchup
import           Server.Player
import           Server.Token
import           Server.Venue

type LadderAPI =
  MatchAPI
  :<|> MatchupAPI
  :<|> PlayerAPI
  :<|> TokenAPI
  :<|> VenueAPI

api :: Proxy LadderAPI
api = Proxy

server :: JWTSettings -> Server LadderAPI
server settings =
  matchServer :<|>
  matchupServer :<|>
  playerServer :<|>
  tokenServer settings :<|>
  venueServer

application :: IO Application
application = do
  key <- jwk <$> defaultConfig
  jwtSettings <- pure $ defaultJWTSettings key
  cfg <- pure $  jwtSettings :. defaultCookieSettings :. EmptyContext
  return $ serveWithContext api cfg (server jwtSettings)
