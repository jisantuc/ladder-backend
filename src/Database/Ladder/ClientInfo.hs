module Database.Ladder.ClientInfo ( storePassword
                                  , getJWT ) where

import           Control.Arrow                    (left)
import           Crypto.BCrypt
import           Data.ByteString.Char8            (pack, unpack)
import           Data.ByteString.Lazy             (ByteString)
import           Data.Int                         (Int64)
import           Data.Ladder.ClientInfo
import qualified Data.Ladder.Player               as Player
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.Ladder.Player           as Player
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ
import           Error
import           Servant.Auth.Server              (JWTSettings, makeJWT)

storePassword :: Database.Handle -> ClientInfo -> IO Int64
storePassword handle info =
  let
    userFetchQuery = [sql|SELECT id FROM users WHERE email = ?;|]
    passInsertQuery = [sql|INSERT INTO client_info (user_id, password) VALUES (?, ?);|]
  in
    do
      encrypted <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack . pass $ info)
      users <- Postgres.query (Database.conn handle) userFetchQuery (Postgres.Only $ email info)
      case users of
        (x : []) ->
          Postgres.execute (Database.conn handle) passInsertQuery (Player.playerID x, encrypted)
        _ -> pure 0

getJWT :: Database.Handle -> JWTSettings -> ClientInfo -> IO (Either Message ByteString)
getJWT handle settings info =
  let
    passFetchQuery = [sql|SELECT password FROM client_info WHERE player_id = ?;|]
  in
    do
      users <- Player.getPlayerByEmail handle (email info)
      case users of
        [] ->
          pure $ Left BadPassword
        (x : []) ->
          do
            userPass <- head <$>
              (Postgres.query (Database.conn handle) passFetchQuery (Postgres.Only $ Player.playerID x)
               :: IO [String])
            if (validatePassword (pack userPass) (pack . pass $ info)) then
              left (const BadPassword) <$> makeJWT x settings Nothing
            else
              pure $ Left BadPassword
        _ ->
          pure $ Left BadPassword
