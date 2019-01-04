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
    passInsertQuery = [sql|INSERT INTO client_info (player, password) VALUES (?, ?);|]
  in
    do
      encrypted <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack . pass $ info)
      users <- Player.getPlayerByEmail handle (email info)
      case users of
        (x : []) ->
          Postgres.execute (Database.conn handle) passInsertQuery (Player.playerID x, encrypted)
        _ -> pure 0

getJWT :: Database.Handle -> JWTSettings -> ClientInfo -> IO (Either Message ByteString)
getJWT handle settings info =
  let
    -- It's stupid to select the 1, but it keeps the type mapping from getting weird,
    -- so :man_shrugging:
    passFetchQuery = [sql|SELECT password, 1 FROM client_info WHERE player = ?;|]
  in
    do
      users <- Player.getPlayerByEmail handle (email info)
      case users of
        [] ->
          pure $ Left BadPassword
        (x : []) ->
          do
            (userPass, _) <- head <$>
              (Postgres.query (Database.conn handle) passFetchQuery (Postgres.Only $ Player.playerID x)
               :: IO [(String, Int)])
            if (validatePassword (pack userPass) (pack . pass $ info)) then
              left (const BadPassword) <$> makeJWT x settings Nothing
            else
              pure $ Left BadPassword
        _ ->
          pure $ Left BadPassword
