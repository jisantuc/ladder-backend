module Database.Ladder.Player ( getPlayer
                              , getPlayerByEmail
                              , createPlayer
                              , updatePlayer
                              , deletePlayer
                              , listPlayers ) where

import           Data.Int                         (Int64)
import           Data.Ladder.Player
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ

getPlayer :: Database.Handle -> UUID -> IO [Player]
getPlayer handle playerID =
  let
    fetchQuery = [sql|SELECT id, email, first_name, last_name, accepting_matches
                     FROM players
                     WHERE id = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only playerID)

getPlayerByEmail :: Database.Handle -> String -> IO [Player]
getPlayerByEmail handle email =
  let
    fetchQuery = [sql|SELECT id, email, first_name, last_name, accepting_matches
                     FROM players
                     WHERE email = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only email)

createPlayer :: Database.Handle -> Player -> IO [Player]
createPlayer handle player =
  let
    insertQuery = [sql|INSERT INTO players (id, email, first_name, last_name, accepting_matches)
                      VALUES (?, ?, ?, ?, ?)
                      RETURNING id, email, first_name, last_name, accepting_matches;|]
  in
    Postgres.query (Database.conn handle) insertQuery player

updatePlayer :: Database.Handle -> Player -> IO Int64
updatePlayer handle player =
  let
    updateQuery = [sql|UPDATE players
                      SET email = ?, first_name = ?, last_name = ?, accepting_matches = ?
                      WHERE id = ?;|]
  in
    Postgres.execute (Database.conn handle) updateQuery (playerToUpdate player)

deletePlayer :: Database.Handle -> Player -> IO Int64
deletePlayer handle player =
  let
    deleteQuery = [sql|DELETE FROM players WHERE id = ?;|]
  in
    Postgres.execute (Database.conn handle) deleteQuery (Postgres.Only $ playerID player)

listPlayers :: Database.Handle -> IO [Player]
listPlayers handle =
  let
    listQuery = [sql|SELECT id, email, first_name, last_name, accepting_matches FROM players;|]
  in
    Postgres.query_ (Database.conn handle) listQuery
