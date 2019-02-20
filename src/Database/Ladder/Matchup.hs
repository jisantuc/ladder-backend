module Database.Ladder.Matchup ( getMatchup
                               , createMatchup
                               , scheduleMatchup
                               , deleteMatchup
                               , listUnscheduledMatchups
                               , listMatchupsForPlayer
                               , listMatchupsAtVenue ) where

import           Data.Int                         (Int64)
import           Data.Ladder.Matchup
import qualified Data.Ladder.Player               as Player
import qualified Data.Ladder.Time                 as Time
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ

getMatchup :: Database.Handle -> UUID -> IO [Matchup]
getMatchup handle matchupID =
  let
    fetchQuery = [sql|SELECT id, player1, player2, week, season, date, venue
                     FROM matchups
                     WHERE id = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only matchupID)

listUnscheduledMatchups :: Database.Handle -> Player.Player -> IO [Matchup]
listUnscheduledMatchups handle player =
  let
    fetchQuery = [sql|SELECT
                     matchups.id,
                     player1,
                     player2,
                     week,
                     season,
                     date,
                     venue
                     FROM
                      matchups LEFT JOIN matches ON
                      matches.matchup = matchups.id
                     WHERE
                       matches.matchup IS NULL
                       AND player1 = ?
                       OR player2 = ?; |]
    pid = Player.playerID player
  in
    Postgres.query (Database.conn handle) fetchQuery (pid, pid)

createMatchup :: Database.Handle -> Matchup -> IO [Matchup]
createMatchup handle matchup =
  let
    insertQuery = [sql|INSERT INTO matchups (id, player1, player2, week, season, date, venue)
                      VALUES (?, ?, ?, ?, ?, ?, ?)
                      RETURNING id, player1, player2, week, season, date, venue; |]
  in
    Postgres.query (Database.conn handle) insertQuery matchup

scheduleMatchup :: Database.Handle -> Matchup -> IO Int64
scheduleMatchup handle matchup =
  let
    updateQuery = [sql|UPDATE matchups SET date = ?, venue = ? WHERE id = ?;|]
  in
    case (venue matchup, date matchup) of
      (Just venueID, Just date) ->
        Postgres.execute (Database.conn handle) updateQuery (date, venueID, matchupID matchup)
      _ ->
        pure 0

deleteMatchup :: Database.Handle -> Matchup -> IO Int64
deleteMatchup handle matchup =
  let
    deleteQuery = [sql|DELETE FROM matchups WHERE id = ?;|]
  in
    Postgres.execute (Database.conn handle) deleteQuery (Postgres.Only $ matchupID matchup)

listMatchupsForPlayer :: Database.Handle -> UUID -> IO [Matchup]
listMatchupsForPlayer handle playerID =
  let
    listQuery = [sql|SELECT id, player1, player2, week, season, date, venue
                    FROM matchups
                    WHERE (player1 = ? OR player2 = ?) AND (date > now() OR date IS NULL);|]
  in
    Postgres.query (Database.conn handle) listQuery (playerID, playerID)

listMatchupsAtVenue :: Database.Handle -> VenueFilter -> IO [Matchup]
listMatchupsAtVenue handle filter =
  let
    listQuery = [sql|SELECT id, player1, player2, week, season, date, venue
                    FROM matchups
                    WHERE date(date) = date(?) AND venue = ?;|]
  in
    Postgres.query (Database.conn handle) listQuery filter
