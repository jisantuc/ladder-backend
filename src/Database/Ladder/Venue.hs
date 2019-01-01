module Database.Ladder.Venue ( getVenue
                             , createVenue
                             , updateVenue
                             , deleteVenue
                             , listVenues ) where

import           Data.Int                         (Int64)
import qualified Data.Ladder.Time                 as Time
import           Data.Ladder.Venue
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types as Postgres

getVenue :: Database.Handle -> UUID -> IO [Venue]
getVenue handle venueID =
  let
    fetchQuery = [sql|SELECT id, name, phone, address, league_nights, cost
                     FROM venues
                     WHERE id = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only venueID)

createVenue :: Database.Handle -> Venue -> IO [Venue]
createVenue handle venue =
  let
    insertQuery = [sql|INSERT INTO venues (id, name, phone, address, league_nights, cost)
                      VALUES (?, ?, ?, ?, ? :: day_of_week[], ?)
                      RETURNING id, name, phone, address, league_nights, cost; |]
  in
    Postgres.query (Database.conn handle) insertQuery venue

updateVenue :: Database.Handle -> Venue -> IO Int64
updateVenue handle venue =
  let
    updateQuery = [sql|UPDATE venues
                      SET name = ?, phone = ?, address = ?, league_nights = (? :: day_of_week[]), cost = ?
                      WHERE id = ?;|]
  in
    Postgres.execute (Database.conn handle) updateQuery (venueToUpdate venue)

deleteVenue :: Database.Handle -> Venue -> IO Int64
deleteVenue handle venue =
  let
    deleteQuery = [sql|DELETE FROM venues WHERE id = ?;|]
  in
    Postgres.execute (Database.conn handle) deleteQuery (Postgres.Only $ venueID venue)

listVenues :: Database.Handle -> Postgres.PGArray Time.DayOfWeek -> IO [Venue]
listVenues handle (Postgres.PGArray desiredNights) =
  let
    listQuery = [sql|SELECT id, name, phone, address, league_nights, cost
                    FROM venues
                    WHERE NOT (? :: day_of_week[]) <@ league_nights;|]
    badNights = Postgres.PGArray desiredNights
  in
    Postgres.query (Database.conn handle) listQuery (Postgres.Only badNights)
