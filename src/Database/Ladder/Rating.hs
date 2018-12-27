module Database.Ladder.Rating ( getRating
                              , createRating
                              , listRecentPlayerRatings ) where

import           Data.Int                         (Int64)
import           Data.Ladder.Rating
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ

getRating :: Database.Handle -> UUID -> IO [Rating]
getRating handle ratingID =
  let
    fetchQuery = [sql|SELECT id, season, week, rating, player
                     FROM ratings
                     WHERE id = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only ratingID)

createRating :: Database.Handle -> Rating -> IO [Rating]
createRating handle rating =
  let
    insertQuery = [sql|INSERT INTO ratings (id, season, week, rating, player)
                      VALUES (?, ?, ?, ?, ?)
                      RETURNING id, season, week, rating, player;|]
  in
    Postgres.query (Database.conn handle) insertQuery rating

listRecentPlayerRatings :: Database.Handle -> UUID -> Int -> IO [Rating]
listRecentPlayerRatings handle playerID nRecentWeeks =
  let
    listQuery = [sql|SELECT id, season, week, rating, player
                    FROM ratings
                    ORDER BY season desc, week desc
                    LIMIT ?;|]
  in
    Postgres.query (Database.conn handle) listQuery (Postgres.Only nRecentWeeks)
