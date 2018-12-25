module Database.Ladder.Season ( getCurrentSeason
                              , createSeason ) where

import           Data.Ladder.Season
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ

createSeason :: Database.Handle -> Season -> IO [Season]
createSeason handle season =
  let
    query =
      [sql|INSERT INTO seasons (id, year, session)
          VALUES (?, ?, ?)
          RETURNING id, year, session; |]
  in
    Postgres.query (Database.conn handle) query season

getCurrentSeason :: Database.Handle -> IO [Season]
getCurrentSeason handle =
  let
    query =
        [sql|SELECT id, year, session
            FROM seasons ORDER BY year DESC, session DESC
            LIMIT 1; |]
  in
    Postgres.query_ (Database.conn handle) query
