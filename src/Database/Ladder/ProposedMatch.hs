module Database.Ladder.ProposedMatch ( getProposedMatch
                                     , proposeMatch
                                     , acceptMatch
                                     , cancelMatch
                                     , listProposedMatches ) where

import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres

import           Data.Int                         (Int64)
import           Data.Ladder.ProposedMatch
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ

getProposedMatch :: Database.Handle -> UUID -> IO [ProposedMatch]
getProposedMatch handle proposedMatchID =
  let
    fetchQuery = [sql|SELECT id, matchup, proposed_by, venue, match_time
                     FROM proposed_matches
                     WHERE id = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only proposedMatchID)

proposeMatch :: Database.Handle -> ProposedMatch -> IO [ProposedMatch]
proposeMatch handle proposedMatch =
  let
    insertQuery = [sql|INSERT INTO proposed_matches (id, matchup, proposed_by, venue, match_time, accepted, canceled)
                      VALUES (?, ?, ?, ?, ?, false, false)
                      RETURNING id, matchup, proposed_by, venue, match_time; |]
  in
    Postgres.query (Database.conn handle) insertQuery proposedMatch

acceptMatch :: Database.Handle -> ProposedMatch -> IO Int64
acceptMatch handle proposedMatch =
  let
    updateQuery = [sql|UPDATE proposed_matches SET accepted = true
                      WHERE
                      id = ? AND accepted = false AND canceled = false;|]
    cancelQuery = [sql|UPDATE proposed_matches SET canceled = true
                      WHERE
                      id = ? AND matchup = ? AND accepted = false AND canceled = false; |]
  in
    Postgres.execute (Database.conn handle) updateQuery (Postgres.Only (proposedMatchID proposedMatch)) <*
    Postgres.execute (Database.conn handle) cancelQuery (proposedMatchID proposedMatch, matchup proposedMatch)

cancelMatch :: Database.Handle -> UUID -> IO Int64
cancelMatch handle proposedMatchID =
  let
    cancelQuery = [sql|UPDATE proposed_matches SET canceled = true
                      WHERE id = ? AND accepted = false; |]
  in
    Postgres.execute (Database.conn handle) cancelQuery (Postgres.Only proposedMatchID)

listProposedMatches :: Database.Handle -> UUID -> IO [ProposedMatch]
listProposedMatches handle matchupID =
  let
    listQuery = [sql|SELECT id, matchup, proposed_by, venue, match_time
                    FROM proposed_matches
                    WHERE matchup = ? AND canceled = false AND accepted = false; |]
  in
    Postgres.query (Database.conn handle) listQuery (Postgres.Only matchupID)
