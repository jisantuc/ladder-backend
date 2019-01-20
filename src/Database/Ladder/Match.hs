module Database.Ladder.Match ( getMatch
                             , submitMatch
                             , updateMatch ) where

import           Data.Int                         (Int64)
import           Data.Ladder.Match
import           Data.UUID                        (UUID)
import qualified Database.Ladder                  as Database
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ
import           Error

checkSubmission :: Database.Handle -> Match -> IO (Either Message (Maybe (Int, Int)))
checkSubmission handle match =
  let
    existingMatchQuery = [sql|SELECT player1_wins, player2_wins
                             FROM matches
                             WHERE matchup = ? and submitted_by <> ?; |]
    existingSubmissionQuery = [sql|select 1, 1
                                  FROM matches
                                  WHERE matchup = ? and submitted_by = ?; |]
    priorWinLossIO = (\x -> case x of
                         []      -> Right Nothing
                         [p]     -> Right $ Just p
                         x1:x2:_ -> Left NotYourMatch
                     ) <$>
                     Postgres.query (Database.conn handle)
                     existingMatchQuery
                     (matchup match, submittedBy match)
    -- This has a stupid type and query, but simple-postgresql is happier with tuples than with single values
    priorUserSubmissionIO = (
      \x -> case x of
          [] -> Right ()
          _  -> Left MatchAlreadySubmitted
      ) <$>
      ((Postgres.query (Database.conn handle)
        existingSubmissionQuery
        (matchup match, submittedBy match))
       :: IO [(Int64, Int64)])
  in
    do
      priorUserSubmit <- priorUserSubmissionIO
      case priorUserSubmit of
        Left e ->
          pure $ Left e
        _ ->
          priorWinLossIO

getMatch :: Database.Handle -> UUID -> IO [MatchWithRelated]
getMatch handle matchID =
  let
    fetchQuery = [sql|SELECT matches.id, start_time, recorded, player1_wins, player2_wins,
                     validated, submitted_by, season, week, date, venue
                     FROM matches join matchups on matches.matchup = matchups.id
                     WHERE matches.id = ?;|]
  in
    Postgres.query (Database.conn handle) fetchQuery (Postgres.Only matchID)

submitMatch :: Database.Handle -> Match -> IO (Either Message [Match])
submitMatch handle match =
  let
    insertQuery = [sql|INSERT INTO matches (id, matchup, start_time, recorded, player1_wins,
                      player2_wins, validated, submitted_by)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                      RETURNING id, matchup, start_time, recorded,
                      player1_wins, player2_wins, validated, submitted_by;|]
    updateOtherMatchQuery = [sql|UPDATE matches
                                SET validated = true
                                WHERE matchup = ? and submitted_by <> ?;|]
  in
    do
      validation <- checkSubmission handle match
      case validation of
        Right Nothing ->
          Right <$> Postgres.query (Database.conn handle) insertQuery (match { validated = False })
        Right (Just (p1, p2)) ->
          Right <$> if ((Just p1, Just p2) == (player1Wins match, player2Wins match)) then
                      Postgres.query (Database.conn handle) insertQuery (match { validated = True }) <*
                      Postgres.execute (Database.conn handle) updateOtherMatchQuery (matchup match, submittedBy match)
                    else
                      Postgres.query (Database.conn handle) insertQuery (match { validated = False })
        (Left e) ->
          pure $ Left e

updateMatch :: Database.Handle -> Match -> IO Int64
updateMatch handle match =
  let
    updateQuery = [sql|UPDATE matches
                      SET player1_Wins = ?, player2_Wins = ?
                      WHERE id = ? AND validated = false;|]
    shouldValidate = [sql|SELECT player1_wins, player2_wins, COUNT(1)
                         FROM matches
                         WHERE matchup = ?
                         GROUP BY (player1_wins, player2_wins); |]
    validateMatches = [sql|UPDATE matches SET validated = true
                          WHERE matchup = ?; |]
  in
    do
      validation <- checkSubmission handle match
      nUpdated <- case validation of
                    -- using a left as a success case? You bet! I'm ashamed, no one look at this line
                    Left MatchAlreadySubmitted ->
                      Postgres.execute (Database.conn handle) updateQuery (matchToUpdate match)
                    _ -> pure 0
      distinctScores <- (
        Postgres.query (Database.conn handle) shouldValidate (Postgres.Only $ matchup match))
        :: IO [(Int, Int, Int)]
      case distinctScores of
        [(_, _, 2)] ->
          Postgres.execute (Database.conn handle) validateMatches (Postgres.Only $ matchup match) *>
          pure nUpdated
        _ ->
          pure nUpdated
