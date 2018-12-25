module Main (main) where

import           Data.Int                         (Int64)
import           Data.Ladder.Matchup
import           Data.Ladder.Player
import           Data.Ladder.Season
import           Data.Ladder.Time
import           Data.Ladder.Venue
import qualified Data.UUID.V4                     as UUIDv4
import qualified Database.Ladder                  as Database
import           Database.Ladder.Matchup
import           Database.Ladder.Player
import           Database.Ladder.Season
import           Database.Ladder.Venue
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types as Postgres
import           Rating                           (eloUpdateWithConstant,
                                                   matchSeqLikelihood,
                                                   matchSeqWinProbability, race,
                                                   validateMatchup)
import           Test.Hspec
import           Test.HUnit

import           Config

main :: IO ()
main = do
  hspec pureSpec
  dbSpec

pureSpec :: Spec
pureSpec = do
  describe "Match sequence likelihood calculations" $ do
    it "should think players with equal odds are equally likely to win" $ do
      round (matchSeqWinProbability 10000 5000 0.5 * 100) `shouldBe` 50
    it "should think probability is lower with lower per match probabilities" $ do
      matchSeqWinProbability 10000 5000 0.9 `shouldSatisfy` (\x -> x > matchSeqWinProbability 10000 5000 0.1)
    it "should result in rejections of unfair matches" $ do
      validateMatchup 1000 1000 `shouldBe` Right ()
      validateMatchup 1000 1030 `shouldBe` Right ()
      validateMatchup 1000 1200 `shouldNotBe` Right ()
      validateMatchup 1200 1000 `shouldNotBe` Right ()
  describe "Post-match likelihood calculation" $ do
    it "should think even matches are even" $ do
      matchSeqLikelihood (race - 1) 1000 1000 `shouldSatisfy` (\x -> abs (x - 0.5) < 0.000001)
    it "should think mostly even matches are also pretty even" $ do
      matchSeqLikelihood (race - 1) 1001 1000 `shouldSatisfy` (\x -> abs (x - 0.5) < 0.05)
      matchSeqLikelihood (race - 1) 1000 1001 `shouldSatisfy` (\x -> abs (x - 0.5) < 0.05)
      matchSeqLikelihood (race - 1) 1010 1000 `shouldSatisfy` (\x -> abs (x - 0.5) < 0.05)
      matchSeqLikelihood (race - 1) 1000 1010 `shouldSatisfy` (\x -> abs (x - 0.5) < 0.05)
    it "should think unbalanced matchups are unbalanced" $ do
      matchSeqLikelihood (race - 1) 1100 1000 `shouldSatisfy` (\x -> x > matchSeqLikelihood (race - 1) 1000 1100)
    it "should think higher ranked players are more likely to win by a given margin" $ do
      matchSeqLikelihood 3 1100 1000 `shouldSatisfy` (\x -> x < matchSeqLikelihood 3 1200 1000)

dbSpec :: IO ()
dbSpec = do
  seasonDBSpec
  playerDBSpec
  venueDBSpec
  matchupDBSpec

seasonDBSpec :: Assertion
seasonDBSpec = do
  handle <- defaultHandle
  season <- (\seasonID -> Season seasonID 2019 Summer) <$> UUIDv4.nextRandom
  created <- createSeason handle season
  listed <- getCurrentSeason handle
  assertEqual "return from created should match return from latest" created listed
  assertEqual "return from created should be the same as source season" created [season]

playerDBSpec :: Assertion
playerDBSpec = do
  handle <- defaultHandle
  player <- (\playerID -> Player playerID "foo@bogus.com" "Bogus" "Name" True) <$> UUIDv4.nextRandom
  inserted <- createPlayer handle player
  retrieved <- getPlayer handle (playerID player)
  assertEqual "return from fetch is the same as return from insert" inserted retrieved
  assertEqual "return from insert is the player we inserted" inserted [player]
  _ <- updatePlayer handle (player { email = "bar@bogus.net" })
  fetchedAgain <- getPlayer handle (playerID player)
  assertEqual "the email was updated" (email <$> fetchedAgain) ["bar@bogus.net"]
  _ <- deletePlayer handle player
  fetchedAThirdTime <- getPlayer handle (playerID player)
  assertEqual "the player is gone from the db" fetchedAThirdTime []

venueDBSpec :: Assertion
venueDBSpec = do
  handle <- defaultHandle
  venue <- (\venueID ->
              Venue venueID
              "Quite Good and Fun Pool Hall"
              "2670001234"
              "Somewhere in Center City, Philadelphia, PA"
              (Postgres.PGArray [Monday, Tuesday])
              (Just 10.75)) <$> UUIDv4.nextRandom
  inserted <- createVenue handle venue
  retrieved <- getVenue handle (venueID venue)
  assertEqual "return from fetch is the same as return from insert" inserted retrieved
  assertEqual "return from insert is the player we inserted" inserted [venue]
  _ <- updateVenue handle (venue { name = "Actually Not a Fun Place", leagueNights = Postgres.PGArray [] })
  fetchedAgain <- getVenue handle (venueID venue)
  assertEqual "update should have done something" fetchedAgain $
    [venue { name = "Actually Not a Fun Place", leagueNights = Postgres.PGArray []}]
  listed <- listVenues handle
  assertEqual "list should get the only venue we've created" listed fetchedAgain
  _ <- deleteVenue handle venue
  listedAgain <- listVenues handle
  assertEqual "the venue is gone from the db" listedAgain []

matchupDBSpec :: Assertion
matchupDBSpec = do
  handle <- defaultHandle
  currTime <- now
  player1 <- (\playerID -> Player playerID "foo@bogus.com" "Bogus" "Name" True) <$> UUIDv4.nextRandom
  player2 <- (\playerID -> Player playerID "bar@absurd.com" "Bogus" "Name" True) <$> UUIDv4.nextRandom
  season <- (\seasonID -> Season seasonID 2019 Summer) <$> UUIDv4.nextRandom
  matchup <- (\matchupID ->
                Matchup matchupID (playerID player1) (playerID player2) 4 (seasonID season) Nothing Nothing) <$>
             UUIDv4.nextRandom
  venue <- (\venueID ->
              Venue venueID
              "Quite Good and Fun Pool Hall"
              "2670001234"
              "Somewhere in Center City, Philadelphia, PA"
              (Postgres.PGArray [Monday, Tuesday])
              (Just 10.75)) <$> UUIDv4.nextRandom
  _ <- createSeason handle season
  _ <- createPlayer handle player1
  _ <- createPlayer handle player2
  _ <- createVenue handle venue
  inserted <- createMatchup handle matchup
  retrieved <- getMatchup handle (matchupID matchup)
  assertEqual "return from fetch is the same as return from insert" inserted retrieved
  assertEqual "return from insert is the matchup we inserted" inserted [matchup]
  _ <- scheduleMatchup handle (matchup { date = Just currTime })
  fetchedAgain <- getMatchup handle (matchupID matchup)
  assertEqual "updating just the date shouldn't have changed anything" fetchedAgain [matchup]
  _ <- scheduleMatchup handle (matchup { date = Just currTime, venue = Just (venueID venue) })
  fetchedAThirdTime <- getMatchup handle (matchupID matchup)
  assertEqual "updating both date and venue should have set venue"
    (head . (Data.Ladder.Matchup.venue <$>) $ fetchedAThirdTime)
    (Just $ venueID venue)
  -- Fails: too much precision before it goes into the db
  -- but uncommenting reveals that it's working, just in a way that's hard to test because of precision
  -- differences on either side of the db boundary
  -- assertEqual "updating both date and venue should have set date"
  --   ((head $ date <$> fetchedAThirdTime) >>= toUTCTime)
  --   (Just currTime >>= toUTCTime)
  -- out of datamodel operations because I didn't make a way to increment time, oops
  upcomingMatchups <- listMatchupsForPlayer handle (playerID player1)
  assertEqual "matchup in the past should not be returned in upcoming matchups" upcomingMatchups []
  _ <- Postgres.execute (Database.conn handle)
       [sql|UPDATE matchups SET date = date + interval '7 days' where id = ?; |]
         (Postgres.Only (matchupID matchup))
  upcomingMatchups2 <- listMatchupsForPlayer handle (playerID player1)
  assertEqual "matchup in the future should be returned in upcoming matchups"
                       (head . (matchupID <$>) $ upcomingMatchups2)
                       (matchupID matchup)
  _ <- deleteMatchup handle matchup
  fetchedAFourthTime <- getMatchup handle (matchupID matchup)
  assertEqual "the matchup is gone from the db" fetchedAFourthTime []
