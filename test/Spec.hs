module Main (main) where

import           Data.Int                         (Int64)
import           Data.Ladder.Season
import           Data.Ladder.Time
import qualified Data.UUID.V4                     as UUIDv4
import qualified Database.Ladder                  as Database
import           Database.Ladder.Season
import qualified Database.PostgreSQL.Simple       as Postgres
import           Database.PostgreSQL.Simple.SqlQQ
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
  (\_ -> ()) <$> truncateTables

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

dbSpec :: Assertion
dbSpec = do
  handle <- defaultHandle
  season <- (\seasonID -> Season seasonID 2019 Summer) <$> UUIDv4.nextRandom
  created <- createSeason handle season
  listed <- getCurrentSeason handle
  assertEqual "return from created should match return from latest" created listed
  assertEqual "return from created should be the same as source season" created [season]

truncateTables :: IO Int64
truncateTables = do
  handle <- defaultHandle
  Postgres.execute_ (Database.conn handle) [sql|TRUNCATE TABLE seasons CASCADE;|]
