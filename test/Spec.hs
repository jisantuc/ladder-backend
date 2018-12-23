module Main (main) where

import           Rating     ( eloUpdateWithConstant
                            , matchSeqWinProbability
                            , matchSeqLikelihood
                            , validateMatchup
                            , race)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Ratings adjustments" $ do
    it "matches what's on https://www.geeksforgeeks.org/elo-rating-algorithm/ for an upset" $ do
      eloUpdateWithConstant 30 1000 1200 `shouldBe` (1023, 1177)
    it "matches what's on https://www.geeksforgeeks.org/elo-rating-algorithm/ for a non-upset" $ do
      eloUpdateWithConstant 30 1200 1000 `shouldBe` (1207, 993)
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
      matchSeqLikelihood (race - 1) 5000 1000 `shouldSatisfy` (\x -> x < matchSeqLikelihood (race - 3) 5000 1000)
