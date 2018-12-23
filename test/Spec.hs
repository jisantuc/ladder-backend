{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Rating     (eloUpdate, matchSeqWinProbability)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Ratings adjustments" $ do
    it "matches what's on https://www.geeksforgeeks.org/elo-rating-algorithm/ for an upset" $ do
      eloUpdate 1000 1200 `shouldBe` (1023, 1177)
    it "matches what's on https://www.geeksforgeeks.org/elo-rating-algorithm/ for a non-upset" $ do
      eloUpdate 1200 1000 `shouldBe` (1207, 993)
  describe "Match sequence likelihood calculations" $ do
    it "should think players with equal odds are equally likely to win" $ do
      round (matchSeqWinProbability 10000 5000 0.5 * 100) `shouldBe` 50
    it "should think probability is lower with lower per match probabilities" $ do
      matchSeqWinProbability 10000 5000 0.9 `shouldSatisfy` (\x -> x > matchSeqWinProbability 10000 5000 0.1)
