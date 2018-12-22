{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Rating     (eloUpdate)
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
