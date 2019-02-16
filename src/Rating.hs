module Rating ( eloUpdate
              , eloUpdateWithConstant
              , perMatchWinProbability
              , validateMatchup ) where

import           Debug.Trace
import           Error                            (Message (..))
import           Statistics.Distribution          (cumulative, probability)
import           Statistics.Distribution.Binomial (binomial)

eloUpdateWithConstant :: (RealFrac a, Floating a, Ord a) => a -> a -> a -> a -> a -> (a, a)
eloUpdateWithConstant k playerGamesWon oppGamesWon playerRating oppRating =
  let
    totalGames = playerGamesWon + oppGamesWon
    probWinner = perMatchWinProbability playerRating oppRating
    playerExpectation = probWinner * totalGames
    playerAdjustment = k * (playerGamesWon - playerExpectation)
    oppAdjustment = k * (oppGamesWon - (totalGames - playerExpectation))
  in
    (playerRating + playerAdjustment, oppRating + oppAdjustment)

eloUpdate :: Double -> Double -> Double -> Double -> (Double, Double)
eloUpdate playerGamesWon oppGamesWon winnerRating loserRating =
  eloUpdateWithConstant 20 playerGamesWon oppGamesWon winnerRating loserRating

{-Calculate the likelihood that the higher-ranked of two elo ratings wins a rack
-}
perMatchWinProbability :: (Floating a, Ord a) => a -> a -> a
perMatchWinProbability playerRating oppRating =
  ( 1 / ( 1 + 10 ** ((oppRating - playerRating) / 400)) )

{- Reject matchups if the lower-ranked player is more than 75% likely to win based on elo
-}
validateMatchup :: Double -> Double -> Either Message ()
validateMatchup rating1 rating2 =
  let
    tilt = perMatchWinProbability rating1 rating2
    underdogChance = tilt `min` (1 - tilt)
  in
    if (underdogChance * 11 <= 4) then
      Left $ UnbalancedMatch ("Lower-ranked player has only a " ++
                              show (round ((tilt `min` (1 - tilt)) * 100)) ++
                              "% chance to win each rack")
    else
      Right ()
