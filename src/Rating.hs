module Rating ( eloUpdate
              , eloUpdateWithConstant
              , matchSeqLikelihood
              , matchSeqWinProbability
              , perMatchWinProbability
              , race
              , validateMatchup ) where

import           Debug.Trace
import           Error                            (Message (..))
import           Statistics.Distribution          (cumulative, probability)
import           Statistics.Distribution.Binomial (binomial)

race :: Int
race = 6

eloUpdateWithConstant :: Int -> Int -> Double -> Double -> (Int, Int)
eloUpdateWithConstant k oppGamesWon winnerRating loserRating =
  let
    probWinner = matchSeqLikelihood oppGamesWon winnerRating loserRating
    probLoser = 1 - probWinner
    newWinnerRating = winnerRating + fromIntegral k * (1 - probWinner)
    newLoserRating = loserRating + fromIntegral k * (0 - probLoser)
  in
    (round newWinnerRating, round newLoserRating)

eloUpdate :: Int -> Double -> Double -> (Int, Int)
eloUpdate oppGamesWon winnerRating loserRating =
  eloUpdateWithConstant 20 oppGamesWon winnerRating loserRating

matchSeqLikelihood :: Int -> Double -> Double -> Double
matchSeqLikelihood oppGamesWon rating oppRating =
  let
    favoredWon = rating > oppRating
    perMatchSuccessRate =
      perMatchWinProbability rating oppRating
    winnerPerMatchSuccess =
      if (favoredWon) then
        perMatchSuccessRate
      else
        1 - perMatchSuccessRate
  in
    cumulative (binomial (race + oppGamesWon) winnerPerMatchSuccess) (fromIntegral oppGamesWon)

{-Calculate the likelihood that the higher-ranked of two elo ratings wins a rack
-}
perMatchWinProbability :: Double -> Double -> Double
perMatchWinProbability rank1 rank2 =
  let
    highRank = rank1 `max` rank2
    lowRank = rank1 `min` rank2
  in
    ( 1 / ( 1 + 10 ** ((highRank - lowRank) / 400)) )

{- Calculate the likelihood that a player with a given likelihood of winning each match
wins the series of n matches, with a required number of matches to win
-}
matchSeqWinProbability :: Int -> Double -> Double -> Double
matchSeqWinProbability nMatches nMatchesMustWin perMatchProbability =
  1 - (cumulative (binomial (nMatches + 1) perMatchProbability) nMatchesMustWin)

{- Reject matchups if the favored player is more than 75% likely to win based on elo
-}
validateMatchup :: Double -> Double -> Either Message ()
validateMatchup rating1 rating2 =
  let
    tilt = (matchSeqWinProbability 11 6 $ perMatchWinProbability rating1 rating2)
  in
    if (tilt < 0.25) then
      Left $ UnbalancedMatch ("Lower-ranked player has only a " ++
                              show (round ((tilt `min` (1 - tilt)) * 100)) ++
                              "% chance to win")
    else
      Right ()
