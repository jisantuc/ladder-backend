module Rating ( eloUpdate
              , eloUpdateWithConstant
              , perMatchWinProbability
              , matchSeqWinProbability
              , validateMatchup ) where

import           Data.Matchup                     (Matchup)
import           Debug.Trace
import           Error                            (Message (..))
import           Statistics.Distribution          (cumulative)
import           Statistics.Distribution.Binomial (binomial)

eloUpdateWithConstant :: Int -> Double -> Double -> (Int, Int)
eloUpdateWithConstant k winnerRating loserRating =
  let
    probLoser = ( 1 / ( 1 + 10 ** ((winnerRating - loserRating) / 400)) )
    probWinner = 1 - probLoser
    newWinnerRating = winnerRating + fromIntegral k * (1 - probWinner)
    newLoserRating = loserRating + fromIntegral k * (0 - probLoser)
  in
    (round newWinnerRating, round newLoserRating)

eloUpdate :: Double -> Double -> (Int, Int)
eloUpdate winnerRating loserRating =
  eloUpdateWithConstant 20 winnerRating loserRating

matchSeqLikelihood :: Int -> Int -> Double -> Double -> Double
matchSeqLikelihood gamesWon oppGamesWon rating oppRating = undefined

perMatchWinProbability :: Double -> Double -> Double
perMatchWinProbability opponentRank playerRank =
  ( 1 / ( 1 + 10 ** ((opponentRank - playerRank) / 400)) )

{- Calculate the likelihood that a player with a given likelihood of winning each match
wins the series of n matches, with a required number of matches to win
-}
matchSeqWinProbability :: Int -> Double -> Double -> Double
matchSeqWinProbability nMatches nMatchesMustWin perMatchProbability =
  1 - (cumulative (binomial (nMatches + 1) perMatchProbability) nMatchesMustWin)

validateMatchup :: Double -> Double -> Either Message ()
validateMatchup rating1 rating2 =
  let
    tilt = (matchSeqWinProbability 10 6 $ perMatchWinProbability rating1 rating2)
  in
    if (tilt < 0.25 || 1 - tilt < 0.25) then
      Left $ UnbalancedMatch ("Lower-ranked player has only a " ++
                              show ((tilt `min` 1 - tilt) * 100) ++
                              "% chance to win")
    else
      Right ()
