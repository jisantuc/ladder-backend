module Rating ( eloUpdate
              , perMatchWinProbability
              , matchSeqWinProbability
              , makeFairMatch ) where

import           Data.Matchup                     (Matchup)
import           Statistics.Distribution          (cumulative)
import           Statistics.Distribution.Binomial (binomial)

data FairMatch = FairMatch { lowerRankedMustWin            :: Int
                           , higherRankedMustWin           :: Int
                           , lowerRankedVictoryProbability :: Double } deriving (Eq, Show)

eloUpdate :: Double -> Double -> (Int, Int)
eloUpdate winnerRating loserRating =
  let
    -- constant -- scaling factor for score adjustments
    -- gives 15 points to winner in even matches, which seems fine
    k = 30
    -- prior likelihood that the winner would have lost the match
    probLoser = ( 1 / ( 1 + 10 ** ((winnerRating - loserRating) / 400)) )
    probWinner = 1 - probLoser
    newWinnerRating = winnerRating + k * (1 - probWinner)
    newLoserRating = loserRating + k * (0 - probLoser)
  in
    (round newWinnerRating, round newLoserRating)

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

makeFairMatch :: Double -> Double -> FairMatch
makeFairMatch rating1 rating2 =
  let
    lowerRankedPerMatchProbability =
      perMatchWinProbability (rating1 `max` rating2) (rating1 `min` rating2)
    m = 5
  -- problem: choose a number of matches n and a win condition out of those matches m
  -- such that:
  --
  -- cumulative (binomial n m) ~= 50%
  in
    go m lowerRankedPerMatchProbability
  where
    go numberOfMatches perMatchSuccessRate
      | abs(matchSeqWinProbability 10 numberOfMatches perMatchSuccessRate - 0.5) <= 0.05 =
        FairMatch (round numberOfMatches) (round $ 10 - numberOfMatches) (matchSeqWinProbability 10 numberOfMatches perMatchSuccessRate)
      | otherwise =
        go (numberOfMatches - 1) perMatchSuccessRate
