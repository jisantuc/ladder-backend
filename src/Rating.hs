module Rating ( eloUpdate ) where

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
