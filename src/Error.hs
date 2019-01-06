module Error (Message(..)) where

data Message =
  UnbalancedMatch String
  | NotYourMatch
  | MatchAlreadySubmitted
  | BadPassword
  deriving (Eq, Show)
