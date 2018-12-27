module Error (Message(..)) where

data Message =
  UnbalancedMatch String
  | NotYourMatch
  | MatchAlreadySubmitted
  deriving (Eq, Show)
