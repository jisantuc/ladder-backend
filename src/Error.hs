module Error (Message(..)) where

data Message =
  UnbalancedMatch String
  deriving (Eq, Show)
