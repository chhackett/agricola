module Types.BasicGameTypes where

type Round = Int
type Stage = Int
numRounds = 14
numStages = 6

data Phase =
  StartRound |
  Replenish |
  Work |
  ReturnHome |
  Harvest |
  Field |
  Feed |
  Breed |
  EndRound
  deriving (Show, Read, Eq, Enum, Ord, Bounded)
