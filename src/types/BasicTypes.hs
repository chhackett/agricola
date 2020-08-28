module Types.BasicTypes where

----------------------------
--------- Game Data --------
----------------------------

type Round = Int
type Stage = Int
numRounds = 14 :: Round
numStages = 6 :: Stage

data Phase =
    StartRound
  | Replenish
  | Work
  | ReturnHome
  | Harvest
  | HarvestField
  | HarvestFeed
  | HarvestBreed
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type NumPlayers = Int

type NumWorkers = Int
type PlayerId = Int

type BonusPoints = Int

data DeckType = Easy | Interactive | Komplex
  deriving (Show, Read, Eq, Ord)

type Bonus = Int

data NumberOfTimes =
  Any          |   -- Zero or more
  Some         |   -- One or more
  ExactlyZero  |   -- Exactly 0
  ExactlyOnce  |   -- Exactly 1
  ExactlyTwice |   -- Exactly 2
  UpToOnce     |
  UpToTwice
  deriving (Read, Eq, Enum, Ord, Bounded)

instance Show NumberOfTimes where
  show Any = " unlimited times"
  show Some = " at least once"
  show ExactlyZero = " exactly zero times"
  show ExactlyOnce = " exactly once"
  show ExactlyTwice = " exactly twice"
  show UpToOnce = " up to one time"
  show UpToTwice = " up to two times"

data BooleanOp = And | Or | Xor
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

-- A baking bread action is described by how many times it can be used, and how much food is obtained when converting one grain.
-- newtype BakeBreadType = BakeBread (NumberOfTimes, Int)
