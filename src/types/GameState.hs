module Types.GameState where

import System.Random
import System.Random.Shuffle
import Types.BasicGameTypes
import Types.PlayerData
import Types.ResourceTypes
import Types.ActionTypes
import Types.CardData

data ActionSpaceState = ActionSpaceState
  { hasWorkers :: [PlayerId]
  , hasResources :: Resources }
  deriving (Show, Read, Eq)

type ActionSpaceStates = [ActionSpaceState]

data GameState = GameState { round :: Round
                           , phase :: Phase
                           , currentPlayer :: Player
                           , players :: Players
                           , actionSpaceStates :: ActionSpaceStates
                           , availableRoundCards :: [RoundCardType]
                           , remainingRoundCards :: [RoundCardType] }
  deriving (Show, Read)

initGameState :: (RandomGen g) => g -> GameState
initGameState g =
  let (g1, g2) = split g
      occupations = getSevenRandoms g1 :: OccupationTypes
      improvements = getSevenRandoms g2 :: ImprovementTypes
      player = Player 0
                      (Board ([(0,0),(0,1)], Wood) [] [] [])
                      2   -- workers
                      0   -- money
                      0   -- food
                      []  -- crops
                      []  -- materials
                      (occupations, improvements)
                      ([], []) in
  GameState 1
            StartRound
            player
            [player]
            []
            []
            []

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g
