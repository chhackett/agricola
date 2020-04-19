module Types.GameState where

import System.Random
import System.Random.Shuffle
import Types.BasicGameTypes
import Types.PlayerData
import Types.ResourceTypes
import Types.ActionTypes

data GameState = GameState
  { round :: Round
  , phase :: Phase
  , currentPlayer :: Player
  , players :: Players
  , currentActionStates :: ActionStates
  , futureActionCards :: Actions
  , majorImprovements :: MajorImprovementTypes }
  deriving (Show)

initGameState :: (RandomGen g) => g -> String -> GameState
initGameState g pn =
  let (g1, g2) = split g
      occupations = getSevenRandoms g1 :: OccupationTypes
      improvements = getSevenRandoms g2 :: MinorImprovementTypes
      player = Player 0
                      pn
                      (Board ([(0,0),(0,1)], Wood) [] [] [])
                      2     -- workers
                      ( 0   -- food
                      , 2   -- money
                      , []  -- materials
                      , []) -- crops
                      (occupations, improvements)
                      ([], [], []) in
  GameState 1
            StartRound
            player
            [player]
            []
            []
            []

-- Initialize the actions space cards
initializeActionStates :: ActionStates
initializeActionStates =
  let actions = [minBound .. maxBound] :: Actions
      actions' = filter (\action -> getActionType action == ActionSpace) actions
      buildActionState action = ActionState ActionSpace action "" [] (0, 0, [], [], []) [] in
  map buildActionState actions'

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g
