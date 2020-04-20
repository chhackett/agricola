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
  , currentPlayer :: PlayerId
  , players :: Players
  , currentActionStates :: ActionStates
  , futureActionCards :: Actions
  , availableMajorImprovements :: MajorImprovementTypes }
  deriving (Show)

initGameState :: (RandomGen g) => g -> String -> GameState
initGameState g pn =
  let (g1, g2) = split g
      (g3, g4) = split g2
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
            0
            [player]
            initializeActionStates
            (initFutureActionCards g3)
            []

-- Initialize the actions space cards
initActionStates :: ActionStates
initActionStates =
  let actions = [minBound .. maxBound] :: Actions
      actions' = filter (\action -> getActionType action == ActionSpace) actions
      buildActionState action = ActionState ActionSpace action "" [] (0, 0, [], [], []) [] in
  map buildActionState actions'

initFutureActionCards :: (RandomGen g) => g -> Actions
initFutureActionCards g =
  let stages = [0 .. 5]
      gs = []
      -- actions = [minBound .. maxBound] :: Actions
      -- roundActions = filter (\action -> getActionType action == RoundSpace) actions
      randomize as g' = let (g1, g2) = split g' in (g1, shuffle' as (length as) g2) in
      map (\s -> randomize (roundCardStages !! s, g)) stages

generateSomeGenerators :: (RandomGen g) => g -> Int -> [g]
generateSomeGenerators g n = splitAdd gs

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g
