module Types.GameState where

import System.Random
import System.Random.Shuffle
import Types.BasicGameTypes
import Types.PlayerData
import Types.ResourceTypes
import Types.ActionTypes

data GameState = GameState
  { _round :: Round
  , _phase :: Phase
  , _currentPlayer :: PlayerId
  , _players :: Players
  , _currentActionStates :: ActionStates
  , _futureActionCards :: Actions
  , _availableMajorImprovements :: MajorImprovementTypes }
  deriving (Show)

initGameState :: (RandomGen g) => g -> String -> GameState
initGameState g pn =
  let (g1, g2) = split g
      (g3, g4) = split g2
      occupations = getSevenRandoms g1 :: OccupationTypes
      improvements = getSevenRandoms g2 :: MinorImprovementTypes
      pId = 0
      player = Player pId
                      pn
                      (Board ([(0,0),(0,1)], Wood) [] [] [])
                      2     -- workers
                      ( 0   -- food
                      , 2   -- money
                      , []  -- materials
                      , []) -- crops
                      ( occupations
                      , improvements)
                      ( []
                      , []
                      , []) in
  GameState 1
            StartRound
            pId
            [player]
            initActionStates
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
initFutureActionCards g = snd $ foldl randomize (g, []) roundCardStages
  where randomize (g', rcs) rc = let (g1, g2) = split g' in (g1, rcs ++ shuffle' rc (length rc) g2)

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g
