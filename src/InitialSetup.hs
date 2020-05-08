module InitialSetup where

import System.Random
import System.Random.Shuffle
import Types.BasicGameTypes
import Types.PlayerData
import Types.ResourceTypes
import Types.GameState
import Actions.ResourceActions

initGameState :: (RandomGen g) => g -> String -> GameState
initGameState g name =
  let (g1, g2) = split g
      (g3, g4) = split g2
      occupations = getSevenRandoms g1 :: OccupationTypes
      improvements = getSevenRandoms g2 :: MinorImprovementTypes
      pId = 0
      player = Player pId
                      name
                      (Board ([(0,0),(0,1)], Wood) [] [] [])
                      2     -- workers
                      []
                      (occupations, improvements)
                      ([], [], []) in
  GameState 1
            StartRound
            player
            [player]
            (filter isStartingActionSpace initActionSpaces)
            (initFutureActionCards g3)
            []

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g

isStartingActionSpace :: ActionSpace -> Bool
isStartingActionSpace a = actionType a `elem` startingActionTypes
startingActionTypes = [BuildRoomAndOrStables .. Fishing]

-- Initialize the actions space cards
initActionSpaces :: ActionSpaces
initActionSpaces =
  [ ActionSpace BuildRoomAndOrStables [] ifNoWorkers noop [],
    ActionSpace StartingPlayerAndOrMinorImprovement [] ifNoWorkers noop [],
    ActionSpace Take1Grain [] ifNoWorkers noop [],
    ActionSpace Plow1Field [] ifNoWorkers noop [],
    ActionSpace PlayOneOccupation [] ifNoWorkers noop [],
    ActionSpace DayLaborer [] ifNoWorkers noop [],
    ActionSpace TakeWood [] ifNoWorkers (giveResourcesOfTypeToCurrentPlayer Wood TakeWood) [],
    ActionSpace TakeClay [] ifNoWorkers (giveResourcesOfTypeToCurrentPlayer Clay TakeClay) [],
    ActionSpace TakeReed [] ifNoWorkers (giveResourcesOfTypeToCurrentPlayer Reed TakeReed) [],
    ActionSpace Fishing [] ifNoWorkers (giveResourcesOfTypeToCurrentPlayer Food Fishing) [],
    ActionSpace SowAndOrBakeBread [] ifNoWorkers noop [],
    ActionSpace TakeSheep [] ifNoWorkers noop [],
    ActionSpace Fences [] ifNoWorkers noop [],
    ActionSpace MajorOrMinorImprovement [] ifNoWorkers noop [],
    ActionSpace AfterFamilyGrowthAlsoImprovement [] ifNoWorkers noop [],
    ActionSpace AfterRenovationAlsoImprovement [] ifNoWorkers noop [],
    ActionSpace TakeStone [] ifNoWorkers noop [],
    ActionSpace TakeBoar [] ifNoWorkers noop [],
    ActionSpace TakeVege [] ifNoWorkers noop [],
    ActionSpace TakeStone [] ifNoWorkers noop [],
    ActionSpace TakeCattle [] ifNoWorkers noop [],
    ActionSpace PlowAndOrSow [] ifNoWorkers noop [],
    ActionSpace FamilyGrowthWithoutRoom [] ifNoWorkers noop [],
    ActionSpace AfterRenovationAlsoFences [] ifNoWorkers noop [] ]

-- initialize future action cards list with random generator
initFutureActionCards :: (RandomGen g) => g -> ActionSpaces
initFutureActionCards g = map getActionSpace $ snd $ foldl randomize (g, []) actionCardStageMap
  where randomize (g', rcs) rc = let (g1, g2) = split g' in (g1, rcs ++ shuffle' rc (length rc) g2)

getActionSpace :: ActionType -> ActionSpace
getActionSpace at =
  let maybeA = getActionSpaceFromType at initActionSpaces in
  case maybeA of
    Nothing -> ActionSpace Fishing [] ifNoWorkers noop []
    Just actionSpace -> actionSpace
  
actionCardStageMap :: [[ActionType]]
actionCardStageMap =
  [[SowAndOrBakeBread, TakeSheep, Fences, MajorOrMinorImprovement],
    [AfterFamilyGrowthAlsoImprovement, AfterRenovationAlsoImprovement, TakeStone],
    [TakeBoar, TakeVege],
    [TakeStone, TakeCattle],
    [PlowAndOrSow, FamilyGrowthWithoutRoom],
    [AfterRenovationAlsoFences]]

ifNoWorkers :: GameState -> ActionSpace -> Bool
ifNoWorkers gs a = let w = Types.GameState.workers a in null w

alwaysAllowed :: GameState -> Bool
alwaysAllowed gs = True

noop :: GameAction
noop gs = gs
