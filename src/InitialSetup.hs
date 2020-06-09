module InitialSetup where

import qualified Data.Map as M
import System.Random
import System.Random.Shuffle
import Types.BasicGameTypes
import Types.PlayerData
import Types.GameState as GS
import Actions.GameActionFuncs
import Actions.ResourceActions
import Actions.BoardActions
import Utils.ListUtils

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
            [player]
            (filter isStartingActionSpace initActionSpaces)
            (initFutureActionCards g3)
            []
            actionTypeToGameActionMap

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g

isStartingActionSpace :: ActionSpace -> Bool
isStartingActionSpace a = GS._actionType a `elem` startingActionTypes
startingActionTypes = [BuildRoomAndOrStables .. Fishing]

-- Initialize the actions space cards
initActionSpaces :: ActionSpaces
initActionSpaces =
  [ ActionSpace BuildRoomAndOrStables "BuildRoomAndOrStables" [] [],
    ActionSpace StartingPlayerAndOrMinorImprovement "StartingPlayerAndOrMinorImprovement" [] [],
    ActionSpace Take1Grain "Take 1 Grain" [] [],
    ActionSpace Plow1Field "Plow 1 Field" [] [],
    ActionSpace PlayOneOccupation "Play 1 Occupation. First Costs 1, the rest Cost 2" [] [],
    ActionSpace DayLaborer "DayLaborer" [] [],
    ActionSpace TakeWood (getResourcesDescription Wood) [] [],
    ActionSpace TakeClay (getResourcesDescription Clay) [] [],
    ActionSpace TakeReed (getResourcesDescription Reed) [] [],
    ActionSpace Fishing (getResourcesDescription Food) [] [],
    ActionSpace SowAndOrBakeBread "SowAndOrBakeBread" [] [],
    ActionSpace TakeSheep (getResourcesDescription Sheep) [] [],
    ActionSpace Fences "Fences" [] [],
    ActionSpace MajorOrMinorImprovement "MajorOrMinorImprovement" [] [],
    ActionSpace AfterFamilyGrowthAlsoImprovement "AfterFamilyGrowthAlsoImprovement" [] [],
    ActionSpace AfterRenovationAlsoImprovement "AfterRenovationAlsoImprovement" [] [],
    ActionSpace TakeBoar (getResourcesDescription Boar) [] [],
    ActionSpace TakeVege "Take 1 Vege" [] [],
    ActionSpace TakeStone1 (getResourcesDescription Stone) [] [],
    ActionSpace TakeStone2 (getResourcesDescription Stone) [] [],
    ActionSpace TakeCattle (getResourcesDescription Cattle) [] [],
    ActionSpace PlowAndOrSow "PlowAndOrSow" [] [],
    ActionSpace FamilyGrowthWithoutRoom "FamilyGrowthWithoutRoom" [] [],
    ActionSpace AfterRenovationAlsoFences "AfterRenovationAlsoFences" [] [] ]

-- initialize future action cards list with random generator
initFutureActionCards :: (RandomGen g) => g -> ActionSpaces
initFutureActionCards g = map getActionSpace $ snd $ foldl randomize (g, []) actionCardStageMap
  where randomize (g', rcs) rc = let (g1, g2) = split g' in (g1, rcs ++ shuffle' rc (length rc) g2)

getActionSpace :: ActionType -> ActionSpace
getActionSpace at =
  let maybeA = getFirstElem GS._actionType at initActionSpaces in
  case maybeA of
    Nothing -> ActionSpace Fishing (getResourcesDescription Food) [] []   -- Fix this!
    Just actionSpace -> actionSpace

actionCardStageMap :: [[ActionType]]
actionCardStageMap =
  [ [SowAndOrBakeBread, TakeSheep, Fences, MajorOrMinorImprovement],
    [AfterFamilyGrowthAlsoImprovement, AfterRenovationAlsoImprovement, TakeStone1],
    [TakeBoar, TakeVege],
    [TakeStone2, TakeCattle],
    [PlowAndOrSow, FamilyGrowthWithoutRoom],
    [AfterRenovationAlsoFences] ]

actionSpaceRunList :: [(ActionType, GameStateT ())]
actionSpaceRunList =
  [ (BuildRoomAndOrStables,               runBuildRoomAndOrStables)
  , (StartingPlayerAndOrMinorImprovement, noIO)
  , (Take1Grain,                          noIO)
  , (Plow1Field,                          runPlowFieldAction)
  , (PlayOneOccupation,                   noIO)
  , (DayLaborer,                          noIO)
  , (TakeWood,                            giveResourcesAction Wood TakeWood)
  , (TakeClay,                            giveResourcesAction Clay TakeClay)
  , (TakeReed,                            giveResourcesAction Reed TakeReed)
  , (Fishing,                             giveResourcesAction Food Fishing)
  , (SowAndOrBakeBread,                   runSowAndOrBakeBreadAction)
  , (TakeSheep,                           giveResourcesAction Sheep TakeSheep)
  , (Fences,                              runFencesAction)
  , (MajorOrMinorImprovement,             noIO)
  , (AfterFamilyGrowthAlsoImprovement,    noIO)
  , (AfterRenovationAlsoImprovement,      noIO)
  , (TakeStone1,                          giveResourcesAction Stone TakeStone1)
  , (TakeStone2,                          giveResourcesAction Stone TakeStone2)
  , (TakeBoar,                            giveResourcesAction Boar TakeBoar)
  , (TakeVege,                            noIO)
  , (TakeCattle,                          giveResourcesAction Cattle TakeCattle)
  , (PlowAndOrSow,                        noIO)
  , (FamilyGrowthWithoutRoom,             noIO) 
  , (AfterRenovationAlsoFences,           noIO)
  ]

actionTypeToGameActionMap :: GameActionMap
actionTypeToGameActionMap = foldl buildGameAction M.empty actionSpaceRunList
  where buildGameAction m x = M.insert (fst x) (GameAction (ifNoWorkers (fst x)) (snd x)) m

