module Actions.ResourceActions where

import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import ActionTypes
import Types.ResourceTypes
import Types.BasicTypes
import Types.BasicGameTypes
import Actions.BoardActions
import Actions.AutomaticActions
import Actions.CardActions
import Utils.ListUtils
import Utils.Selection

----------------------------------
----- Starting Player Actions ----
----------------------------------

runStartingPlayerAndStorehouse :: ActionSpaceId -> GameStateT ActionPrimitives
runStartingPlayerAndStorehouse id = do
  runStartingPlayer
  result <- takeResourcesAction id
  return (StartingPlayer : result)

runStartingPlayerAndOrMinorImprovement :: ActionSpaceId -> GameStateT ActionPrimitives
runStartingPlayerAndOrMinorImprovement id = do
  runStartingPlayer
  result <- playMinorImprovement
  return (StartingPlayer : result)

runStartingPlayer :: GameStateT ActionPrimitives
runStartingPlayer = do
  gs <- get
  let pid = currentPlayer gs ^. playerId
  put (gs & nextStartingPlayer .~ pid)
  return [StartingPlayer]

changeStartingPlayer :: GameState -> GameState
changeStartingPlayer gs =
  case find (\p -> _playerId p == _nextStartingPlayer gs) $ _players gs of
    Nothing -> error "Can't find player"
    Just p' -> let ps = filter (\p'' -> _playerId p'' /= _playerId p') $ _players gs in
               gs { _players = p':ps }

-------------------------------
------- Choose 1 Resource -----
-------------------------------

runTakeBuildingResource :: GameStateT ActionPrimitives
runTakeBuildingResource = do
  gs <- get
  let supply = _personalSupply $ currentPlayer gs
      materialOptions = [Wood, Clay, Reed, Stone]
  material <- lift $ getBuildingMaterialChoice materialOptions
  let supply' = addBuildingMaterialToSupply material supply
  put (gs & players . ix 0 . personalSupply .~ supply')
  return [TakeResources [(Material material, 1)]]

---------------------------------
--- Take 2 Building Resources ---
---------------------------------

runTake2DifferentBuildingResources :: GameStateT ActionPrimitives
runTake2DifferentBuildingResources = do
  gs <- get
  let supply = _personalSupply $ currentPlayer gs
      materialOptions = [Wood, Clay, Reed, Stone]
  material1 <- lift $ getBuildingMaterialChoice materialOptions
  let materialOptions' = filter (\e -> e /= material1) materialOptions
  material2 <- lift $ getBuildingMaterialChoice materialOptions'
  let supply' = addBuildingMaterialToSupply material2 $ addBuildingMaterialToSupply material1 supply
  put (gs & players . ix 0 . personalSupply .~ supply')
  return [TakeResources [(Material material1, 1), (Material material2, 1)]]

getBuildingMaterialChoice :: [MaterialType] -> IO MaterialType
getBuildingMaterialChoice mts = do
  let options = map (\mt -> (show mt, mt)) mts
  putStrLn "Select a building resource type:"
  getNextSelection options

addBuildingMaterialToSupply :: MaterialType -> PersonalSupply -> PersonalSupply
addBuildingMaterialToSupply mt supply
  | mt == Wood  = supply & wood %~ (+1)
  | mt == Clay  = supply & clay %~ (+1)
  | mt == Reed  = supply & reed %~ (+1)
  | mt == Stone = supply & stone %~ (+1)

---------------------------------
--- Take Reed, Stone and Food ---
---------------------------------

runTakeReedStoneAndFood :: GameStateT ActionPrimitives
runTakeReedStoneAndFood = do
  modify (\s -> s & players . ix 0 . personalSupply . reed %~ (+1)
                  & players . ix 0 . personalSupply . stone %~ (+1)
                  & players . ix 0 . personalSupply . food %~ (+1))
  return [TakeResources [(Material Reed, 1), (Material Stone, 1), (Food, 1)]]

----------------------------------
-- Take Sheep or Board or Cattle -
----------------------------------

runTakeSheepBoarOrCattle :: GameStateT ActionPrimitives
runTakeSheepBoarOrCattle = do
  gs <- get
  lift $ putStrLn "What would you like to do?"
  at <- lift $ getNextSelection ([("Take 1 Sheep + 1 Food", Sheep), ("Take 1 Boar", Boar)] ++ 
              if currentPlayer gs ^. personalSupply . food > 0 then [("Pay 1 Food for 1 Cattle", Cattle)] else [])
  let foodOp =
        case at of
          Sheep  -> (+1)
          Boar   -> (+0)
          Cattle -> (`subtract` 1)
  b' <- lift $ placeNewAnimals [(at, 1)] $ currentPlayer gs ^. board
  modify (\s -> s & players . ix 0 . personalSupply . food %~ foodOp
                  & players . ix 0 . board .~ b')
  return [TakeResources [(Animal at, 1)]]

----------------------------------
-- Take 2 Building Resources or FamilyGrowth -
----------------------------------

runTake2DifferentBuildingResourcesOrFamilyGrowth :: GameStateT ActionPrimitives
runTake2DifferentBuildingResourcesOrFamilyGrowth = do
  gs <- get
  if _round gs < 5
  then runTake2DifferentBuildingResources
  else do
    lift $ putStrLn "Would you like to take building resources or Family Growth?"
    yes <- lift $ getNextSelection [("Take building resources", True), ("Family Growth", False)]
    if yes then runTake2DifferentBuildingResources else runFamilyGrowth

----------------------------------
-- Build 1 Room Or Traveling Players -
----------------------------------

runBuildRoomOrTravelingPlayers :: ActionSpaceId -> GameStateT ActionPrimitives
runBuildRoomOrTravelingPlayers id = do
  lift $ putStrLn "Would you like to build a room or Traveling Players?"
  yes <- lift $ getNextSelection [("Build a room", True), ("Traveling Players", False)]
  if yes then runBuildRoom else takeResourcesAction id

----------------------------------
-- Take Reed and 1 Stone and 1 Wood -
----------------------------------

runTakeReedand1Stoneand1Wood :: GameStateT ActionPrimitives
runTakeReedand1Stoneand1Wood = do
  modify (\s -> s & players . ix 0 . personalSupply . reed %~ (+1)
                  & players . ix 0 . personalSupply . stone %~ (+1)
                  & players . ix 0 . personalSupply . wood %~ (+1))
  return [TakeResources [(Material Reed, 1), (Material Stone, 1), (Material Wood, 1)]]

getResourcesDescription :: ResourceType -> String
getResourcesDescription rs = "Take all resources of type [" ++ show rs ++ "]"

-------------------------------
--------- Day Laborer ---------
-------------------------------

runDayLaborer :: GameStateT ActionPrimitives
runDayLaborer = do
  modify (\s -> s & players . ix 0 . personalSupply . food %~ (+2))
  return [TakeResources [(Food, 2)]]

runDayLaborerWithBuildingResource :: GameStateT ActionPrimitives
runDayLaborerWithBuildingResource = do
  modify (\s -> s & players . ix 0 . personalSupply . food %~ (+1))
  results <- runTakeBuildingResource
  return (TakeResources [(Food, 1)] : results)

-------------------------------
--------- TakeResources -------
-------------------------------

runTakeGrain :: GameStateT ActionPrimitives
runTakeGrain = do
  modify (\s -> s & players . ix 0 . personalSupply . grain %~ (+1))
  return [TakeResources [(Crop Grain, 1)]]

runTakeVege :: GameStateT ActionPrimitives
runTakeVege = do
  modify (\s -> s & players . ix 0 . personalSupply . veges %~ (+1))
  return [TakeResources [(Crop Veges, 1)]]

-------------------------------
--------- TakeResources -------
-------------------------------

takeResourcesAction :: ActionSpaceId -> GameStateT ActionPrimitives
takeResourcesAction id = do
  gs <- get
  (gs', rs) <- lift $ giveResourcesToCurrentPlayer id gs
  put gs'
  return [TakeResources rs | not (null rs)]

-- Take all resources from the action space specified by the action type and give them to the current player
giveResourcesToCurrentPlayer :: ActionSpaceId -> GameState -> IO (GameState, Resources)
giveResourcesToCurrentPlayer id gs = do
  let asm = _actionSpaceMap gs
  case M.lookup id asm of
    Nothing -> error "Unable to find action space"
    Just as -> do
      let rs = _resources as
      cp' <- giveResourcesToPlayer rs $ currentPlayer gs
      let as' = as & resources .~ []
          asm' = M.insert id as' asm
      return (gs & players . ix 0 .~ cp'
                 & actionSpaceMap .~ asm', rs)
  where
    giveResourcesToPlayer :: Resources -> Player -> IO Player
    giveResourcesToPlayer [] p = return p
    giveResourcesToPlayer ((rt, n) : rs) p =
      case rt of
        Food           -> return $ p & personalSupply . food +~ n
        Crop Grain     -> return $ p & personalSupply . grain +~ n
        Crop Veges     -> return $ p & personalSupply . veges +~ n
        Material Wood  -> return $ p & personalSupply . wood +~ n
        Material Clay  -> return $ p & personalSupply . clay +~ n
        Material Reed  -> return $ p & personalSupply . reed +~ n
        Material Stone -> return $ p & personalSupply . stone +~ n
        Animal at      -> giveAnimalToPlayer (at, n) p

giveAnimalToPlayer :: Animal -> Player -> IO Player
giveAnimalToPlayer (at,n) p = do
  b <- placeNewAnimals [(at, n)] (_board p)
  return (p & board .~ b)

putCurrentPlayerWorkerOnActionSpace :: ActionSpaceId -> GameState -> GameState
putCurrentPlayerWorkerOnActionSpace actionId gs =
  let p = currentPlayer gs
      n = _workers p in
  if n > 0
  then
    let asm' = addWorker actionId (_playerId p) $ _actionSpaceMap gs in
    gs & players . ix 0 . workers .~ (n - 1)
       & actionSpaceMap .~ asm'
  else error "No workers left to put on action space"

addWorker :: ActionSpaceId -> PlayerId -> ActionSpaceMap -> ActionSpaceMap
addWorker actionId pid asm =
  case M.lookup actionId asm of
    Nothing -> error ("Unable to find action space for action id " ++ show actionId)
    Just as ->
      let wm = _workersMap as in
      case M.lookup pid wm of
        Nothing -> M.insert actionId (as { _workersMap = M.insert pid 1 wm }) asm
        Just n  -> M.insert actionId (as { _workersMap = M.insert pid (n + 1) wm }) asm
