module Actions.ResourceActions where

import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import Types.BasicGameTypes
import Actions.BoardActions
import Utils.ListUtils
import Utils.Selection

-------------------------------
------- Choose 1 Resource -----
-------------------------------

runTakeBuildingResource :: GameStateT ActionPrimitives
runTakeBuildingResource = do
  gs <- get
  let ps = _players gs
      p = head ps
      supply = _personalSupply p
  material <- lift getBuildingMaterialChoice
  let supply' = addBuildingMaterialToSupply material supply
      p' = p { _personalSupply = supply' }
      ps' = p' : tail ps
  put (gs { _players = ps' })
  return [TakeResources [(Material material, 1)]]

---------------------------------
--- Take 2 Building Resources ---
---------------------------------

runTake2DifferentBuildingResources :: GameStateT ActionPrimitives
runTake2DifferentBuildingResources = return []

getBuildingMaterialChoice :: IO MaterialType
getBuildingMaterialChoice = do
  let options = getResourceOptions
  putStrLn "Select a building resource type:"
  getNextSelection options
  where
    getResourceOptions :: Options MaterialType
    getResourceOptions = [("Wood", Wood), ("Clay", Clay), ("Stone", Stone), ("Reed", Reed)]

addBuildingMaterialToSupply :: MaterialType -> PersonalSupply -> PersonalSupply
addBuildingMaterialToSupply mt supply
  | mt == Wood  = supply { _wood = 1 + _wood supply}
  | mt == Clay  = supply { _clay = 1 + _clay supply}
  | mt == Reed  = supply { _reed = 1 + _reed supply}
  | mt == Stone = supply { _stone = 1 + _stone supply}

---------------------------------
--- Take Reed, Stone and Food ---
---------------------------------

runTakeReedStoneAndFood :: GameStateT ActionPrimitives
runTakeReedStoneAndFood = do
  gs <- get
  let ps = _players gs
      p = head ps
      supply = _personalSupply p
      supply' = supply { _reed = 1 + _reed supply }  { _stone = 1 + _stone supply }  { _food = 1 + _food supply }
      p' = p { _personalSupply = supply' }
      ps' = p' : tail ps
  put (gs { _players = ps' })
  return [TakeResources [(Material Reed, 1), (Material Stone, 1), (Food, 1)]]

----------------------------------
-- Take Sheep or Board or Cattle -
----------------------------------

runTakeSheepBoarOrCattle :: GameStateT ActionPrimitives
runTakeSheepBoarOrCattle = do
  gs <- get
  lift $ putStrLn "What would you like to do?"
  choice <- lift $ getNextSelection ([("Take 1 Sheep + 1 Food", 1), ("Take 1 Boar", 2)] ++ 
              if currentPlayer gs ^. personalSupply . food > 0 then [("Pay 1 Food for 1 Cattle", 3)] else [])
  let (at, foodOp) =
        case choice of
          1 -> (Sheep, (+1))
          2 -> (Boar,  (+0))
          3 -> (Cattle, (`subtract` 1))
  b' <- lift $ placeNewAnimals (at, 1) $ currentPlayer gs ^. board
  modify (\s -> s & players . ix 0 . personalSupply . food %~ foodOp
                  & players . ix 0 . board .~ b')
  return [TakeResources [(Animal at, 1)]]

----------------------------------
-- Take 2 Building Resources or FamilyGrowth -
----------------------------------

runTake2BuildingResourcesOrFamilyGrowth :: GameStateT ActionPrimitives
runTake2BuildingResourcesOrFamilyGrowth = return []

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
runTakeReedand1Stoneand1Wood = return []

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

takeResourcesAction :: ActionSpaceId -> GameStateT ActionPrimitives
takeResourcesAction id = do
  gs <- get
  let (gs', rs) = giveResourcesToCurrentPlayer id gs
  put gs'
  return [TakeResources rs | not (null rs)]
  where
    -- Take all resources from the action space specified by the action type and give them to the current player
    giveResourcesToCurrentPlayer :: ActionSpaceId -> GameState -> (GameState, Resources)
    giveResourcesToCurrentPlayer id gs =
      let asm = _actionSpaceMap gs in
      case M.lookup id asm of
        Nothing -> error "Unable to find action space"
        Just as ->
          let resources = _resources as
              cp' = giveResourcesToPlayer resources $ currentPlayer gs
              as' = as { _resources = [] } in
          (gs { _players = cp':tail (_players gs) } { _actionSpaceMap = M.insert id as' asm }, resources)

giveResourcesToPlayer :: Resources -> Player -> Player
giveResourcesToPlayer rs p =
  let ps = _personalSupply p
      ps' = addResources rs ps in
  p { _personalSupply = ps' }

putCurrentPlayerWorkerOnActionSpace :: ActionSpaceId -> GameState -> GameState
putCurrentPlayerWorkerOnActionSpace actionId gs =
  let p = currentPlayer gs
      n = _workers p in
  if n > 0
  then
    let p' = p { _workers = n - 1 }
        ps' = p':tail (_players gs)
        asm' = addWorker actionId (_playerId p) $ _actionSpaceMap gs in
    gs { _players = ps' } { _actionSpaceMap = asm' }
  else error "No workers left to put on action space"

addWorker :: ActionSpaceId -> PlayerId -> ActionSpaceMap -> ActionSpaceMap
addWorker actionId pid asm =
  case M.lookup actionId asm of
    Nothing -> error ("Unable to find action space for action id " ++ show actionId)
    Just as ->
      case M.lookup pid (_workersMap as) of
        Nothing -> M.insert actionId (as { _workersMap = M.insert pid 1 (_workersMap as) }) asm
        Just n  -> M.insert actionId (as { _workersMap = M.insert pid (n + 1) (_workersMap as) }) asm

addResources :: Resources -> PersonalSupply -> PersonalSupply
addResources rs ps = foldl addResource ps rs
  where
  addResource :: PersonalSupply -> Resource -> PersonalSupply
  addResource ps' (rt, amount)
    | rt == Food  = ps' { _food = _food ps' + amount}
    | rt == Crop Grain = ps' { _grain = _grain ps' + amount}
    | rt == Crop Veges = ps' { _veges = _veges ps' + amount}
    | rt == Material Wood  = ps' { _wood = _wood ps' + amount}
    | rt == Material Clay  = ps' { _clay = _clay ps' + amount}
    | rt == Material Reed  = ps' { _reed = _reed ps' + amount}
    | rt == Material Stone = ps' { _stone = _stone ps' + amount}

removeResourceType :: ResourceType -> Resources -> Resources
removeResourceType rt = filter (\(rt', _) -> rt /= rt')


--------------------------------
--------- Animal Storage -------
--------------------------------

placeNewAnimals :: Animal -> Board -> IO Board
placeNewAnimals (at, n) b = do
  putStrLn "Select where to put the new animals:"
  let options = getAnimalStoreOptions b
  getNextSelection options
  return b
  where
    getAnimalStoreOptions :: Board -> Options Int
    getAnimalStoreOptions b =
      let houseOption = ("In your house", 0)
          pastureOptions = zipWith (\(ss, mAnimal) n -> ("In pasture: " ++ show ss ++ ", containing: " ++ show mAnimal, n)) (_pastures b) [1 ..]
          ps = allPastureSpaces b
          unfencedStables = getUnfencedStables (_stables b) ps
          stableOptions = zipWith (\(s, mAnimal) m -> ("In stable: " ++ show s ++ ", containing: " ++ show mAnimal, 10 + m)) unfencedStables [10 .. ] in
      houseOption : pastureOptions ++ stableOptions

    getUnfencedStables :: [(Space, Maybe Animal)] -> Spaces -> [(Space, Maybe Animal)]
    getUnfencedStables allStables ss =
      filter (\(s, _) -> s `notElem` ss) allStables
