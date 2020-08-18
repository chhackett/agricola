module Actions.ResourceActions where

import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import Types.BasicGameTypes
import Actions.BoardActions
import Actions.AutomaticActions
import Utils.ListUtils
import Utils.Selection

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
    giveResourcesToPlayer ((rt, n) : rs) p = do
      p' <- case rt of
        Food           -> return $ giveResourceToPlayer (rt,n) p
        Crop Grain     -> return $ giveResourceToPlayer (rt,n) p
        Crop Veges     -> return $ giveResourceToPlayer (rt,n) p
        Material Wood  -> return $ giveResourceToPlayer (rt,n) p
        Material Clay  -> return $ giveResourceToPlayer (rt,n) p
        Material Reed  -> return $ giveResourceToPlayer (rt,n) p
        Material Stone -> return $ giveResourceToPlayer (rt,n) p
        Animal Sheep   -> giveAnimalToPlayer (Sheep, n) p
        Animal Boar    -> giveAnimalToPlayer (Boar, n) p
        Animal Cattle  -> giveAnimalToPlayer (Cattle, n) p
      giveResourcesToPlayer rs p'

giveResourceToPlayer :: Resource -> Player -> Player
giveResourceToPlayer (rt, n) p =
  let supply = putResourceInSupply (rt, n) (_personalSupply p) in
  p & personalSupply .~ supply

giveAnimalToPlayer :: Animal -> Player -> IO Player
giveAnimalToPlayer (at,n) p = do
  b <- placeNewAnimals (at, n) (_board p)
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

putResourceInSupply :: Resource -> PersonalSupply -> PersonalSupply
putResourceInSupply (rt, amount) ps
  | rt == Food           = ps & food +~ amount
  | rt == Crop Grain     = ps & grain +~ amount
  | rt == Crop Veges     = ps & veges +~ amount
  | rt == Material Wood  = ps & wood +~ amount
  | rt == Material Clay  = ps & clay +~ amount
  | rt == Material Reed  = ps & reed +~ amount
  | rt == Material Stone = ps & stone +~ amount
  | otherwise            = error "Can't use this function to store animals"

removeResourceType :: ResourceType -> Resources -> Resources
removeResourceType rt = filter (\(rt', _) -> rt /= rt')

--------------------------------
--------- Animal Storage -------
--------------------------------
houseOption = 0
pastureOption = 1
stablesOption = 10

placeNewAnimals :: Animal -> Board -> IO Board
placeNewAnimals (at, n) b = do
  putStrLn "Select where to put the new animals:"
  let options = getAnimalStoreOptions b
  location <- getNextSelection options
  case location of
    l | l == houseOption
              -> return (b & houseAnimal ?~ at)
      | l < stablesOption
              -> do let (ss, _) = (b ^. pastures) !! (l - 1)     -- spaces the chosen pasture
                        size = length ss
                        hasStable = null (ss `intersect` allStables b)
                        n' = min (if hasStable then 4*size else 2*size) n
                    return (b & pastures . ix (l - pastureOption) . _2 ?~ (at, n'))
      | l >= stablesOption
              -> do let unfencedStables = getUnfencedStables (_stables b) (allPastureSpaces b)
                        s = fst $ unfencedStables !! (l - stablesOption)
                        mi = findIndex (\(s',_) -> s' == s) (_stables b)
                    case mi of
                        Nothing -> error "Can't find the stable"
                        Just i  -> return (b & stables . ix i . _2 ?~ (at,1))
      | otherwise -> error "Invalid selection"
  where
    getAnimalStoreOptions :: Board -> Options Int
    getAnimalStoreOptions b =
      let houseOption = ("In your house, containing: " ++ show (_houseAnimal b), 0)
          pastureOptions = zipWith (\(ss, mAnimal) n -> ("In pasture: " ++ show ss ++ ", containing: " ++ show mAnimal, n)) (_pastures b) [pastureOption ..]
          unfencedStables = getUnfencedStables (_stables b) (allPastureSpaces b)
          stableOptions = zipWith (\(s, mAnimal) m -> ("In (unfenced) stable: " ++ show s ++ ", containing: " ++ show mAnimal, m)) unfencedStables [stablesOption .. ] in
      houseOption : pastureOptions ++ stableOptions

    getUnfencedStables :: [(Space, Maybe Animal)] -> Spaces -> [(Space, Maybe Animal)]
    getUnfencedStables allStables ss =
      filter (\(s, _) -> s `notElem` ss) allStables
