{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Actions.BoardActions where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Data.List.Lens
import qualified Data.Set as Set
import Data.List

import ActionFunctions
import AnimalFunctions
import Actions.CardActions
import Types.CardNames
import Types.ResourceTypes
import Types.BasicGameTypes
import Types.CardDeclarations
import Utils.Selection
import Utils.ListUtils

class ResourceAction a where
  getCosts :: a -> [Resources] -> [Resources]
  run :: a -> GameStateT EventTypes

-----------------------------
--------- BuildRoom ---------
-----------------------------

buildRoomConditions :: ActionSpaceId -> ActionAllowedFunc
buildRoomConditions asId =
  allConditions [anyConditions [ifNoWorkers asId, ifHasCardName HeadOfTheFamily], ifHaveAdjacentEmptySpace, ifHaveHouseResources]

ifHaveAdjacentEmptySpace :: ActionAllowedFunc
ifHaveAdjacentEmptySpace gs =
  let b  = _board $ currentPlayer gs
      hs = _houses b in
  not . null $ getAdjacentEmptySpaces b (fst hs)

ifHaveHouseResources :: ActionAllowedFunc
ifHaveHouseResources gs =
  let p = currentPlayer gs
      houseType = snd $ p ^. board . houses in
  case houseType of
    WoodHouse  -> True
    ClayHouse  -> hasClayHouseResources gs
    StoneHouse -> True
  where
    hasClayHouseResources :: ActionAllowedFunc
    hasClayHouseResources gs = undefined

-- replaceResourcesToExtendRoom :: [(CardName, [Resources] -> [Resources])]
-- replaceResourcesToExtendRoom =
--   [ (ClayRoof, clayRoof),
--     (BrushwoodRoof, brushwoodRoof),
--     (BrushwoodCollector, brushwoodCollector),
--     (FrameBuilder, frameBuilder),
--     (WoodCarver, woodCarver),
--     (Ladder, ladder),
--     (Bricklayer, bricklayer)
--   ]
--   where
--     clayRoof (r:rs) = undefined

replaceResource :: ResourceType -> Resource -> Resources -> Resources
replaceResource rtnew (rtold, nmax) =
  foldl go []
  where
    go :: Resources -> Resource -> Resources
    go rs' (rt', n') =
      if rtold == rt'
      then if nmax >= n'
           then (rtnew, n') : rs'
           else (rtnew, nmax) : (rt', n' - nmax) : rs'
      else (rt', n') : rs'

calcExtendHouseCosts :: GameState -> [Resources]
calcExtendHouseCosts gs =
  let p = currentPlayer gs
      houseType = snd $ p ^. board . houses
      cards = map _cardName $ p ^. activeCards
      baseCosts = case houseType of
        WoodHouse  -> [(Wood, 5), (Reed, 2)] : foldr goWood [] cards
        ClayHouse  -> [(Clay, 5), (Reed, 2)] : foldr goClay [] cards
        StoneHouse -> [[(Stone, 5), (Reed, 2)]]
      cost1 = removeReed cards baseCosts in
      --cost2 = in
  undefined
  where
    goWood _ _ = undefined
    goClay card costs =
      case card of
        ClaySupports  -> [(Clay, 2), (Wood, 1), (Reed, 1)] : costs
        ClayPlasterer -> [(Clay, 3), (Reed, 2)] : costs
        _   -> costs

-- calcResourceOptions :: Resources

--modify :: ([[(ResourceType, Int)]] -> [[(ResourceType, Int)]])
--maybeRemoveReed :: CardNames -> [[(ResourceType, Int)]] -> [[(ResourceType, Int)]]
removeReed cards costs =
  if StrawthatchedRoof `elem` cards
    then map (filter (\(rt, n) -> rt /= Reed)) costs
    else costs

reduceCosts :: Eq a => (a, Int) -> [[(a, Int)]] -> [[(a, Int)]]
reduceCosts (rt, n) = map $ map reduceCost
  where
    -- reduceCost :: (b, Int) -> (b, Int)
    reduceCost (rt', n') =
      if rt == rt' then (rt, max 0 (n' - n))
                   else (rt', n')

-------------------------------
-- Build Room And/Or Stables --
-------------------------------

runBuildRoomAndOrStables :: Resources -> GameStateT EventTypes
runBuildRoomAndOrStables rs = do
  gs <- get
  let houseType = currentPlayer gs ^. board . houses . _2
  results <-
    if ifHaveResources rs gs
      then optionalDoGS "Do you want to build a room?" [] runBuildRoom
      else return []
  results' <- 
    if ifHaveResources [(Material Wood, 2)] gs
      then optionalDoGS "Do you want to build a stables?" [] runBuildStables
      else return []
  return (results ++ results')

runBuildRoom :: GameStateT EventTypes
runBuildRoom = do
  gs <- get
  let b = currentPlayer gs ^. board
  b' <- lift $ getBuildRoomChoices b
  put $ gs & players . ix 0 . board .~ b'
           & players . ix 0 . personalSupply . wood -~ 5
  if ifHaveResources [(Material Wood, 5), (Material Reed, 2)] gs
    then optionalDoGS "Do you want to build another room?" [ExtendHouse 1] runBuildRoom
    else return [ExtendHouse 1]

getBuildRoomChoices :: Board -> IO Board
getBuildRoomChoices b = do
  let options = getBuildRoomOptions b
  putStrLn "Select a location to build a room"
  space <- getNextSelection options
  return $ addRoom b space
  where
    -- User can pick from any empty space adjacent to existing rooms
    getBuildRoomOptions :: Board -> Options Space
    getBuildRoomOptions b = buildSpaceOptions $ getAdjacentEmptySpaces b $ fst $ _houses b

addRoom :: Board -> Space -> Board
addRoom b s = b & houses . _1 %~ (s:)

-----------------------------
------- BuildStables --------
-----------------------------

buildStablesConditions :: ActionSpaceId -> ActionAllowedFunc
buildStablesConditions asId =
  allConditions [ifNoWorkers asId, ifHaveResources [(Material Wood, 2)]]

runBuildStables :: GameStateT EventTypes
runBuildStables = do
  gs <- get
  let b = currentPlayer gs ^. board
  b' <- lift $ getBuildStablesChoices b
  put $ gs & players . ix 0 . board .~ b'
           & players . ix 0 . personalSupply . wood -~ 2
  if ifHaveResources [(Material Wood, 2)] gs
    then optionalDoGS "Do you want to build another stable?" [BuildStables 1] runBuildStables
    else return [BuildStables 1]

getBuildStablesChoices :: Board -> IO Board
getBuildStablesChoices b = do
  let options = getBuildStablesOptions b
  putStrLn "Select a location to build a stable"
  space <- getNextSelection options
  return $ addStable b space
  where
    -- User can pick from any empty space adjacent to existing rooms
    getBuildStablesOptions :: Board -> Options Space
    getBuildStablesOptions b = buildSpaceOptions $ notHouseFieldOrStable b

addStable :: Board -> Space -> Board
addStable b s = b & unfencedStables %~ ((s, Nothing) :)

-------------------------------
----- Renovation actions ------
-------------------------------

renovateConditions :: ActionSpaceId -> ActionAllowedFunc
renovateConditions asId =
  allConditions [ifNoWorkers asId, haveResourcesForRenovation]
  where
    haveResourcesForRenovation :: GameState -> Bool
    haveResourcesForRenovation gs =
      let n = length $ currentPlayer gs ^. board . houses . _1
          material = currentPlayer gs ^. board . houses . _2 in
      currentPlayer gs ^. personalSupply . reed >= 1 &&
      currentPlayer gs ^. personalSupply .
        (if material == WoodHouse then clay else stone) >= n

renovationAndMajorOrMinor :: GameStateT EventTypes
renovationAndMajorOrMinor = do
  result <- renovateHouse
  result' <- playMajorOrMinorImprovement
  return (result ++ result')

renovateAndFences :: GameStateT EventTypes
renovateAndFences = do
  result <- renovateHouse
  result' <- runFencesAction
  return (result ++ result')

renovateHouse :: GameStateT EventTypes
renovateHouse = do
  gs <- get
  let n = length $ currentPlayer gs ^. board . houses . _1
      m = currentPlayer gs ^. board . houses . _2
  put $ gs & players . ix 0 . board . houses . _2 .~ (if m == WoodHouse then ClayHouse else StoneHouse)
           & players . ix 0 . personalSupply . (if m == WoodHouse then clay else stone) -~ n
           & players . ix 0 . personalSupply . reed -~ 1
  return [RenovateHouse]

-----------------------------
--------- Plow1Field --------
-----------------------------

plowFieldConditions :: ActionSpaceId -> ActionAllowedFunc
plowFieldConditions id =
  allConditions [ifNoWorkers id, haveEmptySpace]

haveEmptySpace :: ActionAllowedFunc
haveEmptySpace gs =
  let b = currentPlayer gs ^. board in
  not . null $ allEmptySpaces b

runPlowAndOrSow :: GameStateT EventTypes
runPlowAndOrSow = do
  lift $ putStrLn "Would you like to plow, sow, or both?"
  choice <- lift $ getNextSelection $ oneOrBothOptions "Plow" "Sow"
  case choice of
    First  -> runPlowFieldAction
    Second -> runSowAction
    Both   ->
      do result <- runPlowFieldAction
         result' <- runSowAction
         return (result ++ result')

runPlowFieldAction :: GameStateT EventTypes
runPlowFieldAction = do
  gs <- get
  let b = currentPlayer gs ^. board
  s <- lift $ getPlowFieldChoice b
  put $ gs & players . ix 0 . board .~ addField b s
  return [PlowField 1]

getPlowFieldChoice :: Board -> IO Space
getPlowFieldChoice b = do
  let options = getPlowFieldOptions b
  putStrLn "Select a field to plow"
  getNextSelection options

-- User can pick from any empty space in his board except ones that are surrounded by fences
getPlowFieldOptions :: Board -> Options Space
getPlowFieldOptions b = buildSpaceOptions $ allEmptySpaces b

addField :: Board -> Space -> Board
addField b s = b & fields %~ ((s, Nothing):)

-----------------------------
------ SowAndOrBakeBread ----
-----------------------------

sowAndOrBakeBreadConditions :: ActionSpaceId -> ActionAllowedFunc
sowAndOrBakeBreadConditions asId =
  allConditions [ifNoWorkers asId, anyConditions [sowCondition, bakeBreadCondition]]

sowCondition :: ActionAllowedFunc
sowCondition gs =
  let g = currentPlayer gs ^. personalSupply . grain
      v = currentPlayer gs ^. personalSupply . veges in
  ifHaveEmptyField gs && (g > 0 || v > 0)
  where
    ifHaveEmptyField :: ActionAllowedFunc
    ifHaveEmptyField gs = 
      let fs = currentPlayer gs ^. board . fields in
      not (null fs) && any (\f -> case snd f of
                                    Nothing -> True
                                    Just n  -> False) fs

bakeBreadCondition :: ActionAllowedFunc
bakeBreadCondition gs =
  let g = currentPlayer gs ^. personalSupply . grain
      cards = currentPlayer gs ^. activeCards in
  g > 0 && not (null $ getBakingBreadCards cards)

runSowAndOrBakeBreadAction :: GameStateT EventTypes
runSowAndOrBakeBreadAction = do
  gs <- get
  results <- runSowAction
  if bakeBreadCondition gs
  then
    optionalDoGS "Would you like to bake bread too?" results $ do
      results' <- runBakeBreadAction
      return (results ++ results')
  else return results

runSowAction :: GameStateT EventTypes
runSowAction = do
  gs <- get
  let b = currentPlayer gs ^. board
      supply = currentPlayer gs ^. personalSupply
  input <- lift $ getSowFieldInput b supply
  case input of
    Nothing -> return []
    Just (s, ct) -> do
      let supply' = supply & (if ct == Grain then grain else veges) %~ (`subtract` 1)
      put $ gs & players . ix 0 . board .~ sowField b s ct
               & players . ix 0 . personalSupply .~ supply'
      return [SowField ct]

-- User needs to pick which field to sow.
getSowFieldInput :: Board -> PersonalSupply -> IO (Maybe (Space, CropType))
getSowFieldInput b s = do
  let fs = _fields b
      emptySpaces = map fst (filter (isNothing . snd) fs)
      options = buildSpaceOptions emptySpaces
      cropChoices = filter (\(_, ct) -> (s ^. if ct == Grain then grain else veges) > 0) [("Grain", Grain), ("Veges", Veges)]
  if null options || null cropChoices
  then return Nothing
  else do
    putStrLn "Select a field to sow"
    space <- getNextSelection options
    putStrLn "Select crop type (Grain or Veges)"
    crop <- getNextSelection cropChoices
    return $ Just (space, crop)

sowField :: Board -> Space -> CropType -> Board
sowField b s ct = b & fields %~ sow
  where sow = map (\(s', r') -> if s == s'
                                 then (s', addCrop)
                                 else (s', r'))
        addCrop = if ct == Veges then Just (Veges, 2) else Just (Grain, 3)

-----------------------------
----------- Fences ----------
-----------------------------

fencesConditions :: ActionSpaceId -> ActionAllowedFunc
fencesConditions asId =
  allConditions [ifNoWorkers asId, ifHaveResources [(Material Wood, 1)] ]

-- fence action rules:
-- cannot place a fence where you already placed a fence
-- fences must entirely enclose a region or regions
-- all fences must be part of the boundary of enclosed regions (no extra fences that do not enclose a region)
-- enclosed regions may not contain fields or houses (but may contain stables)

runFencesAction :: GameStateT EventTypes
runFencesAction = do
  gs <- get
  let b = currentPlayer gs ^. board
      ps = b ^. pastures
      oldPastureEdges = concatMap calculateBoundaryEdges (toListOf (pastures . traverse . _1) b)
      n = currentPlayer gs ^. personalSupply . wood
  es <- lift $ getFenceChoices (b, n, [])
  let ps' = map (\ss -> (ss, Nothing, computeStablesInPasture b ss)) $ calculatePastureSpaces b (es ++ oldPastureEdges) -- use all edges on the board
  if (length es + Set.size (calculateAllPastureEdges ps)) /= Set.size (calculateAllPastureEdges ps')
  then do
    lift $ putStrLn "At least one fence is not part of the boundary of a valid pasture. Would you like to try again?"
    yes <- lift $ getNextSelection yesNoOptions
    if yes then runFencesAction
           else return []
  else do
    let animals = getAllAnimals b
    b' <- lift $ placeNewAnimals animals $ b & pastures .~ ps'
    put $ gs & players . ix 0 . board .~ b'
             & players . ix 0 . personalSupply . wood .~ n - length es
    return [BuildFences $ length es]

-- Given the board compute where the user selects to put the next fence piece
getFenceChoices :: (Board, Int, Edges) -> IO Edges
getFenceChoices (b, n, es) = do
    putStrLn ("You can place [" ++ show n ++ "] more fences. Select where to place your next fence")
    e <- getNextSelection $ options $ filter (`notElem` es) $ availableFenceLocations b
    if n > 0
      then optionalDoIO "Do you want to place another fence?" (e : es) $ getFenceChoices (b, n - 1, e : es)
      else return (e : es)
    where
      options = map (\e' -> ("Location: " ++ show e', e'))

availableFenceLocations :: Board -> Edges
availableFenceLocations b = [ e | e <- allEdges, isFenceAllowed e b]

-- A fence is allowed at a specified edge location if there is not already a fence there,
-- and if the edge is adjacent to a space that does not have a field or house on it
isFenceAllowed :: Edge -> Board -> Bool
isFenceAllowed e b =
  let validSpaces = filter isValidSpace (getAdjacentSpaces e)
      fences = concatMap (\(ss,_,_) -> calculateBoundaryEdges ss) (_pastures b)
      hf = allHousesAndFields b in
  any (`notElem` hf) validSpaces && (e `notElem` fences)

getAdjacentSpaces :: Edge -> Spaces
getAdjacentSpaces ((x1, y1), (x2, y2)) =
  if isVerticalEdge ((x1, y1), (x2, y2))
    then [(x1 - 1, y1), (x1, y1)]
    else [(x1, y1 - 1), (x1, y1)]

isVerticalEdge :: Edge -> Bool
isVerticalEdge ((x1, _), (x2, _)) = x1 == x2

-- Given list of spaces, compute bordering edges, removing edges that are shared by orthogonally adjacent spaces
calculateBoundaryEdges :: Spaces -> Edges
calculateBoundaryEdges ss =
  let allEdges = concatMap calcEdges ss in
  concat $ filter (\g -> length g == 1) $ group $ sort allEdges
  where
    calcEdges :: Space -> Edges
    calcEdges (x, y) =
      [ ((x, y), (x + 1, y))
      , ((x, y + 1), (x + 1, y + 1))
      , ((x, y), (x, y + 1))
      , ((x + 1, y), (x + 1, y + 1)) ]

-- Calculate connected regions: For each orthogonally adjacent pair of spaces on the board, if they are not separated by
-- a fence, then they are part of the same region.
-- Add all 'connected' spaces to the region. Visit each connected space and repeat. If a space is on the edge of the board 
-- and the space is not separated from the edge by a fence, then the region is not fenced in.
calculatePastureSpaces :: Board -> Edges -> [Spaces]
calculatePastureSpaces b es =
  let houseAndFieldSpaces = allHousesAndFields b
      validSpaces = difference houseAndFieldSpaces allSpaces
      allEdges = calculateBoundaryEdges (allPastureSpaces b) ++ es
      regions = calculateDisconnectedRegions allEdges in
  filter (isRegionFencedIn allEdges) regions

calculateAllPastureEdges :: Pastures -> Set.Set Edge
calculateAllPastureEdges ps =
  Set.unions $ map (Set.fromList . (\(ss, _, _) -> calculateBoundaryEdges ss)) ps

-- Given the set of fences, calculate regions that are completely separated by fences
calculateDisconnectedRegions :: Edges -> [Spaces]
calculateDisconnectedRegions edges =
  let regions = foldl (buildRegions edges) [] allSpaces
      mergedRegions = mergeRegions regions in
  map Set.elems mergedRegions

-- For each space, if the current space is not part of a region, make a new region.
-- If current space is NOT on right edge of the board, check space to right.
--    If spaces are connected, add the space to right to current region. Else, do nothing.
-- Repeat for space above current space.
buildRegions :: Edges -> [Set.Set Space] -> Space -> [Set.Set Space]
buildRegions es regions s =
  let mIdx = findIndex (s `Set.member`) regions in
  case mIdx of
    Nothing -> addConnectedSpaces es (Set.singleton s) s : regions
    Just i  -> take i regions ++ addConnectedSpaces es (regions !! i) s : drop (i + 1) regions

addConnectedSpaces :: Edges -> Set.Set Space -> Space -> Set.Set Space
addConnectedSpaces es region s = Set.union region $ getConnectedRightAndTopSpaces es s

getConnectedRightAndTopSpaces :: Edges -> Space -> Set.Set Space
getConnectedRightAndTopSpaces edges s =
  let connectedSpaces = map snd (filter (\t -> fst t `notElem` edges) [(getRightEdge s, getRightHandSpace s), (getTopEdge s, getTopSpace s)]) in
  Set.fromList $ filter isValidSpace connectedSpaces

-- For each region, if it has any element in common with another region, combine the two regions. Continue checking each region with all other regions.
mergeRegions :: Ord a => [Set.Set a] -> [Set.Set a]
mergeRegions [] = []
mergeRegions [r] = [r]
mergeRegions (r:rs) = let (r', rs') = mergeRegion r rs in r' : mergeRegions rs'

mergeRegion :: Ord a => Set.Set a -> [Set.Set a] -> (Set.Set a, [Set.Set a])
mergeRegion r = foldl innerMerge (r,[])
  where
  innerMerge (r', rs') r'' =
    if Set.disjoint r' r'' then (r', r'' : rs')
                           else (Set.union r' r'', rs')

isRegionFencedIn :: Edges -> Spaces -> Bool
isRegionFencedIn edges region =
  let regionBoundary = calculateBoundaryEdges region in
  Set.isSubsetOf (Set.fromList regionBoundary) (Set.fromList edges)

getRightEdge :: Space -> Edge
getRightEdge (x, y) = ((x + 1, y), (x + 1, y + 1))

getTopEdge :: Space -> Edge
getTopEdge (x, y) = ((x, y + 1), (x + 1, y + 1))

getRightHandSpace :: Space -> Space
getRightHandSpace (x, y) = (x + 1, y)

getTopSpace :: Space -> Space
getTopSpace (x, y) = (x, y + 1)

getAdjacentEmptySpaces :: Board -> Spaces -> Spaces
getAdjacentEmptySpaces b ss =
  let ss' = concatMap allAdjacentSpaces ss in
  filter (isSpaceEmpty b) ss'

allAdjacentSpaces :: Space -> Spaces
allAdjacentSpaces (x, y) = 
  let spaces = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] in
  filter isValidSpace spaces

allHousesAndFields :: Board -> Spaces
allHousesAndFields b =
  let ss1 = fst $ _houses b
      ss2 = toListOf (fields . traverse . _1) b in
  ss1 ++ ss2

allStables :: Board -> Spaces
allStables b = toListOf (unfencedStables . traverse . _1) b ++ concatMap (\(_, _, ss) -> ss) (b ^. pastures)

-- Calculate the subset of spaces of the ones provided which contain a stables
computeStablesInPasture :: Board -> Spaces -> Spaces
computeStablesInPasture b ss = ss `intersect` allStables b

allPastureSpaces :: Board -> Spaces
allPastureSpaces b = concatMap (\(ss, _, _) -> ss) (_pastures b)

-- Include houses, fields, pastures and stables
allUsedSpaces :: Board -> Spaces
allUsedSpaces b = concat [allHousesAndFields b, allStables b, allPastureSpaces b]

notHouseFieldOrStable :: Board -> Spaces
notHouseFieldOrStable b = difference (allHousesAndFields b ++ allStables b) allSpaces

allEmptySpaces :: Board -> Spaces
allEmptySpaces b = difference (allUsedSpaces b) allSpaces

isSpaceUsed :: Board -> Space -> Bool
isSpaceUsed b s = s `elem` allUsedSpaces b

isSpaceEmpty :: Board -> Space -> Bool
isSpaceEmpty b s = not (isSpaceUsed b s)

isValidSpace :: Space -> Bool
isValidSpace s = s `elem` allSpaces

buildSpaceOptions :: Spaces -> Options Space
buildSpaceOptions = map (\s -> ("Location: " ++ show s, s))

allSpaces :: Spaces
allSpaces = [(x, y) | y <- [0 .. 2], x <- [0 .. 4]]

allEdges :: Edges
allEdges = allHorizontalEdges ++ allVerticalEdges

allHorizontalEdges :: Edges
allHorizontalEdges = [((x, y), (x + 1, y)) | x <- [0 .. 4], y <- [0 .. 3]]

allVerticalEdges :: Edges
allVerticalEdges = [((x, y), (x, y + 1)) | x <- [0 .. 5], y <- [0 .. 2]]

--------------------------------
--------- Animal Storage -------
--------------------------------

-- This action allows user to take list of animal types, assign them to pastures, unfenced stables, or the house.
-- Any extra animals that are not placed 'run away' when this action completes. Perhaps in the future we also need
-- to allow for animals to be converted to food while this action proceeds. But for now, that will be handled as
-- a separate action.
placeNewAnimals :: Animals -> Board -> IO Board
placeNewAnimals [] b = return b
placeNewAnimals as b = do
  putStrLn $ "Unassigned animals:\n\t" ++ concatMap show as
  putStrLn "You may store animals in the following locations:"
  putStrLn $ "Your house, currently holding [" ++ show (_houseAnimal b) ++ "]"
  mapM_ showPastures $ zip (_pastures b) [0 ..]
  mapM_ showUnfencedStables $ zip (_unfencedStables b) [0 ..]
  putStrLn "Select which animal type you would like to assign to a pasture, unfenced stable, or the house:"
  choice <- getNextSelection $ map (\(at, n) -> (show at, (at, n))) as
  (b', as') <- placeNewAnimal choice b
  if null as'
  then do
    putStrLn "There are no more unassigned animals"
    return b'
  else do
    putStrLn $ "These animals are now unassigned: " ++ show as'
    optionalDoIO "Do you want to assign the remaining animals?" b' (placeNewAnimals as' b')
  where
    showPastures ((ss, a, st), i) = print $ "Pasture [" ++ show i ++ "], currently holding [" ++ showAnimal a ++ "] can hold up to " ++ show (if null st then 2 * length ss else 4 * length ss) ++ " animals"
    showUnfencedStables ((_, a), i) = print $ "Unfenced stable [" ++ show i ++ "], currently holding [" ++ showAnimal a ++ "] can hold up to 2 animals"
    showAnimal a = case a of
      Nothing -> "Nothing"
      Just (at, i) -> show i ++ " " ++ show at

-- Place the requested animals in a valid location on the board. Other animals may have to be removed from that location. Those are returned in the result.
placeNewAnimal :: Animal -> Board -> IO (Board, Animals)
placeNewAnimal (at, n) b = do
  putStrLn "Select where to put the new animals (animals in that location will be replaced if they are of different type):"
  let options = getAnimalStoreOptions b
  choice <- getNextSelection options
  case choice of
    House -> do
      let ((at', n'), leftOvers) = storeAnimals 1 (at, n) $ (, 1) <$> _houseAnimal b
      return (b & houseAnimal ?~ at', leftOvers)
    Pasture i ->
      let (ss, a, st) = (b ^. pastures) !! i
          cap = getCapacity (ss, a, st)
          ((at', n'), leftOvers) = storeAnimals cap (at, n) a in
      return (b & pastures . ix i . _2 ?~ (at', n'), leftOvers)
    UnfencedStable i ->
      let a = snd $ (b ^. unfencedStables) !! i
          ((at', n'), leftOvers) = storeAnimals 2 (at, n) a in
      return (b & unfencedStables . ix i . _2 ?~ (at, n'), leftOvers)
    where
    -- n is capacity that can be stored in a location, (at, n') is unassigned animal we want to store in the location, (at', n'') is animals already in that location
    -- resulting tuple is (animal in the location, leftover animals that are unassigned)
    storeAnimals :: Int -> Animal -> Maybe Animal -> (Animal, Animals)
    storeAnimals cap (at, n') Nothing
      | n' > cap = ((at, cap), [(at, n' - cap)])
      | otherwise = ((at, n'), [])
    storeAnimals cap (at, n') (Just (at', n''))
      | at == at' = if n' + n'' > cap then ((at, cap), [(at, n' + n'' - cap)]) else ((at, n' + n''), [])
      | otherwise = if n' > cap
                    then ((at, cap), [(at, n' - cap), (at', n'')])
                    else ((at, n'), [(at', n'')])

    getCapacity :: Pasture -> Int
    getCapacity p = let (ss, _, st) = p in if null st then 2 * length ss else 4 * length ss
