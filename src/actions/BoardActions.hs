{-# LANGUAGE TupleSections #-}

module Actions.BoardActions where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Data.List.Lens
import qualified Data.Set as Set
import Data.List

import Actions.CardActions
import Types.BasicGameTypes
import Utils.Selection
import Utils.ListUtils
import ActionTypes

-----------------------------
--------- BuildRoom ---------
-----------------------------

buildRoomConditions :: ActionSpaceId -> ActionAllowedFunc
buildRoomConditions asId =
  allConditions [ifNoWorkers asId, ifHaveEmptySpace, ifHaveResources [(Material Wood, 5), (Material Clay, 2)]]

buildStablesConditions :: ActionSpaceId -> ActionAllowedFunc
buildStablesConditions asId =
  allConditions [ifNoWorkers asId, ifHaveEmptySpace, ifHaveResources [(Material Wood, 2)]]

runBuildRoomAndOrStables :: GameStateT ActionPrimitives
runBuildRoomAndOrStables = do
  gs <- get
  results <-
    if ifHaveResources [(Material Wood, 5), (Material Clay, 2)] gs
      then optionalDoGS "Do you want to build a room?" [] runBuildRoom
      else return []
  results' <- 
    if ifHaveResources [(Material Wood, 2)] gs
      then optionalDoGS "Do you want to build a stables?" [] runBuildStables
      else return []
  return (results ++ results')

runBuildRoom :: GameStateT ActionPrimitives
runBuildRoom = do
  gs <- get
  let b = currentPlayer gs ^. board
  b' <- lift $ getBuildRoomChoices b
  put (gs & players . ix 0 . board .~ b')
  if ifHaveResources [(Material Wood, 5), (Material Clay, 2)] gs
    then optionalDoGS "Do you want to build another room?" [ExtendHouse] runBuildRoom
    else return [ExtendHouse]

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

runBuildStables :: GameStateT ActionPrimitives
runBuildStables = do
  gs <- get
  let b = currentPlayer gs ^. board
  b' <- lift $ getBuildStablesChoices b
  put $ gs & players . ix 0 . board .~ b'
  if ifHaveResources [(Material Wood, 2)] gs
    then optionalDoGS "Do you want to build another stable?" [BuildStables] runBuildStables
    else return [BuildStables]

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
addStable b s = b & stables %~ ((s, Nothing) :)

-------------------------------
-- Renovate + Major or Minor --
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

afterRenovationAlsoMajorOrMinor :: GameStateT ActionPrimitives
afterRenovationAlsoMajorOrMinor = do
  result <- renovateHouse
  result' <- runPlayMajorOrMinorImprovement
  return (result ++ result')

renovateAndFences :: GameStateT ActionPrimitives
renovateAndFences = do
  result <- renovateHouse
  result' <- runFencesAction
  return (result ++ result')

-----------------------------
------- RenovateHouse -------
-----------------------------

renovateHouse :: GameStateT ActionPrimitives
renovateHouse = do
  gs <- get
  let n = length $ currentPlayer gs ^. board . houses . _1
      m = currentPlayer gs ^. board . houses . _2
  put $ gs & players . ix 0 . board . houses . _2 .~ (if m == WoodHouse then ClayHouse else StoneHouse)
           & players . ix 0 . personalSupply . (if m == WoodHouse then clay else stone) -~ n
           & players . ix 0 . personalSupply . reed -~ 1
  return [RenovateHouse]

runPlowAndOrSow :: GameStateT ActionPrimitives
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

-----------------------------
--------- Plow1Field --------
-----------------------------

runPlowFieldAction :: GameStateT ActionPrimitives
runPlowFieldAction = do
  gs <- get
  let b = currentPlayer gs ^. board
  s <- lift $ getPlowFieldChoice b
  put $ gs & players . ix 0 . board .~ addField b s
  return [PlowField]

getPlowFieldChoice :: Board -> IO Space
getPlowFieldChoice b = do
  let options = getPlowFieldOptions b
  putStrLn "Select a field to plow"
  getNextSelection options

-- User can pick from any empty space in his board except ones that are surrounded by fences
getPlowFieldOptions :: Board -> Options Space
getPlowFieldOptions b = buildSpaceOptions $ allEmptySpaces b

addField :: Board -> Space -> Board
addField b s = b & fields %~ ((s,Nothing):)

-----------------------------
------ SowAndOrBakeBread ----
-----------------------------

sowAndOrBakeBreadConditions :: ActionSpaceId -> ActionAllowedFunc
sowAndOrBakeBreadConditions asId =
  allConditions [ifNoWorkers asId, ifHaveEmptyField]
  where
    ifHaveEmptyField :: ActionAllowedFunc
    ifHaveEmptyField gs = 
      let fs = currentPlayer gs ^. board . fields in
      not (null fs) && any (\f -> case snd f of
                                    Nothing -> True
                                    Just n  -> False) fs

runSowAndOrBakeBreadAction :: GameStateT ActionPrimitives
runSowAndOrBakeBreadAction = do
  results <- runSowAction
  optionalDoGS "Would you like to bake bread too?" results $
    do results' <- runBakeBreadAction
       return (results ++ results')

runSowAction :: GameStateT ActionPrimitives
runSowAction = do
  gs <- get
  let b = currentPlayer gs ^. board
  (s, ct) <- lift $ getSowFieldInput b
  let supply = currentPlayer gs ^. personalSupply
      supply' = if ct == Grain
                then supply & grain %~ (`subtract` 1)
                else supply & veges %~ (`subtract` 1)
  put $ gs & players . ix 0 . board .~ sowField b s ct
           & players . ix 0 . personalSupply .~ supply'
  return []

-- User needs to pick which field to sow.
getSowFieldInput :: Board -> IO (Space, CropType)
getSowFieldInput b = do
  let fs = _fields b
      emptySpaces = map fst (filter (isNothing . snd) fs)
      options = buildSpaceOptions emptySpaces
  putStrLn "Select a field to sow"
  space <- getNextSelection options
  putStrLn "Select crop type (Grain or Veges)"
  crop <- getNextSelection [("Grain", Grain), ("Veges", Veges)]
  return (space, crop)

sowField :: Board -> Space -> CropType -> Board
sowField b s ct = b & fields %~ sow
  where sow = map (\(s', r') -> if s == s'
                                 then (s', addCrop)
                                 else (s', r'))
        addCrop = if ct == Veges then Just (Veges, 2) else Just (Grain, 3)

-----------------------------
--------- BakeBread ---------
-----------------------------

runBakeBreadAction :: GameStateT ActionPrimitives
runBakeBreadAction = return []

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

runFencesAction :: GameStateT ActionPrimitives
runFencesAction = do
  gs <- get
  let b = currentPlayer gs ^. board
      n = currentPlayer gs ^. personalSupply . wood
  es <- lift $ getFenceChoices (b, n, [])
  let es' = concatMap calculateBoundaryEdges (toListOf (pastures . traverse . _1) b)
      pastures' = map (\ss -> (ss, Nothing)) $ calculatePastures b (es ++ es') -- use all edges on the board
  put $ gs & players . ix 0 . board . pastures .~ pastures'
           & players . ix 0 . personalSupply . wood .~ n - length es
  return []

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
      fences = concatMap (\(ss,_) -> calculateBoundaryEdges ss) (_pastures b)
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
calculatePastures :: Board -> Edges -> [Spaces]
calculatePastures b es =
  let houseAndFieldSpaces = allHousesAndFields b
      validSpaces = difference houseAndFieldSpaces allSpaces
      allEdges = calculateBoundaryEdges (allPastureSpaces b) ++ es
      regions = calculateDisconnectedRegions allEdges in
  filter (isRegionFencedIn allEdges) regions

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
allStables = toListOf (stables . traverse . _1)

allPastureSpaces :: Board -> Spaces
allPastureSpaces b = concatMap fst (_pastures b)

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

ifHaveEmptySpace :: ActionAllowedFunc
ifHaveEmptySpace gs =
  let b  = _board $ currentPlayer gs
      hs = _houses b in
  not . null $ getAdjacentEmptySpaces b (fst hs)
