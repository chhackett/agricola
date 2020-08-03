{-# LANGUAGE TupleSections #-}

module Actions.BoardActions where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Data.List.Lens
import qualified Data.Set as Set
import Data.List

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
  where
    ifHaveEmptySpace :: ActionAllowedFunc
    ifHaveEmptySpace gs =
      let b =  _board $ currentPlayer gs
          hs = _houses b in
      not . null $ getAdjacentEmptySpaces b (fst hs)

runBuildRoomAndOrBuildStables :: GameStateT ActionPrimitives
runBuildRoomAndOrBuildStables = do
  results <- runBuildRoom
  results' <- runBuildStables
  return (results ++ results')

runBuildRoom :: GameStateT ActionPrimitives
runBuildRoom = do
  gs <- get
  let b = currentPlayer gs ^. board
  b' <- lift $ getBuildRoomChoices b
  put (gs & players . ix 0 . board .~ b')
  return [ExtendHouse]

getBuildRoomChoices :: Board -> IO Board
getBuildRoomChoices b = do
  let options = getBuildRoomOptions b
  putStrLn "Select a location to build a room"
  space <- getNextSelection options
  let b' = addRoom b space
  putStrLn "Do you want to build another room?"
  yes <- getNextSelection yesNoOptions
  if yes then getBuildRoomChoices b'
         else return b'
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
  let ps = _players gs
      p = head ps
      b = _board p
  b' <- lift $ getBuildStablesChoices b
  let p' = p { _board = b' }
      ps' = p' : tail ps
  put (gs { _players = ps' })
  return [BuildStables]

getBuildStablesChoices :: Board -> IO Board
getBuildStablesChoices b = do
  let options = getBuildStablesOptions b
  putStrLn "Select a location to build a stable"
  space <- getNextSelection options
  let b' = addStable b space
  putStrLn "Do you want to build another stable?"
  yes <- getNextSelection yesNoOptions
  if yes then getBuildStablesChoices b'
         else return b'
  where
    -- User can pick from any empty space adjacent to existing rooms
    getBuildStablesOptions :: Board -> Options Space
    getBuildStablesOptions b = buildSpaceOptions $ notHouseFieldOrStable b

addStable :: Board -> Space -> Board
addStable b s = b & stables %~ ((s, Nothing) :)
    
-----------------------------
------- RenovateHouse -------
-----------------------------

renovateHouse :: GameStateT ()
renovateHouse = return ()

-----------------------------
--------- Plow1Field --------
-----------------------------

runPlowFieldAction :: GameStateT ()
runPlowFieldAction = do
  gs <- get
  let ps = _players gs
      p = head ps
      b = _board p
  s <- lift $ getPlowFieldChoice b
  let b' = addField b s
      p' = p { _board = b' }
      ps' = p' : tail ps
  put (gs { _players = ps' })

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
      let fs = _fields $ _board $ currentPlayer gs in
      not (null fs) && any (\f -> case snd f of
                                    Nothing -> True
                                    Just n  -> False) fs

runSowAndOrBakeBreadAction :: GameStateT ActionPrimitives
runSowAndOrBakeBreadAction = do
  results <- runSowAction
  lift $ putStrLn "Would you like to bake bread too?"
  yes <- lift $ getNextSelection yesNoOptions
  if yes
    then (do results' <- runBakeBreadAction
             return (results ++ results'))
    else return results

runSowAction :: GameStateT ActionPrimitives
runSowAction = do
  gs <- get
  let ps = _players gs
      p = head ps
      b = _board p
  (s, ct) <- lift $ getSowFieldInput b
  let b' = sowField b s ct
      p' = p { _board = b' }
      ps' = p' : tail ps
  put (gs { _players = ps' })
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
  allConditions [ifNoWorkers asId]

-- fence action rules:
-- cannot place a fence where you already placed a fence
-- fences must entirely enclose a region or regions
-- all fences must be part of the boundary of enclosed regions (no extra fences that do not enclose a region)
-- enclosed regions may not contain fields or houses (but may contain stables)

runFencesAction :: GameStateT ActionPrimitives
runFencesAction = do
  gs <- get
  let ps = _players gs
      p = head ps
      b = _board p
      supply = _personalSupply p
      n = _wood supply
  es <- lift $ getFenceChoices (b, n, [])
  let es' = concatMap calculateBoundaryEdges (toListOf (pastures . traverse . _1) b)
      pastures' = map (\ss -> (ss, Nothing)) $ calculatePastures b (es ++ es') -- use all edges on the board
      b' = b { _pastures = pastures' }
      supply' = supply { _wood = n - length es}
      p' = p { _board = b' } { _personalSupply = supply'}
      ps' = p' : tail ps
  put (gs { _players = ps' })
  return []

-- Given the board compute where the user selects to put the next fence piece
getFenceChoices :: (Board, Int, Edges) -> IO Edges
getFenceChoices (b, n, es) = do
    putStrLn ("You can place [" ++ show n ++ "] more fences. Select where to place your next fence")
    e <- getNextSelection $ options $ filter (`notElem` es) $ availableFenceLocations b
    if n > 0
      then do
        putStrLn "Do you want to place another fence?"
        yes <- getNextSelection yesNoOptions
        if yes then getFenceChoices (b, n - 1, e : es)
               else return (e : es)
      else return (e : es)
    where
      options = map (\e' -> ("Location: " ++ show e', e'))

availableFenceLocations :: Board -> Edges
availableFenceLocations b = [ e | e <- allEdges, isFenceAllowed e b]
  where
    isFenceAllowed :: Edge -> Board -> Bool
    isFenceAllowed e b =
      let spaces = getAdjacentSpaces e
          validSpaces = filter isValidSpace spaces in
      not $ null $ intersect (allEmptySpaces b) validSpaces

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