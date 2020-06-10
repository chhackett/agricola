{-# LANGUAGE TupleSections #-}

module Actions.BoardActions where

import qualified Data.Set as Set
import Data.List

import Types.BasicGameTypes
import Types.GameState
import Types.PlayerData as PD
import Utils.Selection
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.List.Lens
import Utils.ListUtils
import Actions.GameActionFuncs

-----------------------------
--- BuildRoomAndOrStables ---
-----------------------------

runBuildRoomAndOrStables :: GameStateT ()
runBuildRoomAndOrStables = return ()

addRoom :: Board -> Space -> Board
addRoom (Board (hcs,mt) fs ps ss) c = Board (c:hcs, mt) fs ps ss

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
getPlowFieldOptions b = map build $ allEmptySpaces b
  where
    build s = ("Location: " ++ show s, s)

addField :: Board -> Space -> Board
addField (Board hs fs ps ss) s = Board hs ((s, (None, 0)):fs) ps ss


-----------------------------
------ SowAndOrBakeBread ----
-----------------------------

sowAndOrBakeBreadConditions :: ActionAllowedFunc
sowAndOrBakeBreadConditions =
  allConditions [ifNoWorkers SowAndOrBakeBread, ifHaveEmptyField]
  where
    ifHaveEmptyField :: ActionAllowedFunc
    ifHaveEmptyField gs = 
      let fs = _fields $ _board $ currentPlayer gs in
      any (\f -> (snd . snd) f == 0) fs

runSowAndOrBakeBreadAction :: GameStateT ()
runSowAndOrBakeBreadAction = do
  runSowAction
  lift $ putStrLn "Would you like to bake bread too?"
  yes <- lift $ getNextSelection yesNoOptions
  when yes runBakeBreadAction
   --    else return ()

runSowAction :: GameStateT ()
runSowAction = do
  gs <- get
  let ps = _players gs
      p = head ps
      b = _board p
  (s, rt) <- lift $ getSowFieldInput b
  let b' = sowField b s rt
      p' = p { _board = b' }
      ps' = p' : tail ps
  put (gs { _players = ps' })

-- User needs to pick which field to sow.
getSowFieldInput :: Board -> IO (Space, ResourceType)
getSowFieldInput b = do
  let fs = _fields b
      spaces = map fst (filter (null . snd) fs)
      options = buildOptions spaces
  putStrLn "Select a field to sow"
  space <- getNextSelection options
  putStrLn "Select crop type (Grain or Veges)"
  crop <- getNextSelection [("Grain", Grain), ("Veges", Veges)]
  return (space, crop)
  where
    buildOptions = map (\s -> ("Location: " ++ show s, s))

sowField :: Board -> Space -> ResourceType -> Board
sowField (Board hs fs ps ss) s rt = Board hs (sow fs) ps ss
  where sow = map (\(s', r') -> if s == s'
                                 then (s', addCrop)
                                 else (s', r'))
        addCrop = if rt == Veges then (Veges, 2) else (Grain, 3)

-----------------------------
----------- Fences ----------
-----------------------------

-- fence action rules:
-- cannot place a fence where you already placed a fence
-- fences must entirely enclose a region or regions
-- all fences must be part of the boundary of enclosed regions (no extra fences that do not enclose a region)
-- enclosed regions may not contain fields or houses (but may contain stables)

runFencesAction :: GameStateT ()
runFencesAction = do
  gs <- get
  let ps = _players gs
      p = head ps
      b = _board p
      rs = PD._resources p
      n = getAmount Wood rs
  es <- lift $ getFenceChoices (b, n, [])
  let es' = concatMap calculateBoundaryEdges (toListOf (pastures . traverse . _1) b)
      newps = map (\ss -> (ss, [])) $ calculatePastures b (es ++ es') -- use all edges on the board
      b' = b { _pastures = newps }
      p' = p { _board = b' }
      ps' = p' : tail ps
  put (gs { _players = ps' })

-- Given the board compute where the user selects to put the next fence piece
getFenceChoices :: (Board, Int, Edges) -> IO Edges
getFenceChoices (b, n, es) = do
    putStrLn ("You can place [" ++ show n ++ "] more fences. Select where to place your next fence")
    e <- getNextSelection $ options $ availableFenceLocations b
    if n > 0
      then do
        putStrLn "Do you want to place another fence?"
        yes <- getNextSelection yesNoOptions
        if yes then getFenceChoices (b, n - 1, es)
               else return es
      else return es
    where
      options = map (\e' -> ("Location: " ++ show e', e'))

availableFenceLocations :: Board -> Edges
availableFenceLocations b = [ e | e <- allEdges, isFenceAllowed e b]

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
      validSpaces = difference houseAndFieldSpaces PD.allSpaces
      allEdges = calculateBoundaryEdges (allPastureSpaces b) ++ es
      regions = calculateDisconnectedRegions allEdges in
  filter (isRegionFencedIn allEdges) regions

-- Given the set of fences, calculate regions that are completely separated by fences
calculateDisconnectedRegions :: Edges -> [Spaces]
calculateDisconnectedRegions edges =
  let regions = foldl (buildRegions edges) [] PD.allSpaces
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
mergeRegions :: [Set.Set Space] -> [Set.Set Space]
mergeRegions [r] = [r]
mergeRegions (r:rs) = let (r', rs') = mergeRegion r rs in r' : mergeRegions rs'
mergeRegions [] = []

mergeRegion :: Set.Set Space -> [Set.Set Space] -> (Set.Set Space, [Set.Set Space])
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

-- getRightAndTopAdjacentSpaces :: Space :: Spaces
-- getRightAndOrTopAdjacentSpaces (x, y) =
--   let rightAndTopSpaces = [(x + 1, y), (x, y + 1)] in
--   filter isValidSpace rightAndTopSpaces

-- getOrthogonallyAdjacentSpaces :: Space :: Spaces
-- getOrthogonallyAdjacentSpaces (x, y) =
--   let allAdjacentSpaces = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] in
--   filter isValidSpace allAdjacentSpaces

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

allEmptySpaces :: Board -> Spaces
allEmptySpaces b = difference (allUsedSpaces b) PD.allSpaces

isSpaceUsed :: Space -> Board -> Bool
isSpaceUsed s b = s `elem` allUsedSpaces b

isSpaceEmpty :: Space -> Board -> Bool
isSpaceEmpty s b = not (isSpaceUsed s b)

isValidSpace :: Space -> Bool
isValidSpace s = s `elem` PD.allSpaces

-----------------------------
--------- BakeBread ---------
-----------------------------

runBakeBreadAction :: GameStateT ()
runBakeBreadAction = return ()