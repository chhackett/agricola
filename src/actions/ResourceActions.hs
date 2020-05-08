module Actions.ResourceActions where

import Types.GameState
import Types.ResourceTypes
import Types.PlayerData

addWorkerToPlayer :: Player -> Player
addWorkerToPlayer (Player id n b w ps hcs acs) =
  Player id n b (w + 1) ps hcs acs

giveResourcesOfTypeToCurrentPlayer :: ResourceType -> ActionType -> GameState -> GameState
giveResourcesOfTypeToCurrentPlayer rt at gs =
  let as = _currentActionSpaces gs
      maybeA = getActionSpaceFromType at as in
  case maybeA of
    Nothing -> gs
    Just actionSpace ->
      let cp = _currentPlayer gs
          rs = Types.GameState.resources actionSpace
          amount = getAmount rt rs in
          -- cprs = _resources cp in
      if amount > 0
      then
        let cp' = giveResourceToPlayer (rt, amount) cp
            rs' = removeResourceType rt rs
            actionSpace' = actionSpace { Types.GameState.resources = rs' }
            as' = replaceElem actionType actionSpace' as in
        gs { _currentPlayer = cp' } { _currentActionSpaces = as' }
      else gs

putCurrentPlayerWorkerOnActionSpace :: ActionSpace -> GameAction
putCurrentPlayerWorkerOnActionSpace a g =
  let p = _currentPlayer g
      n = _workers p in
  if n > 0
  then
    let p' = p { _workers = n - 1 }
        pids = Types.GameState.workers a
        a' = a { Types.GameState.workers = _playerId p : pids}
        cas = _currentActionSpaces g
        cas' = replaceElem actionType a' cas in
    g { _currentPlayer = p' } { _currentActionSpaces = cas' }
  else g

getActionSpaceFromType :: ActionType -> ActionSpaces -> Maybe ActionSpace
getActionSpaceFromType at = foldl getActionSpace Nothing
  where getActionSpace acc a = if actionType a == at then Just a else acc

replaceElem :: (Eq b) => (a -> b) -> a -> [a] -> [a]
replaceElem f x xs =
  let t = f x
      replaceIf x' = let t' = f x' in if t == t' then x else x' in
  map replaceIf xs
  
giveResourceToPlayer :: Resource -> Player -> Player
giveResourceToPlayer r (Player id n b w rs hcs acs) =
  let rs' = combineThings r rs in
  Player id n b w rs' hcs acs
         
removeResourceType :: ResourceType -> Resources -> Resources
removeResourceType rt = filter (\(rt', _) -> rt /= rt')

combineThings :: (Eq t) => (t, Int) -> [(t, Int)] -> [(t, Int)]
combineThings (t, n) things = if hasType t things then map addThings things else (t,n):things
  where addThings (t', n') = if t == t' then (t', n + n') else (t', n')

hasType :: (Eq t) => t -> [(t, Int)] -> Bool
hasType t = foldl hasType' False
  where hasType' b (t',_) = b || (t' == t)

hasThings :: [(t, Int)] -> Bool
hasThings = foldl hasThing False
  where hasThing b a = b || snd a > 0

getAmount :: ResourceType -> [(ResourceType, Int)] -> Int
getAmount rt = foldl get 0
  where get acc (rt', n) = if rt == rt' then n else acc