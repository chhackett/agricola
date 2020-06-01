module Actions.ResourceActions where

import System.IO
import Control.Monad
import Control.Monad.State
import Actions.GameActionFuncs
import Control.Lens
import Types.GameState as GS
import Types.BasicGameTypes
import Types.PlayerData as PD
import Utils.ListUtils
import Utils.Selection

addWorkerToPlayer :: Player -> Player
addWorkerToPlayer = over workers (+1)

getResourcesDescription :: ResourceType -> String
getResourcesDescription rs = "Take all resources of type [" ++ show rs ++ "]"

giveResourcesAction :: ResourceType -> ActionType -> GameStateT ()
giveResourcesAction rt at = do
  modify $ giveResourcesOfTypeToCurrentPlayer rt at
  return ()

--liftActionToIO :: (GameState -> GameState) -> GameState -> IO GameState
--liftActionToIO f gs = lift . f gs

-- Take all resources of the specified resource type from the action space specified by the action type and give them to the current player
giveResourcesOfTypeToCurrentPlayer :: ResourceType -> ActionType -> GameState -> GameState
giveResourcesOfTypeToCurrentPlayer rt at gs =
  let cas = GS._currentActionSpaces gs
      maybeA = getFirstElem GS._actionType at cas in
  case maybeA of
    Nothing -> gs
    Just actionSpace ->
      let cp = GS.currentPlayer gs
          rs = GS._resources actionSpace
          amount = getAmount rt rs in
      if amount > 0
      then
        let cp' = giveResourceToPlayer (rt, amount) cp
            ps' = replaceElem PD._playerId cp' (GS._players gs)
            rs' = removeResourceType rt rs
            actionSpace' = actionSpace { GS._resources = rs' }
            cas' = replaceElem GS._actionType actionSpace' cas in
        gs { GS._players = ps' } { GS._currentActionSpaces = cas' }
      else gs

putCurrentPlayerWorkerOnActionSpace :: ActionSpace -> GameState -> GameState
putCurrentPlayerWorkerOnActionSpace a g =
  let p = GS.currentPlayer g
      n = PD._workers p in
  if n > 0
  then
    let p' = p { PD._workers = n - 1 }
        ps' = p':tail (_players g)
        pids = GS._playerIds a
        a' = a { GS._playerIds = PD._playerId p : pids}
        cas = GS._currentActionSpaces g
        cas' = replaceElem GS._actionType a' cas in
    g { _players = ps' } { _currentActionSpaces = cas' }
  else g

giveResourceToCurrentPlayer :: Resource -> GameState -> GameState
giveResourceToCurrentPlayer r gs = 
  let p = currentPlayer gs
      p' = giveResourceToPlayer r p in
  gs { _players = p':tail (_players gs) }

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

getAmount :: (Eq a) => a -> [(a, Int)] -> Int
getAmount rt = foldl get 0
  where get acc (rt', n) = if rt == rt' then n else acc