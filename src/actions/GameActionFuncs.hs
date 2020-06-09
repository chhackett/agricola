module Actions.GameActionFuncs where

import Types.GameState
import Utils.ListUtils
import Utils.Selection

noop :: GameAction
noop = GameAction alwaysAllowed noIO

-- basicGameAction :: ActionType -> GameStateT () -> GameAction
-- basicGameAction at = GameAction (ifNoWorkers at)

allConditions :: [ActionAllowedFunc] -> ActionAllowedFunc
allConditions fs gs = all (\f -> f gs) fs

-- Combine two action condition functions into a single action condition function
chain :: ActionAllowedFunc -> ActionAllowedFunc -> ActionAllowedFunc
chain a1 a2 gs = all (\f -> f gs) [a1, a2]

ifNoWorkers :: ActionType -> ActionAllowedFunc
ifNoWorkers at gs =
  case lookupActionSpace at (_currentActionSpaces gs) of
    Nothing -> error "Unable to lookup action space"
    Just a -> null $ _playerIds a

alwaysAllowed :: ActionAllowedFunc
alwaysAllowed _ = True

lookupActionSpace :: ActionType -> ActionSpaces -> Maybe ActionSpace
lookupActionSpace = getFirstElem _actionType

noIO :: GameStateT ()
noIO = return ()
