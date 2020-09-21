module ActionFunctions where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M

import ResourceHelperFuncs
import Types.BasicTypes
import Types.BasicGameTypes
import Types.ResourceTypes
import Utils.Selection

-- How to handle actions that have a Cost (in resources) to perform them? Some actions have a fixed cost, but that
-- cost can be modified by cards. Some cards reduce the cost by some amount. Other cards changed the cost to a new fixed cost.
-- How to handle cards that give a benefit when a condition occurs, like 'at the start of a round get X'?
-- Other cards give a bonus when you take a certain action. Others define their own actions you can use anytime.
-- There are so many triggers, bonuses, effects etc. Maybe I can break down all of these into a sequence of 'primitive' actions.
-- List of primitive actions:
--    Build: room, stable, fence
--    Plow, Sow, Bake Bread
--    Play occupation or improvement
--    Take resource or Pay resource
--    Put resource somewhere on the board (on action space or occupation/improvement) that you get later

-- Question: Is an action allowed (right now)? Answer: Yes, if the conditions for that action are all met.

-- data ActionCondition =
--   SimpleCondition When Who Cost |
--   ComplexCondition ActionCondition BooleanOp ActionCondition

-- data Composite a = Simple a | Complex a BooleanOp Composite

-----------------------------------------------
--------- Function to register actions --------
-----------------------------------------------

registerEventTriggeredAction :: EventType -> SimpleActionType -> GameState -> GameState
registerEventTriggeredAction et action gs =
  let m = gs ^. eventTriggeredActionMap in
  gs & eventTriggeredActionMap .~ addAction et action m
  where
    addAction :: EventType -> SimpleActionType -> EventTriggeredActionsMap -> EventTriggeredActionsMap
    addAction et action m =
      case M.lookup et m of
        Nothing -> M.insert et [action] m
        Just actions -> M.insert et (action:actions) m

-----------------------------------------------
-- Basic action allowed function definitions --
-----------------------------------------------

allConditions :: [ActionAllowedFunc] -> ActionAllowedFunc
allConditions fs gs = all (\f -> f gs) fs

anyConditions :: [ActionAllowedFunc] -> ActionAllowedFunc
anyConditions fs gs = any (\f -> f gs) fs

meetsOneOrTheOtherCondition :: (ActionSpaceId -> ActionAllowedFunc, ActionSpaceId -> ActionAllowedFunc) -> ActionSpaceId -> ActionAllowedFunc
meetsOneOrTheOtherCondition (a, b) id gs = any (\f -> f id gs) [a, b]

ifNoWorkers :: ActionSpaceId -> ActionAllowedFunc
ifNoWorkers asId gs =
  let as = _actionSpaceMap gs M.! asId in
  M.null $ _workersMap as

ifHaveResources :: Resources -> ActionAllowedFunc
ifHaveResources rs gs =
  let ps = _personalSupply $ currentPlayer gs in
  all (\r -> getAmountInPersonalSupply (fst r) ps >= snd r) rs

isRound :: Round -> ActionAllowedFunc
isRound r gs = _round gs >= r

alwaysAllowed :: ActionAllowedFunc
alwaysAllowed _ = True

optionalDoGS :: String -> a -> GameStateT a -> GameStateT a
optionalDoGS prompt nope action = do
  lift $ putStrLn prompt
  yes <- lift $ getNextSelection yesNoOptions
  if yes then action
         else return nope

optionalDoIO :: String -> a -> IO a -> IO a
optionalDoIO prompt nope action = do
  putStrLn prompt
  yes <- getNextSelection yesNoOptions
  if yes then action
         else return nope

noOpAction :: ActionType
noOpAction = AnytimeAction "No-op" doNothing

doNothing :: SimpleActionType
doNothing = return []
