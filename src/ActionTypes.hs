module ActionTypes where

import Control.Monad.State
import qualified Data.Map as M

import Types.BasicGameTypes
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

data ActionCondition =
  SimpleCondition When Who Cost |
  ComplexCondition ActionCondition BooleanOp ActionCondition

data GameMode = FamilyGame | NormalRules
  deriving (Show, Read, Eq, Ord)

data CompositeAction =
  SimpleAction ActionPrimitive |
  ComplexAction ActionPrimitive BooleanOp CompositeAction

data When =
  AnyTime |
  WhenThisCardIsPlayed |
  AtStartOfRound Round |
  AtStartOfPhase Phase |
  AtEndOfGame |
  WhenAction ActionPrimitive
  deriving (Show, Read, Eq, Ord)

data Who =
  Anyone |
  You |
  AnotherPlayer
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

newtype Cost = Cost Resources

data Number =
  Any  |   -- Zero or more
  Some |   -- One or more
  Zero |   -- Exactly 0
  One  |   -- Exactly 1
  Two  |   -- Exactly 2
  All      -- All of the things
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data BooleanOp = And | Or | Xor
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

-----------------------------------------------
-- Basic action allowed function definitions --
-----------------------------------------------

allConditions :: [ActionAllowedFunc] -> ActionAllowedFunc
allConditions fs gs = all (\f -> f gs) fs

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

getAmountInPersonalSupply :: ResourceType -> PersonalSupply -> Int
getAmountInPersonalSupply Food  = _food
getAmountInPersonalSupply (Crop Grain) = _grain
getAmountInPersonalSupply (Crop Veges) = _veges
getAmountInPersonalSupply (Material Wood)  = _wood
getAmountInPersonalSupply (Material Clay)  = _clay
getAmountInPersonalSupply (Material Reed)  = _reed
getAmountInPersonalSupply (Material Stone) = _stone

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
         