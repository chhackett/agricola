{-# LANGUAGE TemplateHaskell #-}

module Types.GameState where

import Control.Lens as L
import qualified Data.Map as M
import Control.Monad.State
import Types.BasicGameTypes
import Types.PlayerData
import Utils.Selection

-- How do we model the different effects and abilities that the different cards provide.
-- There are several mechanics:
--   - when you take an action you get some extra bonus on top of the standard action bonus
--   - when you take an action you can choose to do something else
--   - the card offers its own action that you can use at any time
type GameStateT a = StateT GameState IO a

data GameState = GameState
  { _round :: Round
  , _phase :: Phase
  , _players :: Players
  , _currentActionSpaces :: ActionSpaces
  , _futureActionSpaces :: ActionSpaces
  , _availableMajorImprovements :: MajorImprovementTypes
  , _gameActionMap :: GameActionMap }

data ActionSpace = ActionSpace
  { _actionType :: ActionType
  , _description :: String
  , _actionSpaceResources :: Resources
  , _playerIds :: [PlayerId] }

type ActionSpaces = [ActionSpace]

type ActionAllowedFunc = GameState -> Bool

data GameAction = GameAction
  { _isAllowed :: ActionAllowedFunc
  , _run :: GameStateT ()}

-- How to handle actions that have a Cost to perform them? Some actions have a fixed cost, but that cost can be modified by cards.
-- Some cards reduce the cost by some amount. Other cards changed the cost to a new fixed cost.
-- How to handle cards that give a benefit when a condition occurs, like 'at the start of a round get X'?
-- Other cards give a bonus when you take a certain action. Others define their own actions you can use anytime.

data CostModifierType =
  Relative |
  Absolute

data BonusType =
  Receive |
  Replace

data Bonus = Bonus
  { _type :: BonusType
  , _bonusResources :: Resources }

data ActionType =
  BuildRoomAndOrStables |
  StartingPlayerAndOrMinorImprovement |
  Take1Grain |
  Plow1Field |
  PlayOneOccupation |
  DayLaborer |
  TakeWood |
  TakeClay |
  TakeReed |
  Fishing |
  SowAndOrBakeBread |
  TakeSheep |
  Fences |
  MajorOrMinorImprovement |
  AfterFamilyGrowthAlsoImprovement |
  AfterRenovationAlsoImprovement |
  TakeStone1 |
  TakeStone2 |
  TakeVege |
  TakeBoar |
  TakeCattle |
  PlowAndOrSow |
  FamilyGrowthWithoutRoom |
  AfterRenovationAlsoFences
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type ActionTypes = [ActionType]

type GameActionMap = M.Map ActionType GameAction

instance Show GameState where
  show (GameState r p ps cas fas amis _) =
    "GameState: Round: " ++ show r ++
    "\n           Phase: " ++ show p ++
    "\n           CurrentPlayer: " ++ _name (head ps) ++
    "\n           Players: " ++ show ps ++
    "\n           ActionSpaces: " ++ show cas ++
    "\n           MajorImprovements: " ++ show amis

instance Show ActionSpace where
  show (ActionSpace at desc rs ws) = show at ++ ": resources: " ++ show rs ++ (if not $ null ws then ": workers: " ++ show ws else "")

$(makeLenses ''GameState)

$(makeLenses ''ActionSpace)

-- Define some utility functions

nextPlayer :: Players -> Players
nextPlayer ps = tail ps ++ [head ps]

currentPlayer :: GameState -> Player
currentPlayer = head . _players
