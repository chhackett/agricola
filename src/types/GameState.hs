{-# LANGUAGE TemplateHaskell #-}

module Types.GameState where

import Control.Lens as L
import qualified Data.Map as M
import Control.Monad.State
import Types.BasicGameTypes
import Types.PlayerData
import Utils.Selection

type GameStateT a = StateT GameState IO a

data GameState = GameState
  { _round :: Round
  , _phase :: Phase
  , _players :: Players
  , _currentActionSpaces :: ActionSpaces
  , _futureActionSpaces :: ActionSpaces
  , _availableMajorImprovements :: MajorImprovementTypes }

data ActionSpace = ActionSpace
  { _actionType :: ActionType
  , _description :: String
  , _resources :: Resources
  , _playerIds :: [PlayerId] }

type ActionSpaces = [ActionSpace]

data GameAction = GameAction
  { _isAllowed :: GameState -> Bool
  , _run :: GameStateT ()}

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
  TakeStone |
  TakeVege |
  TakeBoar |
  TakeCattle |
  PlowAndOrSow |
  FamilyGrowthWithoutRoom |
  AfterRenovationAlsoFences
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type ActionTypes = [ActionType]

type GameActionMap = M.Map ActionType GameAction

-- This action requires user to input the location of field to sow, resource type to sow it with (grain or vege) and how many (if any) grains to convert to food
-- instance GameAction SowAndOrBakeBread (Spaces, ResourceType, Int) where
--   description = "SowAndOrBakeBread"
--   isAllowed = ifNoWorkers
--   run _ gs = gs
--   input = return ([], Grain, 0)

-- instance GameAction BuildRoomAndOrStables () where
--   description = "BuildRoomAndOrStables"
--   isAllowed = ifNoWorkers
--   run _ gs = gs
--   input = return ()

-- class RunGameAction a where
--   run :: a -> GameState -> GameState
--   getInput :: IO a

instance Show GameState where
  show (GameState r p ps cas fas amis) =
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
