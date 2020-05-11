{-# LANGUAGE TemplateHaskell #-}

module Types.GameState where

import Control.Lens
import qualified Data.Map as M
import Control.Monad.State
import Types.BasicGameTypes
import Types.PlayerData

type GameStateT a = StateT GameState IO a

data GameAction = GameAction
  { _description :: String
  , _run :: GameState -> GameState
  , _input :: Maybe (IO String) }

--type GameAction = GameState -> GameState
type PlayerMap = M.Map PlayerId Player
type ActionSpaces = [ActionSpace]
type ActionTypes = [ActionType]

data GameState = GameState
  { _round :: Round
  , _phase :: Phase
  , _players :: Players
  , _currentActionSpaces :: ActionSpaces
  , _futureActionSpaces :: ActionSpaces
  , _availableMajorImprovements :: MajorImprovementTypes }

data ActionSpace = ActionSpace
  { _actionType :: ActionType
  , _resources :: Resources
  , _getConditionFunc :: GameState -> ActionSpace -> Bool
  , _action :: GameAction
  , _playerIds :: [PlayerId] }

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

instance Show GameState where
  show (GameState r p ps cas fas amis) =
    "GameState: Round: " ++ show r ++
    "\n           Phase: " ++ show p ++
    "\n           CurrentPlayer: " ++ _name (head ps) ++
    "\n           Players: " ++ show ps ++
    "\n           ActionSpaces: " ++ show cas ++
    "\n           MajorImprovements: " ++ show amis

instance Show ActionSpace where
  show (ActionSpace at rs _ _ ws) = show at ++ ": resources: " ++ show rs ++ (if not $ null ws then ": workers: " ++ show ws else "")

$(makeLenses ''GameState)

$(makeLenses ''ActionSpace)

-- Define some utility functions
nextPlayer :: Players -> Players
nextPlayer ps = tail ps ++ [head ps]

currentPlayer :: GameState -> Player
currentPlayer = head . _players