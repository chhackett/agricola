module Types.GameState where

import qualified Data.Map as M
import Control.Monad.State
import Types.BasicGameTypes
import Types.PlayerData
import Types.ResourceTypes

type GameStateT a = StateT GameState IO a

data GameState = GameState
  { _round :: Round
  , _phase :: Phase
  , _currentPlayer :: Player
  , _players :: Players
  , _currentActionSpaces :: ActionSpaces
  , _futureActionSpaces :: ActionSpaces
  , _availableMajorImprovements :: MajorImprovementTypes }
  -- deriving (Show)

instance Show GameState where
  show (GameState r p cp ps cas fas amis) =
    "GameState: Round: " ++ show r ++
    "\n           Phase: " ++ show p ++
    "\n           CurrentPlayer: " ++ show (_name cp) ++
    "\n           Players: " ++ show ps ++
    "\n           ActionSpaces: " ++ show cas ++
    "\n           MajorImprovements: " ++ show amis

type GameAction = GameState -> GameState

type PlayerMap = M.Map PlayerId Player

data ActionSpace = ActionSpace
  { actionType :: ActionType
  , resources :: Resources
  , getConditionFunc :: GameState -> ActionSpace -> Bool
  , run :: GameAction
  , workers :: [PlayerId] }

type ActionSpaces = [ActionSpace]

instance Show ActionSpace where
  show (ActionSpace at rs _ _ ws) = show at ++ ": resources: " ++ show rs ++ (if not $ null ws then ": workers: " ++ show ws else "")

type ActionTypes = [ActionType]
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
