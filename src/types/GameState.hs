module Types.GameState where

import qualified Data.Map as M
import Control.Monad.State
import System.Random
import System.Random.Shuffle
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
  deriving (Show)

type GameAction = GameState -> GameState

type PlayerMap = M.Map PlayerId Player

type ActionSpaces = [ActionSpace]

data ActionSpace = ActionSpace
  { actionType :: ActionType
  , resources :: Resources
  , isAllowed :: GameState -> Bool
  , run :: GameAction
  , workers :: [PlayerId] }

instance Show ActionSpace where
  show (ActionSpace at rs _ _ ws) = "\n" ++ show at ++ ": resources: " ++ show rs ++ ": workers: " ++ show ws

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

initGameState :: (RandomGen g) => g -> String -> GameState
initGameState g name =
  let (g1, g2) = split g
      (g3, g4) = split g2
      occupations = getSevenRandoms g1 :: OccupationTypes
      improvements = getSevenRandoms g2 :: MinorImprovementTypes
      pId = 0
      player = Player pId
                      name
                      (Board ([(0,0),(0,1)], Wood) [] [] [])
                      2     -- workers
                      []
                      (occupations, improvements)
                      ([], [], []) in
  GameState 1
            StartRound
            player
            [player]
            initActionSpaces
            (initFutureActionCards g3)
            []

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l  = length es in
  take 7 $ shuffle' es l g

isRoundCard :: ActionType -> Bool
isRoundCard a
  | a == BuildRoomAndOrStables               = False
  | a == StartingPlayerAndOrMinorImprovement = False
  | a == Take1Grain                          = False
  | a == Plow1Field                          = False
  | a == PlayOneOccupation                   = False
  | a == DayLaborer                          = False
  | a == TakeWood                            = False
  | a == TakeClay                            = False
  | a == TakeReed                            = False
  | a == Fishing                             = False
  | otherwise                                = True

-- Initialize the actions space cards
initActionSpaces :: ActionSpaces
initActionSpaces =
  let ats = filter (not . isRoundCard) [minBound .. maxBound] in
  map initActionSpace ats

initActionSpace :: ActionType -> ActionSpace
initActionSpace at = ActionSpace at [] alwaysAllowed noop []

alwaysAllowed :: GameState -> Bool
alwaysAllowed gs = True

noop :: GameAction
noop gs = gs

-- initialize future action cards list with random generator
initFutureActionCards :: (RandomGen g) => g -> ActionSpaces
initFutureActionCards g = map initActionSpace $ snd $ foldl randomize (g, []) actionCardStageMap
  where randomize (g', rcs) rc = let (g1, g2) = split g' in (g1, rcs ++ shuffle' rc (length rc) g2)

actionCardStageMap =
  [[SowAndOrBakeBread, TakeSheep, Fences, MajorOrMinorImprovement],
    [AfterFamilyGrowthAlsoImprovement, AfterRenovationAlsoImprovement, TakeStone],
    [TakeBoar, TakeVege],
    [TakeStone, TakeCattle],
    [PlowAndOrSow, FamilyGrowthWithoutRoom],
    [AfterRenovationAlsoFences]]
