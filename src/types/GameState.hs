module Types.GameState where

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
  , _currentPlayer :: PlayerId
  , _players :: Players
  , _currentActionSpaces :: ActionSpaces
  , _futureActionSpaces :: ActionSpaces
  , _availableMajorImprovements :: MajorImprovementTypes }
  deriving (Show)

type ActionSpaces = [ActionSpace]

data ActionSpace = ActionSpace
  { actionType :: ActionType
  , resources :: Resources
  , cost :: GameState -> Resources
  , isAllowed :: GameState -> Bool
  , run :: GameState -> GameState
  , workers :: [PlayerId] }

instance Show ActionSpace where
  show (ActionSpace at rs _ _ _ ws) = "\n" ++ show at ++ ": resources: " ++ show rs ++ ": workers: " ++ show ws

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
                      ( occupations, improvements)
                      ( [], [], []) in
  GameState 1
            StartRound
            pId
            [player]
            initActionSpaces
            (initFutureActionCards g3)
            []

-- Initialize the actions space cards
initActionSpaces :: ActionSpaces
initActionSpaces =
  let ats = filter (not . isRoundCard) [minBound .. maxBound] in
  map initActionSpace ats

initFutureActionCards :: (RandomGen g) => g -> ActionSpaces
initFutureActionCards g = map initActionSpace $ snd $ foldl randomize (g, []) roundCardStages
  where randomize (g', rcs) rc = let (g1, g2) = split g' in (g1, rcs ++ shuffle' rc (length rc) g2)

initActionSpace :: ActionType -> ActionSpace
initActionSpace at = ActionSpace at [] free alwaysAllowed noop []

-- costs
free :: GameState -> Resources
free gs = []

alwaysAllowed :: GameState -> Bool
alwaysAllowed gs = True

noop :: GameState -> GameState
noop gs = gs

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
  | otherwise = True

roundCardStages =
  [[SowAndOrBakeBread, TakeSheep, Fences, MajorOrMinorImprovement],
    [AfterFamilyGrowthAlsoImprovement, AfterRenovationAlsoImprovement, TakeStone],
    [TakeBoar, TakeVege],
    [TakeStone, TakeCattle],
    [PlowAndOrSow, FamilyGrowthWithoutRoom],
    [AfterRenovationAlsoFences]]
