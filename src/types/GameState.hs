module Types.GameState where

import System.Random
import System.Random.Shuffle
import Types.PlayerData
import Types.ResourceTypes
import Types.ActionTypes
import Types.CardData

data ActionSpaceState = ActionSpaceState
  { actionType :: ActionSpaceType
  , usedBy :: PlayerId
  , resources :: Resources }
  deriving (Show, Read, Eq)

type ActionSpaceStates = [ActionSpaceState]

data GameState = GameState { round :: Round
                           , phase :: Phase
                           , currentPlayer :: Player
                           , players :: Players
                           , actionSpaceStates :: ActionSpaceStates }
  deriving (Show, Read)

initGameState :: (RandomGen g) => g -> GameState
initGameState g =
  let occupations = getSevenRandoms g :: OccupationTypes
      improvements = getSevenRandoms g :: ImprovementTypes
      player = Player 0
                      (Board ([(0,0),(0,1)], Wood) [] [] [])
                      2   -- workers
                      0   -- money
                      0   -- food
                      []  -- crops
                      []  -- materials
                      (occupations, improvements)
                      ([], []) in
  GameState 1 StartRound player [player] []

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a]
getSevenRandoms g =
  let es = [minBound .. maxBound]
      l = length es in
  take 7 $ shuffle' es l g

-- StartRound phase: draw a new round card
-- Replenish: add new goods and animals
-- Work: Place family member on unoccupied action space
-- Return home: put workers back in the house
-- Harvest: Field phase: remove 1 grain or vege from each sown field put them in personal supply
-- Harvest: Feed: pay 2 food/worker. Offspring cost 1 food.
-- Harvest: Breed: for each 2 animals fo the same type get one more animal of that type

data Phase =
  StartRound |
  Replenish |
  Work |
  ReturnHome |
  Harvest |
  Field |
  Feed |
  Breed |
  EndRound
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type ActionCardRoundsMap = [(ActionCardType, Stage)]

actionCardToRound :: ActionCardRoundsMap
actionCardToRound =
  [ (SowAndOrBakeBread,         0),
    (MajorOrMinorImprovement,   0),
    (Fences,                    0),
    (TakeSheep,                 0),
    (AfterRenovationAlsoImprovement,    1),
    (AfterFamilyGrowthAlsoImprovement,  1),
    (TakeStone1,                1),
    (TakeBoar,                  2),
    (TakeVege,                  2),
    (TakeCattle,                3),
    (TakeStone2,                3),
    (FamilyGrowthWithoutRoom,   4),
    (PlowAndOrSow,              4),
    (AfterRenovationAlsoFences, 5)
  ]
