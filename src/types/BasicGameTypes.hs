{-# LANGUAGE TemplateHaskell #-}

module Types.BasicGameTypes where

import Data.List
import qualified Data.Map as M

import Control.Monad.State
import Control.Lens as L

import Types.BasicTypes
import Types.ResourceTypes
import Types.CardDeclarations

------------------------------------------
----------- Player Data ------------------
------------------------------------------

-- Definition of player data types

-- A player board contains:
-- A house, up to five locations, of wood, clay or stone
--    First two houses at (0, 0) and (0, 1)
-- Fields with grain or veges or nothing
-- Pastures (surrounded by fences)
-- Stables
-- Animals in pastures - sheep, board, or cattle
-- A player has (not at a specific location on the board)
--   Workers - up to number of house tiles (with exceptions)
--   Building materials: wood, clay, reed, stone
--   Crop types: grain or veges
--   Food

-- Represents spaces on the board ( 0 <= x <= 4, 0 <= y <= 2 )
type Space = (Int, Int)
type Spaces = [Space]

-- Represents the vertices of the board grid ( 0 <= x <= 5, 0 <= y <= 3 )
type Node = (Int, Int)
type Nodes = [Node]

type Edge = (Node, Node)
type Edges = [Edge]

type Players = [Player]

-- 1: List of all spaces inside the pasture, 2: The animal type, and how many, 3: List of spaces containing a stable inside this pasture
type Pasture = (Spaces, Maybe Animal, Spaces)
type Pastures = [Pasture]

data Board = Board
  { _houses          :: (Spaces, HouseMaterial)
  , _houseAnimal     :: Maybe AnimalType    -- can only have 1 animal in your house, so specify the type only
  , _fields          :: [(Space, Maybe Crop)]
  , _pastures        :: Pastures
  , _unfencedStables :: [(Space, Maybe Animal)] }
  deriving (Show, Read, Eq, Ord)

data PersonalSupply = PersonalSupply
  { _food  :: Int
  , _grain :: Int
  , _veges :: Int
  , _wood  :: Int
  , _clay  :: Int
  , _reed  :: Int
  , _stone :: Int }
  deriving (Show, Read, Eq, Ord, Bounded)

data Player = Player
  { _playerId       :: PlayerId
  , _name           :: String
  , _board          :: Board
  , _workers        :: NumWorkers
  , _personalSupply :: PersonalSupply
  , _hand           :: CardInfos -- occupations, minors
  , _activeCards    :: CardInfos -- occupations, minors, majors
  , _beggingCards   :: Int }
  deriving (Read)

instance Show Player where
  show p = "\n  Name: " ++ _name p ++ "\n  Board: " ++ show (_board p) ++ "\n  NumWorkers: " ++ show (_workers p) ++ "\n  Supply: " ++ show (_personalSupply p) ++
           "\n  Hand (Occupations): " ++ show (filter isOccupation $ _hand p) ++ "\n  Hand (Minors): " ++ show (filter isMinor $ _hand p) ++
           "\n  Active Majors: " ++ show (filter isMajor $ _activeCards p) ++ 
           "\n  Active Minors: " ++ show (filter isMinor $ _activeCards p) ++
           "\n  Active Occupations: " ++ show (filter isOccupation $ _activeCards p) ++  "\n  BeggingCards: " ++ show (_beggingCards p)

$(makeLenses ''Board)
$(makeLenses ''PersonalSupply)
$(makeLenses ''Player)

------------------------------------------
----------- Action Types -----------------
------------------------------------------

-- After an action is performed, it should be possible to describe the action with some combination of the following
-- 'action primitives'. Useful for determining if further actions are triggered/allowed.
-- How do we model the different effects and abilities that the different cards provide.
-- There are several mechanics:
--   - when you take an action you get some extra bonus on top of the standard action bonus
--   - when you take an action you can choose to do something else
--   - the card offers its own action that you can use at any time

data ActionPrimitive =
    PhaseChange Phase
  | RoundChange Round
  | StartingPlayer
  | ExtendHouse
  | RenovateHouse
  | FamilyGrowth
  | BuildFences
  | BuildStables
  | PlowField
  | SowField CropType
  | BakeBread                                    -- just a particular form of converting resources - in this case, grain to food. But its a very common pattern.
  | ConvertAnimalToFood AnimalType               -- another form of converting resources - in this case, animals to food.
  | TakeResources Resources                      -- taking resources from somewhere (general supply, an action space) and putting them in your personal supply
  | ReplenishResources Resource                  -- some cards 'replenish' their resources in the replenish phase
  | PutResourcesOnCard ResourceType              -- Some actions say to put a resource on a particular action space or card (from general supply usually)
  | PayResources ResourceType                    -- pay the cost of some action, resources go back to general supply
  | GiveResourceToPlayer ResourceType            -- some cards require that you pay another player. Or it could be optional to gain a benefit
  | PlayMajorImprovement CardName
  | ReturnMajorImprovement CardName              -- major improvements can be put back for others to buy
  | PlayOccupation CardName
  | PlayMinorImprovement CardName
  | HarvestFields Crops
  | BreedAnimals Animals
  deriving (Show, Read, Eq, Ord)

type ActionPrimitives = [ActionPrimitive]

-- After an action is evaluated, that last action primitive to be executed is supplied to determine which
-- subsequent actions are allowed
-- type ActionEvent = ActionPrimitive
-- type ActionEvents = [ActionEvent]

type Description = String

type ActionSpaceId = Int
type ActionSpaceIds = [ActionSpaceId]

------------------------------------------
------------ Game State ------------------
------------------------------------------

type ActionSpaceMap = M.Map ActionSpaceId ActionSpace

type RoundActionSpaceIdMap = M.Map Round ActionSpaceId

data GameState = GameState
  { _round :: Round
  , _phase :: Phase
  , _players :: Players
  , _actionSpaceMap :: ActionSpaceMap
  , _futureActionSpaces :: [ActionSpace]
  , _actionSpaceRoundMap :: RoundActionSpaceIdMap
  , _availableMajorImprovements :: CardInfos
  , _eventHistory :: ActionPrimitives
  , _currentActionId :: ActionSpaceId
  , _nextStartingPlayer :: PlayerId }

type GameStateT a = StateT GameState IO a

type SimpleActionType = GameStateT ActionPrimitives

type EitherActionType = Either SimpleActionType (ActionSpaceId -> SimpleActionType)

type PlayerCountMap = M.Map PlayerId Int

type ReservedResourceMap = M.Map PlayerId Resource

-- To determine whether an action is allowed, the last action performed and the current game state are supplied
type ActionAllowedFunc = GameState -> Bool

type ActionAllowedMap = M.Map ActionSpaceId ActionAllowedFunc

data ActionSpace = ActionSpace
  { _actionSpaceId :: ActionSpaceId
  , _description :: Description
  , _replenishment :: Maybe Resource
  , _action :: SimpleActionType
  , _actionAllowed :: ActionAllowedFunc
  , _resources :: Resources
  -- , _reservedResources :: ReservedResourceMap
  , _workersMap :: PlayerCountMap
  }

$(makeLenses ''ActionSpace)
$(makeLenses ''GameState)

type ActionSpaces = [ActionSpace]

instance Show ActionSpace where
  show (ActionSpace id desc _ _ _ rs workers) =
    desc ++ ", Id: " ++ show id ++
    ", Workers: " ++ showWorkers workers ++
    ", Resources: " ++ show rs

showWorkers :: PlayerCountMap -> String
showWorkers pcm =
  if M.null pcm
    then "None"
    else "[" ++ intercalate ", " (map toString $ M.toList pcm) ++ "]"
  where
    toString :: (PlayerId, Int) -> String
    toString (pid, n) = show pid ++ ": " ++ show n

instance Show GameState where
  show (GameState round phase players actions futureActions _ majors _ _ nextStart) =
    "GameState: Round: " ++ show round ++
    "\n           Phase: " ++ show phase ++
    "\n           CurrentPlayer: " ++ _name (head players) ++
    "\n           Players: " ++ foldl (\s p -> s ++ show p ++ "\n") "" players ++
    "\n           All Action Spaces: " ++ showActions actions ++
    "\n           Available Major Improvements: " ++ show majors ++
    "\n           Next Round Starting Player: " ++ show nextStart

showActions :: ActionSpaceMap -> String
showActions asm =
  if M.null asm
    then "None"
    else concatMap build $ M.elems asm
  where
    build :: ActionSpace -> String
    build as = "\n\t                         " ++ show as

-- The game has tokens of various types, and places on the board where tokens can be played. It would
-- be helpful to have data types to represent these different areas/tokens. This can help when specifying the
-- behavior of the different actions.

-- data BoardArea =
--   GeneralSupply |          -- where resource are stored until a player takes them
--   ActionSpaceArea |            -- where action cards go, including the round cards and the green action cards
--   PlayersUnusedTokensSupply |  -- tokens that are not used at the moment (fences, extra workers, stables go here until used)
--   PlayerPersonalSupply |   -- where building materials and food are stored after a player takes them (not animals though)
--   PlayerBoardSpace |       -- represents the 15 spaces on the players personal board (can put buildings, fields, animals - if fenced, stables)
--   PlayerBoardEdge |        -- represents edges on the players personal board (only fences go there)
--   PlayerCardArea |         -- represents a card that a player has 'played' so it is face up in front of a player (tokens can be placed there)
--   PlayerHand               -- represents the hand of cards a player has (occupation and minor improvement cards are here until they are played)

-- data GameToken =
--   Worker |
--   Fence |
--   Stable |
--   ActionCard |
--   RoundCard |
--   MajorImprovementCard MajorImprovementType |
--   MinorImprovementCard MinorImprovementType |
--   OccupationCard OccupationType |
--   Room HouseMaterial |
--   Field |
--   ResourceToken ResourceType
--   deriving (Show, Read, Eq, Ord)

currentPlayer :: GameState -> Player
currentPlayer = head . _players
