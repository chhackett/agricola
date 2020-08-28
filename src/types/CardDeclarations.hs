module Types.CardDeclarations where

import qualified Data.Map as M

import Types.BasicTypes
import Types.ResourceTypes

-- Major and Minor improvements have features that are common to all cards:
--   Cost to play
--   Victory Point bonuses
-- Minor improvements has some features specific to them:
--   Which deck they go with (Easy, Interactive, Komplex)
--   When played, some cards are passed to the next player
--   Some minor improvements can only be played after a player has played a certain
--   number of Occupation cards.

data Card =
  MajorImprovement MajorImprovementType Cost BonusPoints |
  MinorImprovement MinorImprovementType Costs BonusPoints DeckType |
  Occupation OccupationType DeckType NumPlayers
  deriving (Show, Read, Eq, Ord)

type Cards = [Card]

type MajorImprovementTypes = [MajorImprovementType]
type MinorImprovementTypes = [MinorImprovementType]
type OccupationTypes = [OccupationType]

data MajorImprovementType =
  Fireplace1 |
  Fireplace2 |
  CookingHearth1 |
  CookingHearth2 |
  StoneOven |
  ClayOven |
  Pottery |
  Joinery |
  BasketmakersWorkshop |
  Well
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

majorImprovementsMap :: M.Map MajorImprovementType Card
majorImprovementsMap = M.fromList
  [ (Fireplace1, MajorImprovement Fireplace1 (CostResources [(Material Clay, 2)]) 1)
  , (Fireplace2, MajorImprovement Fireplace2 (CostResources [(Material Clay, 3)]) 1)
  , (CookingHearth1, MajorImprovement CookingHearth1 (CostResources [(Material Clay, 4)]) 1)
  , (CookingHearth2, MajorImprovement CookingHearth2 (CostResources [(Material Clay, 5)]) 1)
  , (StoneOven, MajorImprovement StoneOven (CostResources [(Material Clay, 1), (Material Stone, 2)]) 3)
  , (ClayOven, MajorImprovement ClayOven (CostResources [(Material Clay, 3), (Material Stone, 1)]) 2)
  , (Pottery, MajorImprovement Pottery (CostResources [(Material Clay, 2), (Material Stone, 2)]) 2)
  , (Joinery, MajorImprovement Joinery (CostResources [(Material Wood, 2), (Material Stone, 2)]) 2)
  , (BasketmakersWorkshop, MajorImprovement BasketmakersWorkshop (CostResources [(Material Reed, 2), (Material Stone, 2)]) 2)
  , (Well, MajorImprovement Well (CostResources [(Material Wood, 1), (Material Stone, 3)]) 4)
  ]

-- data FixedActionSpaceType =
--   BuildRoomAndOrStables |
--   StartingPlayerAndOrMinorImprovement |
--   StartingPlayerAndStorehouse |
--   TakeGrain |
--   PlowField |
--   BuildStableAndOrBakeBread |
--   PlayOccupation |
--   TakeWood |
--   FixedTakeClay |
--   TakeReed |
--   Fishing
--   deriving (Show, Read, Eq, Enum, Ord, Bounded)

-- data RoundCardType =
--   SowAndOrBakeBread |
--   TakeSheep |
--   Fences |
--   MajorOrMinorImprovement |
--   AfterFamilyGrowthAlsoImprovement |
--   AfterRenovationAlsoImprovement |
--   TakeStoneStage2 |
--   TakeStoneStage4 |
--   TakeVege |
--   TakeBoar |
--   TakeCattle |
--   PlowAndOrSow |
--   FamilyGrowthWithoutRoom |
--   AfterRenovationAlsoFences
--   deriving (Show, Read, Eq, Enum, Ord, Bounded)

-- "Easy" minor improvement deck
data MinorImprovementType =
  AnimalPen |
  MarketStall |
  FeedPellets |
  PrivateForest |
  ClayRoof |
  BuildersTrowel |
  Outhouse |
  Axe |
  Basket |
  Plane |
  FruitTree |
  Windmill |
  ButterChurn |
  ReedPond |
  BeanField |
  AnimalYard |
  MiniPasture |
  ClaySupports |
  Clogs |
  HalfTimberedHouse |
  StoneHouseExtension |
  Canoe |
  GypsysCrock |
  LettucePatch |
  MadonnaStatue |
  CattleMarket |
  StoneTongs |
  SimpleFireplace |
  Dovecote |
  RidingPlow |
  Spindle |
  WoodFiredOven |
  Raft |
  BakersOven |
  FieldCard |
  DrinkingTrough |
  Millstone |
  HelpfulNeighbors |
  SackCart |
  PotatoDibber |
  StableCard |
  Ceramics |
  CornScoop |
  FishingRod |
  Spices |
  WritingDesk |
  CarpPond |
  TurnwrestPlow |
  Quarry |
  ShepherdsPipe |
  BakingTray |
  Manger |
  BuildingMaterialCard
    deriving (Show, Read, Eq, Enum, Ord, Bounded)

allMinorImprovements :: Cards
allMinorImprovements =
  [ MinorImprovement AnimalPen [CostResources [(Material Wood, 1)], CostOccs 4] 1 Easy
  , MinorImprovement MarketStall [CostResources [(Food, 1)]] 0 Easy
  , MinorImprovement FeedPellets [] 0 Easy
  , MinorImprovement PrivateForest [CostResources [(Food, 2)]] 0 Easy
  , MinorImprovement ClayRoof [CostOccs 1] 1 Easy
  , MinorImprovement BuildersTrowel [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement Outhouse [CostResources [(Material Wood, 1), (Material Clay, 1)]] 2 Easy
  , MinorImprovement Axe [CostResources [(Material Wood, 1), (Material Stone, 1)]] 0 Easy
  , MinorImprovement Basket [CostResources [(Material Reed, 1)]] 0 Easy
  , MinorImprovement Plane [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement FruitTree [CostOccs 3] 1 Easy
  , MinorImprovement Windmill [CostResources [(Material Wood, 3), (Material Stone, 1)]] 2 Easy
  , MinorImprovement ButterChurn [CostResources [(Material Wood, 2)]] 0 Easy
  , MinorImprovement ReedPond [CostOccs 3] 1 Easy
  , MinorImprovement BeanField [CostOccs 2] 1 Easy
  , MinorImprovement AnimalYard [CostResources [(Material Wood, 2)], CostOccs 1] 1 Easy
  , MinorImprovement MiniPasture [CostResources [(Food, 2)]] 0 Easy
  , MinorImprovement ClaySupports [CostResources [(Material Wood, 2)]] 0 Easy
  , MinorImprovement Clogs [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement HalfTimberedHouse [CostResources [(Material Wood, 1), (Material Clay, 1), (Material Reed, 1), (Material Stone, 2)]] 0 Easy
  , MinorImprovement StoneHouseExtension [CostResources [(Material Reed, 1), (Material Stone, 1)]] 0 Easy
  , MinorImprovement Canoe [CostResources [(Material Wood, 2)], CostOccs 2] 1 Easy
  , MinorImprovement GypsysCrock [CostResources [(Material Clay, 2)]] 1 Easy
  , MinorImprovement LettucePatch [CostOccs 3] 1 Easy
  , MinorImprovement MadonnaStatue [] 2 Easy
  , MinorImprovement CattleMarket [CostResources [(Animal Sheep, 1)]] 0 Easy
  , MinorImprovement StoneTongs [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement SimpleFireplace [CostResources [(Material Wood, 1)]] 1 Easy
  , MinorImprovement Dovecote [CostResources [(Material Stone, 2)]] 2 Easy
  , MinorImprovement RidingPlow [CostResources [(Material Wood, 1)], CostOccs 3] 0 Easy
  , MinorImprovement Spindle [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement WoodFiredOven [CostResources [(Material Wood, 3), (Material Stone, 1)]] 2 Easy
  , MinorImprovement Raft [CostResources [(Material Wood, 2)]] 1 Easy
  , MinorImprovement BakersOven [] 3 Easy
  , MinorImprovement FieldCard [CostResources [(Food, 1)]] 0 Easy
  , MinorImprovement DrinkingTrough [CostResources [(Material Wood, 2)]] 1 Easy
  , MinorImprovement Millstone [CostResources [(Material Stone, 1)]] 0 Easy
  , MinorImprovement HelpfulNeighbors [CostResources [(Material Wood, 1), (Material Clay, 1)]] 0 Easy
  , MinorImprovement SackCart [CostResources [(Material Wood, 2)], CostOccs 2] 0 Easy
  , MinorImprovement PotatoDibber [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement StableCard [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement Ceramics [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement CornScoop [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement FishingRod [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement Spices [] 0 Easy
  , MinorImprovement WritingDesk [CostResources [(Material Wood, 1)], CostOccs 2] 1 Easy
  , MinorImprovement CarpPond [CostOccs 1, CostImprovements 2] 1 Easy
  , MinorImprovement TurnwrestPlow [CostResources [(Material Wood, 1)], CostOccs 2] 0 Easy
  , MinorImprovement Quarry [CostOccs 4] 2 Easy
  , MinorImprovement ShepherdsPipe [CostResources [(Animal Sheep, 1)]] 0 Easy
  , MinorImprovement BakingTray [CostResources [(Material Wood, 1)]] 0 Easy
  , MinorImprovement Manger [CostResources [(Material Wood, 2)]] 0 Easy
  , MinorImprovement BuildingMaterialCard [] 0 Easy
  ]

minorImprovementsMap :: M.Map MinorImprovementType Card
minorImprovementsMap = M.fromList $ map build allMinorImprovements
  where
    build :: Card -> (MinorImprovementType, Card)
    build c =
      case c of
        MajorImprovement {} -> error "Not a minor improvement"
        MinorImprovement minorType costs bonusPoints deckType -> (minorType, MinorImprovement minorType costs bonusPoints deckType)
        Occupation {} -> error "Not a minor improvement"

-- "Easy" occupation deck
data OccupationType =
  ClayMixer |
  HedgeKeeper |
  Stablemaster |
  Merchant |
  MushroomCollector |
  ChiefsDaughter |
  Mason |
  MasterBuilder |
  Maid |
  PlowMaker |
  Tutor |
  Renovator |
  Chief |
  ClayDeliveryman |
  Conservator |
  Baker |
  PlowDriver |
  Grocer |
  MeatSeller |
  WoodCutter |
  DockWorker |
  StoneCarrier |
  MasterBrewer |
  SeasonalWorker |
  Mendicant |
  Carpenter |
  Stablehand |
  LordOfTheManor |
  AnimalKeeper |
  Conjurer |
  UnderGardener |
  Cook |
  MasterBaker |
  Turner |
  Dancer |
  HeadOfTheFamily |
  CharcoalBurner |
  WoodenHutBuilder |
  FieldWarden |
  BerryPicker |
  Swineherd |
  Fisherman |
  Potter |
  Quarryman |
  MasterForester |
  TenantFarmer |
  Stockman |
  StoryTeller |
  Pastor |
  Braggart |
  CattleWhisperer |
  Reeve |
  RatCatcher |
  BasketMaker |
  Patron |
  LandAgent |
  BreadSeller |
  HobbyFarmer |
  GreenGrocer |
  Shepherd |
  GuildMaster |
  StoneCutter |
  ReedCollector |
  Academic |
  Farmer |
  BrushMaker |
  Thatcher |
  ClaySeller |
  YeomanFarmer |
  HutBuilder |
  MasterShepherd |
  ClayFirer |
  EstateManager
    deriving (Show, Read, Eq, Enum, Ord, Bounded)

allOccupations :: Cards
allOccupations =
  [ Occupation ClayMixer Easy 1
  , Occupation HedgeKeeper Easy 1
  , Occupation Stablemaster Easy 1
  , Occupation Merchant Easy 1
  , Occupation MushroomCollector Easy 1
  , Occupation ChiefsDaughter Easy 1
  , Occupation Mason Easy 1
  , Occupation MasterBuilder Easy 1
  , Occupation Maid Easy 1
  , Occupation PlowMaker Easy 1
  , Occupation Tutor Easy 1
  , Occupation Renovator Easy 1
  , Occupation Chief Easy 1
  , Occupation ClayDeliveryman Easy 1
  , Occupation Conservator Easy 1
  , Occupation Baker Easy 1
  , Occupation PlowDriver Easy 1
  , Occupation Grocer Easy 1
  , Occupation MeatSeller Easy 1
  , Occupation WoodCutter Easy 1
  , Occupation DockWorker Easy 1
  , Occupation StoneCarrier Easy 1
  , Occupation MasterBrewer Easy 1
  , Occupation SeasonalWorker Easy 1
  , Occupation Mendicant Easy 1
  , Occupation Carpenter Easy 1
  , Occupation Stablehand Easy 1
  , Occupation LordOfTheManor Easy 1
  , Occupation AnimalKeeper Easy 4
  , Occupation Conjurer Easy 4
  , Occupation UnderGardener Easy 4
  , Occupation Cook Easy 4
  , Occupation MasterBaker Easy 4
  , Occupation Turner Easy 3
  , Occupation Dancer Easy 4
  , Occupation HeadOfTheFamily Easy 4
  , Occupation CharcoalBurner Easy 3
  , Occupation WoodenHutBuilder Easy 3
  , Occupation FieldWarden Easy 4
  , Occupation BerryPicker Easy 3
  , Occupation Swineherd Easy 4
  , Occupation Fisherman Easy 3
  , Occupation Potter Easy 3
  , Occupation Quarryman Easy 3
  , Occupation MasterForester Easy 4
  , Occupation TenantFarmer Easy 4
  , Occupation Stockman Easy 4
  , Occupation StoryTeller Easy 4
  , Occupation Pastor Easy 4
  , Occupation Braggart Easy 3
  , Occupation CattleWhisperer Easy 4
  , Occupation Reeve Easy 3
  , Occupation RatCatcher Easy 3
  , Occupation BasketMaker Easy 4
  , Occupation Patron Easy 4
  , Occupation LandAgent Easy 3
  , Occupation BreadSeller Easy 3
  , Occupation HobbyFarmer Easy 4
  , Occupation GreenGrocer Easy 3
  , Occupation Shepherd Easy 4
  , Occupation GuildMaster Easy 3
  , Occupation StoneCutter Easy 3
  , Occupation ReedCollector Easy 3
  , Occupation Academic Easy 3
  , Occupation Farmer Easy 4
  , Occupation BrushMaker Easy 3
  , Occupation Thatcher Easy 3
  , Occupation ClaySeller Easy 4
  , Occupation YeomanFarmer Easy 3
  , Occupation HutBuilder Easy 4
  , Occupation MasterShepherd Easy 4
  , Occupation ClayFirer Easy 4
  , Occupation EstateManager Easy 3
  ]

isOccupationAllowed :: NumPlayers -> OccupationType -> Bool
isOccupationAllowed n ot =
  any (isAllowed n ot) allOccupations
  where
    isAllowed :: NumPlayers -> OccupationType -> Card -> Bool
    isAllowed n ot card =
      case card of
        Occupation ot' _ n' -> ot == ot' && n >= n'
        _                   -> error "Not an occupation card"
