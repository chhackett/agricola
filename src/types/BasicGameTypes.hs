module Types.BasicGameTypes where

type Round = Int
type Stage = Int
numRounds = 14 :: Round
numStages = 6 :: Stage

type Workers = Int

data ResourceType = Food | Wood | Clay | Reed | Stone | Grain | Veges | Sheep | Boar | Cattle
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data ResourceKind = Crop | Animal | Building
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

buildingTypes :: [ResourceType]
buildingTypes = [Wood, Clay, Reed, Stone]

animalTypes = [Sheep, Boar, Cattle]
cropTypes = [Grain, Veges]

type ResourceKinds = [ResourceKind]

type Resource = (ResourceType, Int)
type Resources = [Resource]

type MajorImprovementTypes = [MajorImprovementType]
type MinorImprovementTypes = [MinorImprovementType]
type OccupationTypes = [OccupationType]

data Phase =
  StartRound |
  Replenish |
  Work |
  ReturnHome |
  Harvest |
  HarvestField |
  HarvestFeed |
  HarvestBreed
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

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
  Field |
  DrinkingTrough |
  Millstone |
  HelpfulNeighbors |
  SackCart |
  PotatoDibber |
  Stable |
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
  BuildingMaterial
    deriving (Show, Read, Eq, Enum, Ord, Bounded)

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