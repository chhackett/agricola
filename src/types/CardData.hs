module Types.CardData where

-- Definition of occupation cards, improvement cards

type MajorImprovementTypes = [MajorImprovementType]
type ImprovementTypes = [ImprovementType]
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

data ImprovementType =
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