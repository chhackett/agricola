module Types.ResourceTypes where

data HouseMaterial = WoodHouse | ClayHouse | StoneHouse
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data MaterialType = Wood | Clay | Reed | Stone
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data CropType = Grain | Veges
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data AnimalType = Sheep | Boar | Cattle
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data ResourceType =
    Food
  | Crop CropType
  | Material MaterialType
  | Animal AnimalType
  deriving (Read, Eq, Ord)

instance Show ResourceType where
  show rt =
    case rt of
      Food        -> "Food"
      Crop ct     -> show ct
      Material mt -> show mt
      Animal at   -> show at

type Resource = (ResourceType, Int)
type Resources = [Resource]

type Crop = (CropType, Int)
type Crops = [Crop]

data FieldType =
    Fallow
  | GrainField
  | VegeField
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type Animal = (AnimalType, Int)
type Animals = [Animal]

data AnimalCount = House | Pasture Int | UnfencedStable Int

houseToMaterial :: HouseMaterial -> MaterialType
houseToMaterial hm =
  case hm of
    WoodHouse -> Wood
    ClayHouse -> Clay
    StoneHouse -> Stone