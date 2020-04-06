module ResourceTypes where

data CropType = Grain | Veges deriving (Show, Read, Eq, Ord)
type Crop = (CropType, Int)
type Crops = [Crop]

data AnimalType = Sheep | Boar | Cattle deriving (Show, Read, Eq, Ord)
type Animal = (AnimalType, Int)
type Animals = [Animal]

data MaterialType = Wood | Clay | Reed | Stone deriving (Show, Read, Eq, Ord)
type Material = (MaterialType, Int)
type Materials = [Material]

type Money = Int
type Food = Int

type Resources = [(Food, Money, Materials, Crops, Animals)]
