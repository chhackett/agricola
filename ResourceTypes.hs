module ResourceTypes where

data Food = Grain Int | Veges Int deriving (Show, Eq, Ord)

data Animals = Sheep Int | Boar Int | Cattle Int  deriving (Show, Eq, Ord)

data Materials = Wood Int | Clay Int | Reed Int | Stone Int  deriving (Show, Eq, Ord)

type Money = Int

type Resources = (Food, Animals, Materials, Money)
