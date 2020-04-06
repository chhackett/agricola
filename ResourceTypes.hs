module ResourceTypes where

data Food = Grain Int | Veges Int deriving (Show, Read, Eq, Ord)

data Animals = Sheep Int | Boar Int | Cattle Int  deriving (Show, Read, Eq, Ord)

data Materials = Wood Int | Clay Int | Reed Int | Stone Int  deriving (Show, Read, Eq, Ord)

type Money = Int

type Resources = (Food, Animals, Materials, Money)
