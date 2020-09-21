module AnimalFunctions where

import Data.Maybe

import Types.ResourceTypes
import Types.BasicGameTypes
import Utils.Selection


getAllAnimals :: Board -> Animals
getAllAnimals b = filter (\(_, n) -> n /= 0) [getAll Sheep b, getAll Boar b, getAll Cattle b]
  where
    getAll :: AnimalType -> Board -> Animal
    getAll at b =
      let c1 = case _houseAnimal b of
                  Nothing -> 0
                  Just at' -> if at == at' then 1 else 0
          c2 = foldl (getAnimalPasture at) 0 $ _pastures b
          c3 = foldl (getAnimalStables at) 0 $ _unfencedStables b in
      (at, c1 + c2 + c3)

    getAnimalPasture :: AnimalType -> Int -> (Spaces, Maybe Animal, Spaces) -> Int
    getAnimalPasture at n (_, ma, _) = n + getAnimal at ma

    getAnimalStables :: AnimalType -> Int -> (Space, Maybe Animal) -> Int
    getAnimalStables at n (_, ma) = n + getAnimal at ma

    getAnimal :: AnimalType -> Maybe Animal -> Int
    getAnimal at ma =
      case ma of
        Nothing -> 0
        Just (at', m) -> if at == at' then m else 0

getAnimalQuantity :: Board -> AnimalType -> Int
getAnimalQuantity b at =
  let i = if isJust $ _houseAnimal b then 1 else 0
      j = getAllInPastures at $ _pastures b
      k = getAll at $ _unfencedStables b in
  i + j + k

getAllInPastures :: Eq b => b -> [(a, Maybe (b, Int), c)] -> Int
getAllInPastures t = foldl (\acc (a, b, _) -> getSum t acc (a, b)) 0

getAll :: Eq b => b -> [(a, Maybe (b, Int))] -> Int
getAll t = foldl (getSum t) 0

getSum :: Eq b => b -> Int -> (a, Maybe (b, Int)) -> Int
getSum t n (_, Just (t', n')) = if t == t' then n + n' else n
getSum _ n (_, Nothing) = n

getAnimalStoreOptions :: Board -> Options AnimalCount
getAnimalStoreOptions b =
  let houseOption = ("In your house, containing: " ++ show (_houseAnimal b), House)
      pastureOptions = zipWith (\(ss, mAnimal, _) i -> ("In pasture: " ++ show ss ++ ", containing: " ++ show mAnimal, Pasture i)) (_pastures b) [0 ..]
      stableOptions = zipWith (\(s, mAnimal) i -> ("In (unfenced) stable: " ++ show s ++ ", containing: " ++ show mAnimal, UnfencedStable i)) (_unfencedStables b) [0 ..] in
  houseOption : pastureOptions ++ stableOptions
