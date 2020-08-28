module ResourceHelperFuncs where

import Data.Maybe

import Types.BasicGameTypes
import Types.ResourceTypes

--------------------------------------------
------ Helper functions for resources ------
--------------------------------------------

getAnimalQuantity :: Board -> AnimalType -> Int
getAnimalQuantity b at =
  let i = if isJust $ _houseAnimal b then 1 else 0
      j = getAll at $ _pastures b
      k = getAll at $ _stables b in
  i + j + k

getAll :: Eq b => b -> [(a, Maybe (b, Int))] -> Int
getAll t = foldl (getSum t) 0

getSum :: Eq b => b -> Int -> (a, Maybe (b, Int)) -> Int
getSum t n (_, Just (t', n')) = if t == t' then n + n' else n
getSum _ n (_, Nothing) = n