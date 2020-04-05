{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module BoundedEnum where

import System.Random

class (Bounded a, Enum a) => BoundedEnum a
instance (Bounded a, Enum a) => BoundedEnum a

instance {-# OVERLAPPABLE #-} BoundedEnum a => Random a where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)
