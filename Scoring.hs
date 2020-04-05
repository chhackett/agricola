module Scoring where

import PlayerData

calculateScore :: Player -> Int
calculateScore p =
  workers p * 3