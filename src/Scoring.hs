module Scoring where

import Types.PlayerData

calculateScore :: Player -> Int
calculateScore p =
  workers p * 3