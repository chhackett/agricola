module Scoring where

import Types.PlayerData

calculateScore :: Player -> Int
calculateScore p =
  _workers p * 3