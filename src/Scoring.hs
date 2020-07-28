module Scoring where

import Types.BasicGameTypes

calculateScore :: Player -> Int
calculateScore p =
  _workers p * 3