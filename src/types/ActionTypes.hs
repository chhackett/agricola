module Types.ActionTypes where

import Types.CardNames
import Types.ResourceTypes

data CardType = Major | Minor | Occupation | RoundCard | ActionCard | Begging
  deriving (Show, Read, Eq, Ord)

data PreReq =
  NoPreReqs |
  PRNumCardTypes CardType Int |
  PRCard CardName Int |
  PRFields FieldType Int |
  PRAnyAnimals Int |
  PRAnimal Animal |
  PREither PreReq PreReq |
  PRAll PreReqs |
  PRAny PreReqs
  deriving (Show, Read, Eq, Ord)

type PreReqs = [PreReq]

data Cost =
  Free |
  Cost Resources |
  Return CardName |
  EitherCost Cost Cost |
  AllCosts Costs
  deriving (Show, Read, Eq, Ord)

type Costs = [Cost]
