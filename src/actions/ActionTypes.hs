module Actions.ActionTypes where

type Action f c m = (ActionType, ActionCost c, f, [ActionModifier m])

data ActionType =
  PhaseAction |
  UserAction |
  CardAction |
  ScoringAction |
  OtherAction

data ActionCost c =
  MaterialCost c |
  ResourceCost c |
  OtherCost c

data ActionModifier m =
  CostModifier m |
  BonusModifier m |
  OtherModifier m
