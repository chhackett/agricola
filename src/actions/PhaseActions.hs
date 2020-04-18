module Actions.PhaseActions where

import Types.ActionTypes
import Types.GameState

type ActionCardStagesMap = [(RoundCardType, Stage)]

roundCardToStage :: ActionCardStagesMap
roundCardToStage =
    [ (SowAndOrBakeBread,         0),
      (MajorOrMinorImprovement,   0),
      (Fences,                    0),
      (TakeSheep,                 0),
      (AfterRenovationAlsoImprovement,    1),
      (AfterFamilyGrowthAlsoImprovement,  1),
      (TakeStone1,                1),
      (TakeBoar,                  2),
      (TakeVege,                  2),
      (TakeCattle,                3),
      (TakeStone2,                3),
      (FamilyGrowthWithoutRoom,   4),
      (PlowAndOrSow,              4),
      (AfterRenovationAlsoFences, 5)
    ]

drawRoundCard :: GameState -> GameState
drawRoundCard gs = undefined

