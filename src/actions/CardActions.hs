module Actions.CardActions where

import Types.CardDeclarations
import Types.BasicGameTypes
import Utils.ListUtils
import Utils.Selection

-------------------------------
------ Play an Occupation -----
-------------------------------

runPlayOccupation :: GameStateT ActionPrimitives
runPlayOccupation = return [PlayOccupation ClayMixer]