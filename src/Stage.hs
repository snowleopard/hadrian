module Stage (Stage (..), stageString) where

import Stage.Type

-- | Prettyprint a 'Stage'.
stageString :: Stage -> String
stageString stage = "stage" ++ show (fromEnum stage)
