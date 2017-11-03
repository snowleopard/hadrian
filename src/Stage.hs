module Stage (Stage (..), stageString) where

import Types.Stage

-- | Prettyprint a 'Stage'.
stageString :: Stage -> String
stageString stage = "stage" ++ show (fromEnum stage)
