module Settings.Builders.Ld (ldBuilderArgs) where

import Settings.Builders.Common
import Builder ()

ldBuilderArgs :: Args
ldBuilderArgs = builder Ld ? mconcat [ getStagedSettingList ConfLdLinkerArgs
                                     , arg "-r"
                                     , arg "-o", arg =<< getOutput
                                     , getInputs ]
