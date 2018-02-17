module Settings.Builders.Alex (alexBuilderArgs) where

import Settings.Builders.Common
import Builder ()

alexBuilderArgs :: Args
alexBuilderArgs = builder Alex ? mconcat [ arg "-g"
                                         , arg =<< getInput
                                         , arg "-o", arg =<< getOutput ]
