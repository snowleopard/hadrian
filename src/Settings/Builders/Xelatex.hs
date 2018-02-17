module Settings.Builders.Xelatex (xelatexBuilderArgs) where

import Settings.Builders.Common
import Hadrian.Expression (getInput)
import Builder ()

xelatexBuilderArgs :: Args
xelatexBuilderArgs = builder Xelatex ? mconcat [ arg "-halt-on-error"
                                               , arg =<< getInput ]
