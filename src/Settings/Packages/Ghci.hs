module Settings.Packages.Ghci (ghciPackageArgs) where

import Expression
import GHC.Packages

ghciPackageArgs :: Args
ghciPackageArgs = package ghci ? notStage0 ? builder CabalFlags ? arg "ghci"
