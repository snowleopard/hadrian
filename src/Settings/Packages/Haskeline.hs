module Settings.Packages.Haskeline (haskelinePackageArgs) where

import Expression
import Oracles.Flag (crossCompiling)

haskelinePackageArgs :: Args
haskelinePackageArgs =
    package haskeline ? builder CabalFlags ? crossCompiling ? arg "-terminfo"
