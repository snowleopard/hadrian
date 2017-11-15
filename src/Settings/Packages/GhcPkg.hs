module Settings.Packages.GhcPkg (ghcPkgPackageArgs) where

import Expression
import Oracles.Flag (crossCompiling)
import GHC.Packages

ghcPkgPackageArgs :: Args
ghcPkgPackageArgs = package ghcPkg ? builder CabalFlags ? crossCompiling ? arg "-terminfo"
