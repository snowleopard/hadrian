module Settings.Packages.GhcPkg (ghcPkgPackageArgs) where

import Expression
import Oracles.Flag (crossCompiling)
import GHC.Packages

ghcPkgPackageArgs :: Args
ghcPkgPackageArgs = package ghcPkg ? builder (GhcCabal Conf) ? crossCompiling ? arg "-f-terminfo"
