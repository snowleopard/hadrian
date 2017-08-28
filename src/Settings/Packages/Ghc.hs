module Settings.Packages.Ghc (ghcPackageArgs) where

import Context (buildPath)
import GHC
import Expression
import Oracles.Setting
import UserSettings (crossCompiling)

ghcPackageArgs :: Args
ghcPackageArgs = package ghc ? do
    stage <- getStage
    path  <- expr $ buildPath (vanillaContext stage compiler)
    mconcat [ builder Ghc      ? arg ("-I" ++ path)
            , builder GhcCabal ? ghcWithInterpreter ? notStage0 ? arg "--flags=ghci"
            , builder GhcCabal ? crossCompiling ? arg "-f-terminfo" ]
