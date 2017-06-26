module Settings.Flavours.Quickest (quickestFlavour) where

import Flavour
import Predicate
import Oracles.Config.Flag (platformSupportsSharedLibs)
import {-# SOURCE #-} Settings.Default

quickestFlavour :: Flavour
quickestFlavour = defaultFlavour
    { name        = "quickest"
    , args        = defaultBuilderArgs <> quickestArgs <> defaultPackageArgs
    , libraryWays = append [vanilla]
    , rtsWays     = quickestRtsWays }

quickestArgs :: Args
quickestArgs = sourceArgs $ SourceArgs
    { hsDefault  = append ["-O0", "-H32m"]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc      = mempty }

quickestRtsWays :: Ways
quickestRtsWays = mconcat
    [ append [vanilla]
    , buildHaddock defaultFlavour ? append [threaded] ]
