module Settings.Flavours.QuickCrossNCG (quickCrossNCGFlavour) where

import Expression
import Flavour
import GHC.Packages
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickCrossNCGFlavour :: Flavour
quickCrossNCGFlavour = defaultFlavour
    { name        = "quick-cross-ncg"
    , args        = defaultBuilderArgs <> quickCrossArgs <> defaultPackageArgs
    , integerLibrary = pure integerSimple
    , libraryWays = mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ] }

quickCrossArgs :: Args
quickCrossArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O" ]
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0" ] ] }
