module Settings.Flavours.QuickCrossNG (quickCrossNGFlavour) where

import Expression
import Flavour
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

-- Please update doc/flavours.md when changing this file.
quickCrossNGFlavour :: Flavour
quickCrossNGFlavour = defaultFlavour
    { name        = "quick-cross-ng"
    , args        = defaultBuilderArgs <> quickCrossNGArgs <> defaultPackageArgs
    , libraryWays = mconcat
                    [ pure [vanilla]
                    , notStage0 ? platformSupportsSharedLibs ? pure [dynamic] ] }

quickCrossNGArgs :: Args
quickCrossNGArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O", arg "-fllvmng" ]
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0", arg "-fllvmng" ] ] }
