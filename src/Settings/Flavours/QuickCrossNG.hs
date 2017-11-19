module Settings.Flavours.QuickCrossNG (quickCrossNGFlavour) where

import Expression
import Types.Flavour
import {-# SOURCE #-} Settings.Default
import GHC.Packages

-- Please update doc/flavours.md when changing this file.
quickCrossNGFlavour :: Flavour
quickCrossNGFlavour = defaultFlavour
    { name        = "quick-cross-ng"
    , args        = defaultBuilderArgs <> quickCrossNGArgs <> defaultPackageArgs
    , integerLibrary = pure integerSimple
    , libraryWays = pure [vanilla]
    }

quickCrossNGArgs :: Args
quickCrossNGArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O", arg "-fllvmng" ]
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0", arg "-fllvmng" ] ] }
