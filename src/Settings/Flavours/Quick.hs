module Settings.Flavours.Quick (quickFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Common

-- Please update doc/flavours.md when changing this file.
quickFlavour :: Flavour
quickFlavour = defaultFlavour
    { name        = "quick"
    , args        = defaultBuilderArgs <> quickArgs <> defaultPackageArgs
    , libraryWays = mconcat
                    [ vanillaAlways
                    , notStage0 ? dynamicWhenPossible ]
    , rtsWays     = quickRtsWays }

quickArgs :: Args
quickArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        , naturalInBaseFixArgs
        ]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler =    stage0 ? arg "-O"
    , hsGhc      =    stage0 ? arg "-O" }
