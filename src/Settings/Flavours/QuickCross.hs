module Settings.Flavours.QuickCross (quickCrossFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Common

-- Please update doc/flavours.md when changing this file.
quickCrossFlavour :: Flavour
quickCrossFlavour = defaultFlavour
    { name        = "quick-cross"
    , args        = defaultBuilderArgs <> quickCrossArgs <> defaultPackageArgs
    , libraryWays = mconcat
                    [ vanillaAlways
                    , notStage0 ? dynamicWhenPossible ]
    , rtsWays     = quickRtsWays }

quickCrossArgs :: Args
quickCrossArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        , naturalInBaseFixArgs
        ]
    , hsLibrary  = notStage0 ? mconcat [ arg "-O", arg "-fllvm" ]
    , hsCompiler = stage0 ? arg "-O"
    , hsGhc      = mconcat
                   [ stage0 ? arg "-O"
                   , stage1 ? mconcat [ arg "-O0", arg "-fllvm" ] ] }
