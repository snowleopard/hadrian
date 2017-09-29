module Settings.Flavours.Validate (validateFlavour) where

import Flavour
import Expression
import Oracles.Flag
import {-# SOURCE #-} Settings.Default

validateFlavour :: Flavour
validateFlavour = defaultFlavour
    { name        = "validate"
    , args        = defaultBuilderArgs <> validateArgs <> defaultPackageArgs }

validateArgs :: Args
validateArgs = sourceArgs $ SourceArgs
    { hsDefault  = mconcat
                   [ pure ["-O0", "-H64m"]
                   , stage0 ? arg "-fllvm-fill-undef-with-garbage" ]
    , hsLibrary  = notStage0 ? pure [ "-O", "-dcore-lint", "-dno-debug-output" ]
    , hsGhc      = mconcat
                   [ stage0 ? pure [ "-O", "-DDEBUG" ]
                   , stage1 ? pure [ "-O", "-dcore-lint", "-dno-debug-output" ] ] }
