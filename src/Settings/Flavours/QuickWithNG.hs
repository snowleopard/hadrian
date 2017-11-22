module Settings.Flavours.QuickWithNG (quickWithNGFlavour) where

import Expression
import Types.Flavour
import {-# SOURCE #-} Settings.Default

llvmngPackages :: [Package]
llvmngPackages = [ dataBitcode, dataBitcodeLlvm, dataBitcodeEdsl ]

dataBitcode         = hsLib  "data-bitcode"
dataBitcodeLlvm     = hsLib  "data-bitcode-llvm"
dataBitcodeEdsl     = hsLib  "data-bitcode-edsl"

llvmngWarningArgs :: Args
llvmngWarningArgs = builder Ghc ?
  mconcat [ package dataBitcode  ? pure [ "-Wno-name-shadowing"
                                      , "-Wno-unused-top-binds"
                                      , "-Wno-unused-matches"
                                      , "-Wno-orphans"
                                      , "-Wno-incomplete-patterns"
                                      , "-Wno-unused-do-bind"
                                      , "-Wno-unused-imports"
                                      , "-Wno-missing-methods"
                                      , "-Wno-type-defaults"
                                      ]
        , package dataBitcodeLlvm ? pure [ "-Wno-name-shadowing"
                                      , "-Wno-unused-top-binds"
                                      , "-Wno-unused-matches"
                                      , "-Wno-orphans"
                                      , "-Wno-incomplete-patterns"
                                      , "-Wno-unused-do-bind"
                                      , "-Wno-unused-imports"
                                      , "-Wno-missing-methods"
                                      , "-Wno-unused-local-binds"
                                      , "-Wno-overlapping-patterns"
                                      , "-Wno-type-defaults"
                                      ]
        , package dataBitcodeEdsl ? pure [ "-Wno-name-shadowing"
                                      , "-Wno-unused-top-binds"
                                      , "-Wno-unused-matches"
                                      , "-Wno-orphans"
                                      , "-Wno-incomplete-patterns"
                                      , "-Wno-unused-do-bind"
                                      , "-Wno-unused-imports"
                                      , "-Wno-missing-methods"
                                      , "-Wno-type-defaults"
                                      , "-Wno-unused-local-binds"
                                      , "-Wno-overlapping-patterns"
                                      , "-Wno-type-defaults"
                                      , "-Wno-missing-signatures"
                                      ]
        ]

-- Please update doc/flavours.md when changing this file.
quickWithNGFlavour :: Flavour
quickWithNGFlavour = defaultFlavour
    { name        = "quick-with-ng"
    , args        = defaultBuilderArgs <> quickWithNGArgs <> defaultPackageArgs <> llvmngWarningArgs
    , libraryWays = mconcat
                    [ pure [vanilla]
                    -- , notStage0 ? platformSupportsSharedLibs ? pure [dynamic]
                    ]
    , packages    = \stage -> packages defaultFlavour stage ++ llvmngPackages
    }

quickWithNGArgs :: Args
quickWithNGArgs = sourceArgs SourceArgs
    { hsDefault  = pure ["-O0", "-H64m"]
    , hsLibrary  = notStage0 ? arg "-O"
    , hsCompiler =    stage0 ? arg "-O"
    , hsGhc      =    stage0 ? arg "-O" }
