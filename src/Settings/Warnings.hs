module Settings.Warnings (warningArgs) where

import Expression

-- ref: mk/warnings.mk
-- | Warnings-related build arguments, mostly suppressing various warnings.
warningArgs :: Args
warningArgs = builder Ghc ? mconcat
    [ stage0 ? mconcat
      [ libraryPackage       ? pure [ "-fno-warn-deprecated-flags" ]
      , package terminfo     ? pure [ "-fno-warn-unused-imports" ]
      , package transformers ? pure [ "-fno-warn-unused-matches"
                                    , "-fno-warn-unused-imports" ] ]
    , notStage0 ? mconcat
      [ libraryPackage       ? pure [ "-Wno-deprecated-flags" ]
      , package base         ? pure [ "-Wno-trustworthy-safe" ]
      , package binary       ? pure [ "-Wno-deprecations" ]
      , package bytestring   ? pure [ "-Wno-inline-rule-shadowing" ]
      , package directory    ? pure [ "-Wno-unused-imports" ]
      , package ghcPrim      ? pure [ "-Wno-trustworthy-safe" ]
      , package haddock      ? pure [ "-Wno-unused-imports"
                                    , "-Wno-deprecations" ]
      , package haskeline    ? pure [ "-Wno-deprecations"
                                    , "-Wno-unused-imports"
                                    , "-Wno-redundant-constraints"
                                    , "-Wno-simplifiable-class-constraints" ]
      , package pretty       ? pure [ "-Wno-unused-imports" ]
      , package primitive    ? pure [ "-Wno-unused-imports"
                                    , "-Wno-deprecations" ]
      , package terminfo     ? pure [ "-Wno-unused-imports" ]
      , package transformers ? pure [ "-Wno-unused-matches"
                                    , "-Wno-unused-imports"
                                    , "-Wno-redundant-constraints"
                                    , "-Wno-orphans" ]
      , package win32        ? pure [ "-Wno-trustworthy-safe" ]
      , package xhtml        ? pure [ "-Wno-unused-imports"
                                    , "-Wno-tabs" ] ] ]
