module Settings.Packages (packageArgs) where

import Expression
import Settings
import Types.Flavour
import Oracles.Setting
import Oracles.Flag
import GHC.Packages
import Rules.Gmp

packageArgs :: Args
packageArgs = do
  intLibPkg <- getIntegerPackage
  integerLibraryName <- pkgName <$> getIntegerPackage

  stage   <- getStage
  rtsWays <- getRtsWays
  path    <- getBuildPath

  compilerBuildPath <- expr $ buildPath (vanillaContext stage compiler)

  gmpBuildPath <- expr gmpBuildPath
  let includeGmp = "-I" ++ gmpBuildPath -/- "include"

  version <- getSetting ProjectVersion

  mconcat
    [ package base
      ? mconcat [ builder CabalFlags ? arg ('+':integerLibraryName)
                  -- This fixes the 'unknown symbol stat' issue.
                  -- See: https://github.com/snowleopard/hadrian/issues/259.
                , builder (Ghc CompileCWithGhc) ? arg "-optc-O2" ]
    , package bytestring
      ? builder CabalFlags ? intLibPkg == integerSimple ? arg "integer-simple"
    , package text
      ? builder CabalFlags ? intLibPkg == integerSimple ? arg "integer-simple"
    , package cabal
      -- Cabal is a rather large library and quite slow to compile. Moreover, we
      -- build it for stage0 only so we can link ghc-pkg against it, so there is
      -- little reason to spend the effort to optimize it.
      ? stage0 ? builder Ghc ? arg "-O0"
    , package compiler
      ? mconcat [ builder Alex ? arg "--latin1"
                , builder (Ghc CompileHs) ? mconcat
                  [ inputs ["//GHC.hs", "//GhcMake.hs"] ? arg "-fprof-auto"
                  , input "//Parser.hs" ?
                    pure ["-O0", "-fno-ignore-interface-pragmas", "-fcmm-sink" ] ]
                , builder (GhcCabal Conf) ? mconcat
                  [ arg $ "--ghc-option=-DSTAGE=" ++ show (fromEnum stage + 1)
                  , arg "--disable-library-for-ghci"
                  , anyTargetOs ["openbsd"] ? arg "--ld-options=-E"
                  , flag GhcUnregisterised ? arg "--ghc-option=-DNO_REGS"
                  , notM ghcWithSMP ? arg "--ghc-option=-DNOSMP"
                  , notM ghcWithSMP ? arg "--ghc-option=-optc-DNOSMP"
                  , (threaded `elem` rtsWays) ?
                    notStage0 ? arg "--ghc-option=-optc-DTHREADED_RTS"
                  , ghcWithInterpreter ?
                    ghcEnableTablesNextToCode ?
                    notM (flag GhcUnregisterised) ?
                    notStage0 ? arg "--ghc-option=-DGHCI_TABLES_NEXT_TO_CODE"
                  , ghcWithInterpreter ?
                    ghciWithDebugger <$> flavour ?
                    notStage0 ? arg "--ghc-option=-DDEBUGGER"
                  , ghcProfiled <$> flavour ?
                    notStage0 ? arg "--ghc-pkg-option=--force" ]
                , builder CabalFlags ? mconcat
                  [ ghcWithNativeCodeGen ? arg "ncg"
                  , ghcWithInterpreter ?
                    notStage0 ? arg "ghci"
                  , crossCompiling ? arg "-terminfo"
                  ]
                , builder (Haddock BuildPackage) ? arg ("--optghc=-I" ++ path) ]
    , package ghc
      ? mconcat [ builder Ghc        ? arg ("-I" ++ compilerBuildPath)
                , builder CabalFlags ? ghcWithInterpreter ? notStage0 ? arg "ghci"
                , builder CabalFlags ? crossCompiling ? arg "-terminfo" ]
    , package ghcPkg
      ? builder CabalFlags ? crossCompiling ? arg "-terminfo"
    , package ghcPrim
      ? mconcat [ builder CabalFlags ? arg "include-ghc-prim"
                , builder (Cc CompileC)     ?
                  (not <$> flag GccIsClang) ?
                  input "//cbits/atomic.c"  ? arg "-Wno-sync-nand" ]
    , package ghci ? notStage0 ? builder CabalFlags ? arg "ghci"
    , package haddock ? builder CabalFlags ? arg "in-ghc-tree"
    , package haskeline ? builder CabalFlags ? crossCompiling ? arg "-terminfo"
    , package hsc2hs ? builder CabalFlags ? arg "in-ghc-tree"
    , package integerGmp
      ? mconcat [ builder Cc ? arg includeGmp
                , builder (GhcCabal Conf) ? mconcat
                  [ -- (null gmpIncludeDir && null gmpLibDir) ?
                    -- XXX: this should respect some settings flag "InTreeGmp".
                    --      depending on include and lib dir, is bound to fail
                    --      these are only set if ./configure was explicilty
                    --      called with gmp include and lib dirs.  Their absense
                    --      as such does not imply in-tree-gmp
                    -- arg "--configure-option=--with-intree-gmp"
                    arg ("--configure-option=CFLAGS=" ++ includeGmp)
                  , arg ("--gcc-options="             ++ includeGmp) ] ]
    , package runGhc
      ? builder Ghc ? input "//Main.hs" ? pure ["-cpp", "-DVERSION=" ++ show version]
    ]
