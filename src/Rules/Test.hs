module Rules.Test (testRules) where

import Base
import Expression
import Flavour
import Oracles.Flag
import Oracles.Setting
import Settings
import Target
import Utilities

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do
    "validate" ~> do
        need inplaceLibCopyTargets
        needBuilder $ Ghc CompileHs Stage2
        needBuilder $ GhcPkg Update Stage1
        needBuilder Hpc
        -- TODO: Figure out why @needBuilder Hsc2Hs@ doesn't work.
        -- TODO: Eliminate explicit filepaths.
        -- See https://github.com/snowleopard/hadrian/issues/376.
        need ["inplace/bin/hp2ps", "inplace/bin/hsc2hs"]
        build $ target (vanillaContext Stage2 compiler) (Make "testsuite/tests") [] []

    "test" ~> do
        pkgs     <- stagePackages Stage1
        tests    <- filterM doesDirectoryExist $ concat
                    [ [ pkgPath pkg -/- "tests", pkgPath pkg -/- "tests-ghc" ]
                    | pkg <- pkgs, isLibrary pkg, pkg /= rts, pkg /= libffi ]

        debugged          <- ghcDebugged <$> flavour

        withNativeCodeGen <- ghcWithNativeCodeGen
        withInterpreter   <- ghcWithInterpreter
        unregisterised    <- flag GhcUnregisterised

        withSMP           <- ghcWithSMP

        windows  <- windowsHost
        darwin   <- osxHost

        threads  <- shakeThreads <$> getShakeOptions
        verbose  <- shakeVerbosity <$> getShakeOptions

        testArgs <- getTestArgs
        let testOnlyArg = case testOnly testArgs of
                            Just cases -> map ("--only=" ++) (words cases)
                            Nothing -> []
            skipPerfArg = if testSkipPerf testArgs
                            then Just "--skip-perf-tests"
                            else Nothing
            summaryArg = case testSummary testArgs of
                            Just filepath -> Just $ "--summary-file" ++ quote filepath
                            Nothing -> Just $ "--summary-file=testsuite_summary.txt"
            junitArg = case testJUnit testArgs of
                            Just filepath -> Just $ "--junit " ++ quote filepath
                            Nothing -> Nothing
            configArgs = map ("-e " ++) (testConfigs testArgs)

            extraArgs = catMaybes [skipPerfArg, summaryArg, junitArg] ++ configArgs

        top      <- topDirectory
        compiler <- builderPath $ Ghc CompileHs Stage2
        ghcPkg   <- builderPath $ GhcPkg Update Stage1
        haddock  <- builderPath $ Haddock BuildPackage
        hp2ps    <- builderPath $ Hp2Ps
        hpc      <- builderPath $ Hpc

        let minGhcVersion v = (v <=) <$> ghcCanonVersion

        let ifMinGhcVer v opt = do ver <- ghcCanonVersion
                                   if v <= v then pure opt
                                             else pure ""

        -- Prepare extra flags to send to the Haskell compiler.
        -- TODO: read extra argument for test from command line, like `-fvectorize`.
        let ghcExtraFlags = if unregisterised               -- Value EXTRA_HC_OPTS should be handled.
                               then "-optc-fno-builtin"
                               else ""

        -- Take flags to send to the Haskell compiler from test.mk.
        -- See: https://github.com/ghc/ghc/blob/cf2c029ccdb967441c85ffb66073974fbdb20c20/testsuite/mk/test.mk#L37-L55
        ghcFlags <- sequence
            [ pure "-dcore-lint -dcmm-lint -no-user-package-db -rtsopts"
            , pure ghcExtraFlags
            , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
            , ifMinGhcVer "711" "-fshow-warning-groups"
            , ifMinGhcVer "801" "-fdiagnostics-color=never"
            , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
            , pure "-dno-debug-output"
            ]

        -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L291
        let timeout_prog = "testsuite/timeout/install-inplace/bin/timeout"

        quietly . cmd "python3" $
            [ "testsuite/driver/runtests.py" ] ++
            ["--rootdir=" ++ ("testsuite" -/- "tests")] ++
            map ("--rootdir=" ++) tests ++
            extraArgs ++
            [ "-e", "windows=" ++ show windows
            , "-e", "darwin=" ++ show darwin
            , "-e", "config.speed=2"                        -- Use default value in GHC's test.mk
            , "-e", "config.local=True"
            , "-e", "config.cleanup=False"                  -- Don't clean up.
            , "-e", "config.compiler_debugged=" ++ quote (yesNo debugged)
            , "-e", "ghc_debugged=" ++ quote (yesNo debugged)
            , "-e", "ghc_with_native_codegen=" ++ zeroOne withNativeCodeGen

            , "-e", "config.have_interp=" ++ show withInterpreter
            , "-e", "config.unregisterised=" ++ show unregisterised

            , "-e", "ghc_compiler_always_flags=" ++ quote (unwords ghcFlags)
            , "-e", "ghc_with_vanilla=1"                    -- TODO: do we always build vanilla?
            , "-e", "ghc_with_dynamic=0"                    -- TODO: support dynamic
            , "-e", "ghc_with_profiling=0"                  -- TODO: support profiling

            , "-e", "config.have_vanilla=1"                 -- TODO: support other build context
            , "-e", "config.have_dynamic=0"                 -- TODO: support dynamic
            , "-e", "config.have_profiling=0"               -- TODO: support profiling
            , "-e", "ghc_with_smp=" ++ zeroOne withSMP
            , "-e", "ghc_with_llvm=0"                       -- TODO: support LLVM

            , "-e", "ghc_with_threaded_rts=0"               -- TODO: support threaded
            , "-e", "ghc_with_dynamic_rts=0"                -- TODO: support dynamic
            , "-e", "config.ghc_dynamic_by_default=False"   -- TODO: support dynamic
            , "-e", "config.ghc_dynamic=False"              -- TODO: support dynamic

            , "-e", "config.in_tree_compiler=True"          -- Use default value, see https://github.com/ghc/ghc/blob/master/testsuite/mk/boilerplate.mk

            , "--config-file=testsuite/config/ghc"
            , "--config", "compiler="     ++ show (top -/- compiler)
            , "--config", "ghc_pkg="      ++ show (top -/- ghcPkg)
            , "--config", "haddock="      ++ show (top -/- haddock)
            , "--config", "hp2ps="        ++ show (top -/- hp2ps)
            , "--config", "hpc="          ++ show (top -/- hpc)
            , "--config", "gs=gs"                           -- Use the default value as in test.mk
            , "--config", "timeout_prog=" ++ show (top -/- timeout_prog)
            , "--threads=" ++ show threads
            , "--verbose=" ++ show (fromEnum verbose)
            ] ++
            testOnlyArg
