module Rules.Test (testRules) where

import Base
import Expression
import Flavour
import Oracles.Flag
import Oracles.Setting
import Settings
import Target
import Utilities

import System.Environment

-- TODO: clean up after testing
testRules :: Rules ()
testRules = do

    root <- buildRootRules

    root -/- timeoutPyPath ~> do
        copyFile "testsuite/timeout/timeout.py" (root -/- timeoutPyPath)

    -- TODO windows is still not supported.
    --
    -- See: https://github.com/ghc/ghc/blob/master/testsuite/timeout/Makefile#L23
    root -/- timeoutProgPath ~> do
        python <- builderPath Python
        need [root -/- timeoutPyPath]
        let script = unlines
                [ "#!/usr/bin/env sh"
                , "exec " ++ python ++ " $0.py \"$@\""
                ]
        liftIO $ do
            writeFile (root -/- timeoutProgPath) script
            cmd "chmod" [ "+x", root -/- timeoutProgPath ]

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
        -- Prepare the timeout program.
        need [ root -/- timeoutProgPath ]

        makePath       <- builderPath $ Make ""
        top            <- topDirectory
        ghcPath        <- (top -/-) <$> builderPath (Ghc CompileHs Stage2)
        unregisterised <- flag GhcUnregisterised

        -- Prepare extra flags to send to the Haskell compiler.

        let ifMinGhcVer ver opt = do v <- ghcCanonVersion
                                     if ver <= v then pure opt
                                                 else pure ""

        -- Read extra argument for test from command line, like `-fvectorize`.
        opts <- fromMaybe "" <$> (liftIO $ lookupEnv "EXTRA_HC_OPTS")

        -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L28
        let ghcExtraFlags = opts ++ if unregisterised
                                       then " -optc-fno-builtin"
                                       else ""

        -- Take flags to send to the Haskell compiler from test.mk.
        -- See: https://github.com/ghc/ghc/blob/master/testsuite/mk/test.mk#L37
        ghcFlags <- unwords <$> sequence
            [ pure " -dcore-lint -dcmm-lint -no-user-package-db -rtsopts"
            , pure ghcExtraFlags
            , ifMinGhcVer "711" "-fno-warn-missed-specialisations"
            , ifMinGhcVer "711" "-fshow-warning-groups"
            , ifMinGhcVer "801" "-fdiagnostics-color=never"
            , ifMinGhcVer "801" "-fno-diagnostics-show-caret"
            , pure "-dno-debug-output"
            ]

        -- Set environment variables for test's Makefile.
        liftIO $ do
            setEnv "MAKE" makePath
            setEnv "TEST_HC" ghcPath
            setEnv "TEST_HC_OPTS" ghcFlags

        -- Execute the test target.
        build $ target (vanillaContext Stage2 compiler) RunTest [] []

timeoutPyPath :: FilePath
timeoutPyPath = "test" -/- "bin" -/- "timeout.py"

timeoutProgPath :: FilePath
timeoutProgPath = "test" -/- "bin" -/- "timeout"  -- TODO: `.exe` suffix for windows.
