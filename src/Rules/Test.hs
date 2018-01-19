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
        build $ target (vanillaContext Stage2 compiler) RunTest [] []
