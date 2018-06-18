module Oracles.TestSettings (
    TestSetting (..), testSetting, testRTSSettings
    ) where

import Hadrian.Oracles.TextFile
import Base

testConfigFile :: Action FilePath
testConfigFile = buildRoot <&> (-/- "test/ghcconfig")

-- | Test settings obtained from 
data TestSetting = TestHostOS
                 | TestWORDSIZE
                 | TestTARGETPLATFORM
                 | TestTargetOS_CPP
                 | TestTargetARCH_CPP
                 | TestGhcStage
                 | TestGhcDebugged
                 | TestGhcWithNativeCodeGen
                 | TestGhcWithInterpreter
                 | TestGhcUnregisterised
                 | TestGhcWithSMP
                 | TestGhcDynamicByDefault
                 | TestGhcDynamic
                 | TestGhcProfiled
                 | TestAR
                 | TestCLANG
                 | TestLLC
                 | TestTEST_CC
                 | TestGhcPackageDbFlag
                 | TestMinGhcVersion711
                 | TestMinGhcVersion801
                 deriving (Show)

testSetting :: TestSetting -> Action String
testSetting key = do
    file <- testConfigFile
    lookupValueOrError file $ case key of
        TestHostOS                -> "HostOS"
        TestWORDSIZE              -> "WORDSIZE" 
        TestTARGETPLATFORM        -> "TARGETPLATFORM"
        TestTargetOS_CPP          -> "TargetOS_CPP"
        TestTargetARCH_CPP        -> "TargetARCH_CPP"
        TestGhcStage              -> "GhcStage" 
        TestGhcDebugged           -> "GhcDebugged"
        TestGhcWithNativeCodeGen  -> "GhcWithNativeCodeGen"
        TestGhcWithInterpreter    -> "GhcWithInterpreter"
        TestGhcUnregisterised     -> "GhcUnregisterised"
        TestGhcWithSMP            -> "GhcWithSMP"
        TestGhcDynamicByDefault   -> "GhcDynamicByDefault"
        TestGhcDynamic            -> "GhcDynamic"
        TestGhcProfiled           -> "GhcProfiled"
        TestAR                    -> "AR"
        TestCLANG                 -> "CLANG"
        TestLLC                   -> "LLC"
        TestTEST_CC               -> "TEST_CC"
        TestGhcPackageDbFlag      -> "GhcPackageDbFlag"
        TestMinGhcVersion711      -> "MinGhcVersion711"
        TestMinGhcVersion801      -> "MinGhcVersion801"
    

testRTSSettings :: Action [String]
testRTSSettings = do 
    file <- testConfigFile
    fmap words $ lookupValueOrError file "GhcRTSWays"

