module Settings.Builders.Hsc2Hs (hsc2hsBuilderArgs) where

import Control.Monad.Trans.Class
import Control.Monad.Extra

import Base
import Expression
import Oracles
import Predicates (builder, stage0, notStage0)
import Settings
import Settings.Builders.Common

templateHsc :: FilePath
templateHsc = "inplace/lib/template-hsc.h"

hsc2hsBuilderArgs :: Args
hsc2hsBuilderArgs = builder Hsc2Hs ? do
    stage   <- getStage
    ccPath  <- lift . builderPath $ Gcc stage
    gmpDir  <- getSetting GmpIncludeDir
    cFlags  <- getCFlags
    lFlags  <- getLFlags
    top     <- getTopDirectory
    hArch   <- getSetting HostArch
    hOs     <- getSetting HostOs
    tArch   <- getSetting TargetArch
    tOs     <- getSetting TargetOs
    version <- if stage == Stage0
               then lift $ ghcCanonVersion
               else getSetting ProjectVersionInt
    lift $ need [templateHsc]
    mconcat [ arg $ "--cc=" ++ ccPath
            , arg $ "--ld=" ++ ccPath
            , notM windowsHost ? arg "--cross-safe"
            , append . map ("-I"       ++) $ words gmpDir
            , append $ map ("--cflag=" ++) cFlags
            , append $ map ("--lflag=" ++) lFlags
            , notStage0 ? crossCompiling ? arg "--cross-compile"
            , stage0    ? arg ("--cflag=-D" ++ hArch ++ "_HOST_ARCH=1")
            , stage0    ? arg ("--cflag=-D" ++ hOs   ++ "_HOST_OS=1"  )
            , notStage0 ? arg ("--cflag=-D" ++ tArch ++ "_HOST_ARCH=1")
            , notStage0 ? arg ("--cflag=-D" ++ tOs   ++ "_HOST_OS=1"  )
            , arg ("--cflag=-D__GLASGOW_HASKELL__=" ++ version)
            , arg $ "--template=" ++ top -/- templateHsc
            , arg $ "-I" ++ top -/- "inplace/lib/include/"
            , arg =<< getInput
            , arg "-o", arg =<< getOutput ]

getCFlags :: Expr [String]
getCFlags = fromDiffExpr $ do
    path      <- getTargetPath
    cppArgs   <- pdCppArgs <$> getPkgData
    depCcArgs <- pdDepCcArgs <$> getPkgData
    mconcat [ cArgs
            , argStagedSettingList ConfCcArgs
            , remove ["-O"]
            , argStagedSettingList ConfCppArgs
            , cIncludeArgs
            , append cppArgs
            , append depCcArgs
            , cWarnings
            , arg "-include", arg $ path -/- "build/autogen/cabal_macros.h" ]

getLFlags :: Expr [String]
getLFlags = fromDiffExpr $ do
    pkgLdArgs <- pdLdArgs <$> getPkgData
    libDirs   <- pdDepLibDirs <$> getPkgData
    extraLibs <- pdDepExtraLibs <$> getPkgData
    depLdArgs <- pdDepLdArgs <$> getPkgData
    mconcat [ argStagedSettingList ConfGccLinkerArgs
            , ldArgs
            , append pkgLdArgs
            , append $ [ "-L" ++ unifyPath dir | dir <- libDirs ]
            , append $ [ "-l" ++ unifyPath dir | dir <- extraLibs ]
            , append depLdArgs ]
