module Settings.Builders.Hsc2Hs (hsc2hsBuilderArgs) where

import Settings.Builders.Common
import Types.ConfiguredCabal as ConfCabal

hsc2hsBuilderArgs :: Args
hsc2hsBuilderArgs = builder Hsc2Hs ? do
    stage   <- getStage
    ccPath  <- getBuilderPath $ Cc CompileC stage
    gmpDir  <- getSetting GmpIncludeDir
    top     <- expr topDirectory
    hArch   <- getSetting HostArch
    hOs     <- getSetting HostOs
    tArch   <- getSetting TargetArch
    tOs     <- getSetting TargetOs
    version <- if stage == Stage0
               then expr ghcCanonVersion
               else getSetting ProjectVersionInt
    tmpl    <- (top -/-) <$> expr (templateHscPath Stage0)
    mconcat [ arg $ "--cc=" ++ ccPath
            , arg $ "--ld=" ++ ccPath
            , notM windowsHost ? arg "--cross-safe"
            , pure $ map ("-I" ++) (words gmpDir)
            , map ("--cflag=" ++) <$> getCFlags
            , map ("--lflag=" ++) <$> getLFlags
            , notStage0 ? crossCompiling ? arg "--cross-compile"
            , stage0    ? arg ("--cflag=-D" ++ hArch ++ "_HOST_ARCH=1")
            , stage0    ? arg ("--cflag=-D" ++ hOs   ++ "_HOST_OS=1"  )
            , notStage0 ? arg ("--cflag=-D" ++ tArch ++ "_HOST_ARCH=1")
            , notStage0 ? arg ("--cflag=-D" ++ tOs   ++ "_HOST_OS=1"  )
            , arg $ "--cflag=-D__GLASGOW_HASKELL__=" ++ version
            , arg $ "--template=" ++ tmpl
            , arg $ "-I" ++ top -/- "inplace/lib/include/"
            , arg =<< getInput
            , arg "-o", arg =<< getOutput ]

getCFlags :: Expr [String]
getCFlags = do
    context <- getContext
    autogen <- expr $ autogenPath context
    mconcat [ remove ["-O"] (cArgs <> getStagedSettingList ConfCcArgs)
            , getStagedSettingList ConfCppArgs
            , cIncludeArgs
            , getConfiguredCabalData ConfCabal.cppOpts
            , getConfiguredCabalData ConfCabal.depCcOpts
            , cWarnings
            , arg "-include", arg $ autogen -/- "cabal_macros.h" ]

getLFlags :: Expr [String]
getLFlags = do
    mconcat [ getStagedSettingList ConfGccLinkerArgs
            , ldArgs
            , getConfiguredCabalData ConfCabal.ldOpts
            , getConfiguredCabalData ConfCabal.depLdOpts ]
