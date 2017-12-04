module Settings.Builders.Cc (ccBuilderArgs) where

import Settings.Builders.Common
import Types.ConfiguredCabal as ConfCabal
import Builder ()

ccBuilderArgs :: Args
ccBuilderArgs = do
    way <- getWay
    iconvIncludeDir <- getSetting IconvIncludeDir
    gmpIncludeDir   <- getSetting GmpIncludeDir
    ffiIncludeDir   <- getSetting FfiIncludeDir
    builder Cc ? mconcat
        [ getConfiguredCabalData ConfCabal.ccOpts
        , getStagedSettingList ConfCcArgs
        , cIncludeArgs

        , builder (Cc CompileC) ? mconcat
            [ pure ["-Wall", "-Werror"]
            , Dynamic `wayUnit` way ? pure [ "-fPIC", "-DDYNAMIC" ]
            , arg "-c", arg =<< getInput
            , arg "-o", arg =<< getOutput ]

        , builder (Cc FindCDependencies) ? do
            output <- getOutput
            mconcat [ arg "-E"
                    , arg "-MM", arg "-MG"
                    , arg "-MF", arg output
                    , arg "-MT", arg $ dropExtension output -<.> "o"
                    , pure . map ("-I"++) . filter (/= "") $ [iconvIncludeDir, gmpIncludeDir]
                    , flag UseSystemFfi ? arg ("-I" ++ ffiIncludeDir)
                    , arg "-x", arg "c"
                    , arg =<< getInput ] ]
