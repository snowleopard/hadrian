module Rules.Data (buildPackageData) where

import qualified System.Directory as IO

import Base
import Expression
import GHC
import Oracles
import Predicates (registerPackage)
import Rules.Actions
import Rules.Generate
import Rules.Libffi
import Rules.Resources
import Settings
import Settings.Builders.Common
import Settings.Packages.Rts

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Resources -> PartialTarget -> Rules ()
buildPackageData rs target @ (PartialTarget stage pkg) = do
    let cabalFile = pkgCabalFile pkg
        configure = pkgPath pkg -/- "configure"

    [oldPath -/- "package-data.mk"] &%> \_ -> do
        -- The first thing we do with any package is make sure all generated
        -- dependencies are in place before proceeding.
        orderOnly $ generatedDependencies stage pkg

        -- GhcCabal may run the configure script, so we depend on it
        -- We don't know who built the configure script from configure.ac
        whenM (doesFileExist $ configure <.> "ac") $ need [configure]

        -- We configure packages in the order of their dependencies
        deps <- packageDeps pkg
        pkgs <- interpretPartial target getPackages
        let depPkgs = matchPackageNames (sort pkgs) deps

        need [cabalFile]
        buildWithResources [(resGhcCabal rs, 1)] $
            fullTarget target GhcCabal [cabalFile] [inTreeMk]

        -- TODO: get rid of this, see #113
        autogenFiles <- getDirectoryFiles oldPath ["build/autogen/*"]
        createDirectory $ targetPath stage pkg -/- "build/autogen"
        forM_ autogenFiles $ \file -> do
            copyFile (oldPath -/- file) (targetPath stage pkg -/- file)

        -- ghc-pkg produces inplace-pkg-config when run on packages with
        -- library components only
        when (isLibrary pkg) .
            whenM (interpretPartial target registerPackage) $ do

                -- Post-process inplace-pkg-config. TODO: remove, see #113, #148
                let fixPkgConf = unlines
                               . map (replace oldPath (targetPath stage pkg)
                               . replace (replaceSeparators '\\' $ oldPath)
                                         (targetPath stage pkg) )
                               . lines

                fixFile (oldPath -/- "inplace-pkg-config") fixPkgConf

                buildWithResources [(resGhcPkg rs, 1)] $
                    fullTarget target (GhcPkg stage) [cabalFile] []


    -- TODO: PROGNAME was $(CrossCompilePrefix)hp2ps
    priority 2.0 $ do
        when (pkg == unlit) $ dataFile %> \mk -> do
            let prefix   = fixKey (targetPath stage pkg) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = unlit"
                    , "C_SRCS = unlit.c"
                    , "SYNOPSIS = Literate script filter." ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (pkg == touchy) $ dataFile %> \mk -> do
            let prefix   = fixKey (targetPath stage pkg) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = touchy"
                    , "C_SRCS = touchy.c" ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (pkg == rts && stage == Stage1) $ do
            dataFile %> \mk -> do
                orderOnly $ generatedDependencies stage pkg
                windows <- windowsHost
                let prefix = fixKey (targetPath stage pkg) ++ "_"
                    dirs   = [ ".", "hooks", "sm", "eventlog" ]
                          ++ [ "posix" | not windows ]
                          ++ [ "win32" |     windows ]
                -- TODO: rts/dist/build/sm/Evac_thr.c, rts/dist/build/sm/Scav_thr.c
                -- TODO: adding cmm/S sources to C_SRCS is a hack; rethink after #18
                cSrcs    <- getDirectoryFiles (pkgPath pkg) (map (-/- "*.c") dirs)
                cmmSrcs  <- getDirectoryFiles (pkgPath pkg) ["*.cmm"]
                buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
                buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
                let sSrcs = [ "AdjustorAsm.S" | buildAdjustor   ]
                         ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]
                    extraSrcs = [ rtsBuildPath -/- "AutoApply.cmm" ]
                includes <- interpretPartial target $ fromDiffExpr includesArgs
                let contents = unlines $ map (prefix++)
                        [ "C_SRCS = "
                          ++ unwords (cSrcs ++ cmmSrcs ++ sSrcs ++ extraSrcs)
                        , "CC_OPTS = "  ++ unwords includes
                        , "COMPONENT_ID = " ++ "rts" ]
                writeFileChanged mk contents
                putSuccess $ "| Successfully generated '" ++ mk ++ "'."

                need [rtsConf]
                buildWithResources [(resGhcPkg rs, 1)] $
                    fullTarget target (GhcPkg stage) [rtsConf] []

            rtsConf %> \_ -> do
                orderOnly $ generatedDependencies stage pkg
                need [ rtsConfIn ]
                build $ fullTarget target HsCpp [rtsConfIn] [rtsConf]

                let fixRtsConf = unlines
                               . map
                               ( replace "\"\"" ""
                               . replace "rts/dist/build" rtsBuildPath )
                               . filter (not . null)
                               . lines

                fixFile rtsConf fixRtsConf
