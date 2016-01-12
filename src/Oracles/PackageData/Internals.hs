module Oracles.PackageData.Internals where

import Base
import Package
import Stage
import GHC hiding (compiler)
import Settings.Paths

import Distribution.ModuleName as ModuleName
import Distribution.Package
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.Simple (defaultHookedPackageDesc)
import Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program
import Distribution.Simple.Compiler
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex

-- | Various information of interest scaped from Cabal's 'BuildInfo'.
data PackageData = PackageData { pdPackage        :: PackageIdentifier
                               , pdDependencies   :: [PackageIdentifier]
                               , pdTransitiveDeps :: [PackageIdentifier]
                               , pdDepCcArgs      :: [String]
                               , pdDepLdArgs      :: [String]
                               , pdDepExtraLibs   :: [String]
                               , pdDepIncludeDirs :: [FilePath]
                               , pdDepLibDirs     :: [FilePath]

                               , pdHcArgs         :: [String]
                               , pdCcArgs         :: [String]
                               , pdCppArgs        :: [String]
                               , pdLdArgs         :: [String]
                               , pdIncludeDirs    :: [FilePath]
                               , pdIncludes       :: [String]
                               , pdCSources       :: [FilePath]
                               , pdModules        :: [ModuleName]
                               , pdHiddenModules  :: [ModuleName]
                               , pdWithGHCiLib    :: Bool
                               }

getPackageData :: Stage -> Package.Package -> Action PackageData
getPackageData stage pkg
    | pkg == hp2ps = error "TODO"
    | otherwise    = do
        -- Largely stolen from utils/ghc-cabal/Main.hs
        let verbosity = silent
        let distdir = targetPath stage pkg
        need [pkgCabalFile pkg]
        lbi <- liftIO $ getPersistBuildConfig distdir
        let pd0 = localPkgDescr lbi
        hooked_bi <-
            if (buildType pd0 == Just Configure) || (buildType pd0 == Just Custom)
            then do
                maybe_infoFile <- liftIO defaultHookedPackageDesc
                case maybe_infoFile of
                    Nothing       -> return emptyHookedBuildInfo
                    Just infoFile -> liftIO $ readHookedBuildInfo verbosity infoFile
            else
                return emptyHookedBuildInfo

        let pd = updatePackageDescription hooked_bi pd0

        let libBiModules lib = (libBuildInfo lib, libModules lib)
            exeBiModules exe = (buildInfo exe, ModuleName.main : exeModules exe)
            biModuless = (maybeToList $ fmap libBiModules $ PD.library pd)
                      ++ (map exeBiModules $ executables pd)
            buildableBiModuless = filter isBuildable biModuless
                where isBuildable (bi', _) = buildable bi'
            (bi, modules) = case buildableBiModuless of
                            [] -> error "No buildable component found"
                            [biModules] -> biModules
                            _ -> error ("XXX ghc-cabal can't handle " ++
                                        "more than one buildinfo yet")
        let forDeps f = concatMap f dep_pkgs
            dep_pkgs = PackageIndex.topologicalOrder (installedPkgs lbi)
            transitive_dep_ids = map Installed.sourcePackageId dep_pkgs

        let Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)
            hc_args = programDefaultArgs ghcProg
                   ++ hcOptions GHC bi
                   ++ languageToFlags (compiler lbi) (defaultLanguage bi)
                   ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
                   ++ programOverrideArgs ghcProg

        pure PackageData { pdPackage         = packageId pd
                         , pdDependencies    = map snd $ externalPackageDeps lbi
                         , pdTransitiveDeps  = transitive_dep_ids
                         , pdDepCcArgs       = forDeps Installed.ccOptions
                         , pdDepLdArgs       = forDeps Installed.ldOptions
                         , pdDepLibDirs      = forDeps Installed.libraryDirs
                         , pdDepIncludeDirs  = forDeps Installed.includeDirs
                         , pdDepExtraLibs    = forDeps Installed.extraLibraries
                         , pdHcArgs          = hc_args
                         , pdCcArgs          = ccOptions bi
                         , pdCppArgs         = cppOptions bi
                         , pdLdArgs          = ldOptions bi
                         , pdIncludeDirs     = includeDirs bi
                         , pdIncludes        = includes bi
                         , pdCSources        = cSources bi
                         , pdModules         = modules
                         , pdHiddenModules   = otherModules bi
                         , pdWithGHCiLib     = withGHCiLib lbi
                         }
