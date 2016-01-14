{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveAnyClass #-}
module Oracles.PackageData.Internals where

import Base
import Expression
import GHC hiding (compiler)
import GHC.Generics
import Package
import Settings.Paths hiding (includes)

import Distribution.ModuleName as ModuleName
import Distribution.Package as P
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

import Distribution.Text (display)

-- | Various information of interest scaped from Cabal's 'BuildInfo'.
data PackageData = PackageData { pdComponentId    :: String
                               , pdVersion        :: String
                               , pdSynopsis       :: String
                               , pdPackage        :: PackageIdentifier
                               , pdDependencies   :: [PackageIdentifier]
                               , pdTransitiveDeps :: [PackageIdentifier]
                               , pdDepCcArgs      :: [String]
                               , pdDepLdArgs      :: [String]
                               , pdDepExtraLibs   :: [String]
                               , pdDepIncludeDirs :: [FilePath]
                               , pdDepLibDirs     :: [FilePath]
                               , pdDeps           :: [String]
                               , pdDepNames       :: [String]
                               , pdHsSourceDirs   :: [FilePath]
                               , pdHcArgs         :: [String]
                               , pdHsArgs         :: [String]
                               , pdCcArgs         :: [String]
                               , pdCppArgs        :: [String]
                               , pdLdArgs         :: [String]
                               , pdIncludeDirs    :: [FilePath]
                               , pdIncludes       :: [String]
                               , pdDepIpIds       :: [String]
                               , pdCSources       :: [FilePath]
                               , pdModules        :: [ModuleName]
                               , pdHiddenModules  :: [ModuleName]
                               , pdWithGHCiLib    :: Bool
                               }
                               deriving (Show, Generic, Eq)

emptyPackageData :: PackageData
emptyPackageData = PackageData { pdComponentId     = ""
                               , pdVersion         = ""
                               , pdSynopsis        = ""
                               , pdPackage         = PackageIdentifier { P.pkgName = P.PackageName { unPackageName = "" }
                                                                       , pkgVersion = (read "0.0.0") }
                               , pdDependencies    = []
                               , pdTransitiveDeps  = []
                               , pdDepCcArgs       = []
                               , pdDepLdArgs       = []
                               , pdDepLibDirs      = []
                               , pdDepIncludeDirs  = []
                               , pdDepExtraLibs    = []
                               , pdDeps            = []
                               , pdDepNames        = []
                               , pdHsSourceDirs    = []
                               , pdHcArgs          = []
                               , pdHsArgs          = []
                               , pdCcArgs          = []
                               , pdCppArgs         = []
                               , pdLdArgs          = []
                               , pdIncludeDirs     = []
                               , pdIncludes        = []
                               , pdDepIpIds        = []
                               , pdCSources        = []
                               , pdModules         = []
                               , pdHiddenModules   = []
                               , pdWithGHCiLib     = False }

instance Hashable ModuleName where
    hashWithSalt salt = hashWithSalt salt . show
instance NFData ModuleName where
    rnf = rnf . show
instance Hashable PackageIdentifier where
    hashWithSalt salt = hashWithSalt salt . show
deriving instance Hashable PackageData
deriving instance Binary PackageData
deriving instance NFData PackageData

getPackageData :: Stage -> Package.Package -> Action PackageData
getPackageData stage pkg
    | pkg == hp2ps = do
            let target = PartialTarget stage pkg
            includes <- interpretPartial target $ fromDiffExpr includesArgs
            let cSrcs  = [ "AreaBelow.c", "Curves.c", "Error.c", "Main.c"
                         , "Reorder.c", "TopTwenty.c", "AuxFile.c"
                         , "Deviation.c", "HpFile.c", "Marks.c", "Scale.c"
                         , "TraceElement.c", "Axes.c", "Dimensions.c", "Key.c"
                         , "PsFile.c", "Shade.c", "Utilities.c" ]
            return $ emptyPackageData { pdCSources     = cSrcs
                                      , pdDepExtraLibs = ["m"]
                                      , pdCcArgs       = includes }
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
            dep_ids  = map snd (externalPackageDeps lbi)
            deps     = map display dep_ids
            dep_direct = map (fromMaybe (error "ghc-cabal: dep_keys failed")
                             . PackageIndex.lookupComponentId
                                              (installedPkgs lbi)
                             . fst)
                         . externalPackageDeps
                         $ lbi
            dep_ipids = map (display . Installed.installedComponentId) dep_direct
            depNames = map (display . packageName) dep_ids

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

        pure PackageData { pdComponentId     = display (localCompatPackageKey lbi)
                         , pdVersion         = display (pkgVersion (package pd))
                         , pdSynopsis        = (unwords $ lines $ synopsis pd)
                         , pdPackage         = packageId pd
                         , pdDependencies    = map snd $ externalPackageDeps lbi
                         , pdTransitiveDeps  = transitive_dep_ids
                         , pdDepCcArgs       = forDeps Installed.ccOptions
                         , pdDepLdArgs       = forDeps Installed.ldOptions
                         , pdDepLibDirs      = forDeps Installed.libraryDirs
                         , pdDepIncludeDirs  = forDeps Installed.includeDirs
                         , pdDepExtraLibs    = forDeps Installed.extraLibraries
                         , pdDeps            = deps
                         , pdDepNames        = depNames
                         , pdHsSourceDirs    = hsSourceDirs bi
                         , pdHcArgs          = hc_args
                         , pdHsArgs          = programDefaultArgs ghcProg
                                               ++ hcOptions GHC bi
                                               ++ languageToFlags (compiler lbi) (defaultLanguage bi)
                                               ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
                                               ++ programOverrideArgs ghcProg
                         , pdCcArgs          = ccOptions bi
                         , pdCppArgs         = cppOptions bi
                         , pdLdArgs          = ldOptions bi
                         , pdIncludeDirs     = includeDirs bi
                         , pdIncludes        = includes bi
                         , pdDepIpIds        = dep_ipids
                         , pdCSources        = cSources bi
                         , pdModules         = modules
                         , pdHiddenModules   = otherModules bi
                         , pdWithGHCiLib     = withGHCiLib lbi
                         }
