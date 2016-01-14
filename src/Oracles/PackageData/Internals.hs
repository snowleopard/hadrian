{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveAnyClass #-}
module Oracles.PackageData.Internals where

import Base
import Expression
import GHC hiding (compiler, directory)
import GHC.Generics
import Oracles.Config.Setting
import Package
import Settings.Paths hiding (includes)

import Distribution.ModuleName as ModuleName
import Distribution.Package as P
import Distribution.PackageDescription as PD
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Simple (defaultHookedPackageDesc, defaultMain, defaultMainWithHooks, autoconfUserHooks)
import Distribution.Simple.Build (writeAutogenFiles)
import Distribution.Simple.Compiler
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program
import Distribution.Simple.Register
import Distribution.Simple.Utils (defaultPackageDesc, writeFileAtomic, toUTF8)
import Distribution.System
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex

import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Exception ( bracket )
import System.Directory (setCurrentDirectory, getCurrentDirectory) -- , doesFileExist)
import qualified System.Directory as IO
import System.Environment (withArgs)
import Distribution.Text (display)

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

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

runDefaultMain :: IO ()
runDefaultMain
 = do let verbosity = normal
      gpdFile <- defaultPackageDesc verbosity
      gpd <- readPackageDescription verbosity gpdFile
      case buildType (flattenPackageDescription gpd) of
          Just Configure -> defaultMainWithHooks autoconfUserHooks
          -- time has a "Custom" Setup.hs, but it's actually Configure
          -- plus a "./Setup test" hook. However, Cabal is also
          -- "Custom", but doesn't have a configure script.
          Just Custom ->
              do configureExists <- IO.doesFileExist "configure"
                 if configureExists
                     then defaultMainWithHooks autoconfUserHooks
                     else defaultMain
          -- not quite right, but good enough for us:
          _ -> defaultMain

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
    | pkg == rts   = do
        windows <- windowsHost
        let target = PartialTarget stage pkg
            dirs   = [ ".", "hooks", "sm", "eventlog" ]
                  ++ [ "posix" | not windows          ]
                  ++ [ "win32" |     windows          ]
        -- TODO: rts/dist/build/sm/Evac_thr.c, rts/dist/build/sm/Scav_thr.c
        -- TODO: adding cmm/S sources to C_SRCS is a hack; rethink after #18
        cSrcs    <- getDirectoryFiles (pkgPath pkg) (map (-/- "*.c") dirs)
        cmmSrcs  <- getDirectoryFiles (pkgPath pkg) ["*.cmm"]
        buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
        buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
        let sSrcs     = [ "AdjustorAsm.S" | buildAdjustor   ]
                     ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]
            extraSrcs = [ rtsBuildPath -/- "AutoApply.cmm" ]
        includes <- interpretPartial target $ fromDiffExpr includesArgs
        return $ emptyPackageData { pdCSources     = cSrcs ++ cmmSrcs ++ sSrcs ++ extraSrcs
                                  , pdCcArgs       = includes
                                  , pdComponentId  = "rts" }
    | otherwise    = do
        -- Largely stolen from utils/ghc-cabal/Main.hs
        let verbosity = silent
            directory = targetPath stage pkg
            distdir   = stageString stage
        need [pkgCabalFile pkg]
        -- XXX We shouldn't just configure with the default flags
        -- XXX And this, and thus the "getPersistBuildConfig distdir" below,
        -- aren't going to work when the deps aren't built yet
        liftIO $ withCurrentDirectory (pkgPath pkg)
            (withArgs (["configure", "--distdir", distdir, "--ipid", "$pkg-$version"]) -- ++ config_args)
                runDefaultMain)

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

        -- generate Paths_<pkg>.hs and cabal-macros.h
        liftIO $ writeAutogenFiles verbosity pd lbi

        -- generate inplace-pkg-config
        liftIO $ withLibLBI pd lbi $ \lib clbi ->
            do cwd <- getCurrentDirectory
               let ipid = ComponentId (display (packageId pd))
               let installedPkgInfo = inplaceInstalledPackageInfo cwd distdir
                                          pd (Installed.AbiHash "") lib lbi clbi
                   final_ipi = mangleIPI directory distdir lbi $ installedPkgInfo {
                                   Installed.installedComponentId = ipid,
                                   Installed.compatPackageKey = ipid,
                                   Installed.haddockHTMLs = []
                               }
                   content = Installed.showInstalledPackageInfo final_ipi ++ "\n"
               writeFileAtomic (distdir </> "inplace-pkg-config") (BS.pack $ toUTF8 content)


        let dep_ids  = map snd (externalPackageDeps lbi)
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

-- On Windows we need to split the ghc package into 2 pieces, or the
-- DLL that it makes contains too many symbols (#5987). There are
-- therefore 2 libraries, not just the 1 that Cabal assumes.
mangleIPI :: FilePath -> FilePath -> LocalBuildInfo
          -> Installed.InstalledPackageInfo -> Installed.InstalledPackageInfo
mangleIPI "compiler" "stage2" lbi ipi
 | isWindows =
    -- Cabal currently only ever installs ONE Haskell library, c.f.
    -- the code in Cabal.Distribution.Simple.Register.  If it
    -- ever starts installing more we'll have to find the
    -- library that's too big and split that.
    let [old_hslib] = Installed.hsLibraries ipi
    in ipi {
        Installed.hsLibraries = [old_hslib, old_hslib ++ "-0"]
    }
    where isWindows = case hostPlatform lbi of
                      Platform _ Windows -> True
                      _                  -> False
mangleIPI _ _ _ ipi = ipi
