{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.PackageDeps (packageDeps, packageDepsOracle) where

import Base
import qualified Data.HashMap.Strict as Map
import Package
import Settings.TargetDirectory

import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Simple (defaultHookedPackageDesc)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Verbosity
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex

newtype PackageDepsKey = PackageDepsKey PackageName
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- packageDeps name is an action that given a package looks up its dependencies
-- in Base.packageDependencies file. The dependencies need to be computed by
-- scanning package cabal files (see Rules.Cabal).
packageDeps :: Package -> Action [PackageName]
packageDeps pkg = do
    res <- askOracle . PackageDepsKey . pkgName $ pkg
    return . fromMaybe [] $ res

-- Oracle for the package dependencies file
packageDepsOracle :: Rules ()
packageDepsOracle = do
    deps <- newCache $ \_ -> do
        putOracle $ "Reading package dependencies..."
        contents <- readFileLines packageDependencies
        return . Map.fromList
               $ [ (head ps, tail ps) | line <- contents, let ps = map PackageName $ words line ]
    _ <- addOracle $ \(PackageDepsKey pkg) -> Map.lookup pkg <$> deps ()
    return ()


packageDependencies :: Stage -> Package -> Action [PackageName]
pacakgeDependencies stage pkg
    | pkg == hp2ps = return []
    | otherwise    = do
        let distdir = targetPath stage pkg
        need [pkgCabalFile pkg]
        lbi <- getPersistBuildConfig distdir
        hooked_bi <-
            if (buildType pd0 == Just Configure) || (buildType pd0 == Just Custom)
            then do
                maybe_infoFile <- defaultHookedPackageDesc
                case maybe_infoFile of
                    Nothing       -> return emptyHookedBuildInfo
                    Just infoFile -> readHookedBuildInfo verbosity infoFile
            else
                return emptyHookedBuildInfo

        let pd = updatePackageDescription hooked_bi pd0
        let transitive_dep_ids = map Installed.sourcePackageId dep_pkgs
            dep_ipids = map (display . Installed.installedComponentId) dep_direct
            dep_pkgs = PackageIndex.topologicalOrder (packageHacks (installedPkgs lbi))

        pd <- liftIO $ readPackageDescription silent $ pkgCabalFile pkg
        buildDepends pd
