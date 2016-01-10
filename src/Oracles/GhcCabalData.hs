{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, GADTs #-}

module Oracles.PackageData (
    PackageData (..), PackageDataList (..),
    pkgData, pkgDataList, packageDataOracle
    ) where

import Development.Shake.Config
import Base
import qualified Data.HashMap.Strict as Map

-- For each (PackageData path) the file 'path/package-data.mk' contains
-- a line of the form 'path_VERSION = 1.2.3.4'.
-- pkgData $ PackageData path is an action that consults the file and
-- returns "1.2.3.4".
--
-- PackageDataList is used for multiple string options separated by spaces,
-- such as 'path_MODULES = Data.Array Data.Array.Base ...'.
-- pkgListData Modules therefore returns ["Data.Array", "Data.Array.Base", ...]
data PackageDataField a where
    Package          :: PackageDataKey PackageIdentifier
    BuildGhciLib     :: PackageDataKey Bool
    CcArgs           :: PackageDataKey [String]
    CSources         :: PackageDataKey [FilePath]
    CppArgs          :: PackageDataKey [String]
    DepCcArgs        :: PackageDataKey [String]
    DepExtraLibs     :: PackageDataKey [FilePath]
    Dependencies     :: PackageDataKey [PackageIdentifier]
    DepIncludeDirs   :: PackageDataKey [FilePath]
    DepLdArgs        :: PackageDataKey [String]
    DepLibDirs       :: PackageDataKey [String]
    HiddenModules    :: PackageDataKey [ModuleName]
    HsArgs           :: PackageDataKey [String]
    IncludeDirs      :: PackageDataKey [FilePath]
    LdArgs           :: PackageDataKey [String]
    Modules          :: PackageDataKey [ModuleName]
    SrcDirs          :: PackageDataKey [FilePath]
    TransitiveDeps   :: PackageDataKey [PackageIdentifier]

newtype PackageDataKey = PackageDataKey (FilePath, String)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askPackageData :: FilePath -> String -> Action String
askPackageData path key = do
    let fullKey = replaceSeparators '_' $ path ++ "_" ++ key
        file    = path -/- "package-data.mk"
    maybeValue <- askOracle $ PackageDataKey (file, fullKey)
    case maybeValue of
        Nothing    -> return ""
        Just value -> return value
        -- Nothing    -> putError $ "No key '" ++ key ++ "' in " ++ file ++ "."

pkgData :: PackageData -> Action String
pkgData packageData = case packageData of
    BuildGhciLib path -> askPackageData path "BUILD_GHCI_LIB"
    ComponentId  path -> askPackageData path "COMPONENT_ID"
    Synopsis     path -> askPackageData path "SYNOPSIS"
    Version      path -> askPackageData path "VERSION"

pkgDataList :: PackageDataList -> Action [String]
pkgDataList packageData = fmap (map unquote . words) $ case packageData of
    CcArgs             path -> askPackageData path "CC_OPTS"
    CSrcs              path -> askPackageData path "C_SRCS"
    CppArgs            path -> askPackageData path "CPP_OPTS"
    DepCcArgs          path -> askPackageData path "DEP_CC_OPTS"
    DepExtraLibs       path -> askPackageData path "DEP_EXTRA_LIBS"
    DepIds             path -> askPackageData path "DEP_IPIDS"
    DepIncludeDirs     path -> askPackageData path "DEP_INCLUDE_DIRS_SINGLE_QUOTED"
    DepLibDirs         path -> askPackageData path "DEP_LIB_DIRS_SINGLE_QUOTED"
    DepLdArgs          path -> askPackageData path "DEP_LD_OPTS"
    DepNames           path -> askPackageData path "DEP_NAMES"
    Deps               path -> askPackageData path "DEPS"
    HiddenModules      path -> askPackageData path "HIDDEN_MODULES"
    HsArgs             path -> askPackageData path "HC_OPTS"
    IncludeDirs        path -> askPackageData path "INCLUDE_DIRS"
    LdArgs             path -> askPackageData path "LD_OPTS"
    Modules            path -> askPackageData path "MODULES"
    SrcDirs            path -> askPackageData path "HS_SRC_DIRS"
    TransitiveDepNames path -> askPackageData path "TRANSITIVE_DEP_NAMES"
  where
    unquote = dropWhile (== '\'') . dropWhileEnd (== '\'')

-- Oracle for 'package-data.mk' files
packageDataOracle :: Rules ()
packageDataOracle = do
    pkgDataContents <- newCache $ \file -> do
        need [file]
        putOracle $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    _ <- addOracle $ \(PackageDataKey (file, key)) ->
        Map.lookup key <$> pkgDataContents file
    return ()
