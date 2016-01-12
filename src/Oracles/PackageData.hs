{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Oracles.PackageData (
    PackageData (..), PackageDataList (..),
    pkgData, pkgDataList, packageDataOracle
    ) where

import GHC.Generics

import Development.Shake.Config
import Development.Shake.Rule

import Base
import Stage
import qualified Package
import Oracles.PackageData.Internals

import Distribution.Package
import Distribution.ModuleName as ModuleName

extract :: PackageDataField a -> PackageData -> a
extract Package          = pdPackage
extract BuildGhciLib     = pdWithGHCiLib
extract CcArgs           = pdCcArgs
extract CSources         = pdCSources

-- For each (PackageData path) the file 'path/package-data.mk' contains
-- a line of the form 'path_VERSION = 1.2.3.4'.
-- pkgData $ PackageData path is an action that consults the file and
-- returns "1.2.3.4".
--
-- PackageDataList is used for multiple string options separated by spaces,
-- such as 'path_MODULES = Data.Array Data.Array.Base ...'.
-- pkgListData Modules therefore returns ["Data.Array", "Data.Array.Base", ...]
data PackageDataField a where
    Package          :: PackageDataField PackageIdentifier
    CcArgs           :: PackageDataField [String]
    CSources         :: PackageDataField [FilePath]
    CppArgs          :: PackageDataField [String]
    DepCcArgs        :: PackageDataField [String]
    DepExtraLibs     :: PackageDataField [FilePath]
    Dependencies     :: PackageDataField [PackageIdentifier]
    DepIncludeDirs   :: PackageDataField [FilePath]
    DepLdArgs        :: PackageDataField [String]
    DepLibDirs       :: PackageDataField [String]
    HiddenModules    :: PackageDataField [ModuleName]
    HsArgs           :: PackageDataField [String]
    IncludeDirs      :: PackageDataField [FilePath]
    LdArgs           :: PackageDataField [String]
    Modules          :: PackageDataField [ModuleName]
    SrcDirs          :: PackageDataField [FilePath]
    TransitiveDeps   :: PackageDataField [PackageIdentifier]
    BuildGhciLib     :: PackageDataField Bool
    deriving (Generic, Typeable)

deriving instance Show (PackageDataField a)
deriving instance Eq (PackageDataField a)
deriving instance Hashable (PackageDataField a)
deriving instance Binary (PackageDataField a)
instance NFData (PackageDataField a) where
    rnf x = x `seq` ()

data PackageDataKey a = PackageDataKey Stage Package.Package (PackageDataField a)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

instance (ShakeValue a) => Rule (PackageDataKey a) a where
  storedValue _ _ = return Nothing

declarePackageData :: Rules ()
declarePackageData = rule f
  where
    f :: (ShakeValue a, Typeable a) => PackageDataKey a -> Maybe (Action a)
    f (PackageDataKey stage pkg fld) = Just $ do
      pd <- askAllPackageData stage pkg
      pure $ extract fld pd

askAllPackageData :: Stage -> Package.Package -> Action PackageData
askAllPackageData stage pkg = askOracle $ PackageDataKey stage pkg

-- Oracle for 'package-data.mk' files
packageDataOracle :: Rules ()
packageDataOracle = do
    _ <- addOracle $ \(PackageDataKey stage pkg) -> getPackageData stage pkg
    return ()
