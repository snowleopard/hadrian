{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Oracles.PackageData (
    PackageData (..), -- PackageDataList (..),
    -- pkgData, pkgDataList,
    packageDataOracle,
    askAllPackageData
    ) where

import GHC.Generics

import Base
import Stage
import qualified Package
import Oracles.PackageData.Internals

data PackageDataKey = PackageDataKey Stage Package.Package PackageData
    deriving (Show, Typeable, Generic, Eq, Hashable, Binary, NFData)

askAllPackageData :: Stage -> Package.Package -> Action PackageData
askAllPackageData stage pkg = -- askOracle $ PackageDataKey
    getPackageData stage pkg
--
-- -- Oracle for 'package-data.mk' files
packageDataOracle :: Rules ()
packageDataOracle = do
--    _ <- addOracle $ \(PackageDataKey stage pkg) -> getPackageData stage pkg
    return ()
