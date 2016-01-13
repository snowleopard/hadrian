{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, GADTs #-}

module Oracles.PackageData (
    -- PackageData (..), PackageDataList (..),
    -- pkgData, pkgDataList, packageDataOracle
    ) where

import Development.Shake.Config
import Base
import Distribution.Package
import Distribution.ModuleName as ModuleName
import qualified Data.HashMap.Strict as Map
import Oracles.PackageData.Internals
