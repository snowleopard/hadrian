-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic functionality for extracting Haskell package metadata stored in
-- Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal (
    pkgVersion, pkgIdentifier, pkgSynopsis, pkgDependencies, pkgGenericDescription
    ) where

import Development.Shake
import Distribution.PackageDescription (GenericPackageDescription)

import Hadrian.Haskell.Cabal.CabalData
import Hadrian.Oracles.TextFile
import Hadrian.Package

-- | Read a Cabal file and return the package version. The Cabal file is tracked.
pkgVersion :: Package -> Action String
pkgVersion = fmap version . readCabalData

-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0@.
-- The Cabal file is tracked.
pkgIdentifier :: Package -> Action String
pkgIdentifier package = do
    cabal <- readCabalData package
    return $ if null (version cabal)
        then name cabal
        else name cabal ++ "-" ++ version cabal

-- | Read a Cabal file and return the package synopsis. The Cabal file is tracked.
pkgSynopsis :: Package -> Action String
pkgSynopsis = fmap synopsis . readCabalData

-- | Read a Cabal file and return the sorted list of the package dependencies.
-- The current version does not take care of Cabal conditionals and therefore
-- returns a crude overapproximation of actual dependencies. The Cabal file is
-- tracked.
pkgDependencies :: Package -> Action [PackageName]
pkgDependencies = fmap (map pkgName . packageDependencies) . readCabalData

-- | Read a Cabal file and return the 'GenericPackageDescription'. The Cabal
-- file is tracked.
pkgGenericDescription :: Package -> Action GenericPackageDescription
pkgGenericDescription = fmap genericPackageDescription . readCabalData
