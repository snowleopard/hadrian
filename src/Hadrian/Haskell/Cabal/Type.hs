-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Type
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Data types for storing basic Haskell package metadata, such as package name,
-- version and dependencies, extracted from a Cabal file.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Type where

import Development.Shake.Classes
import Distribution.PackageDescription
import GHC.Generics

import Hadrian.Package

-- | Haskell package metadata extracted from a Cabal file without performing
-- the resolution of package configuration flags and associated conditionals.
-- One consequence is that 'packageDependencies' is an overappoximation of
-- actual package dependencies; for example, both @unix@ and @win32@ packages
-- may be included even if only one of them is required on the target OS.
-- See 'PackageData' for metadata obtained after resolving package configuration
-- flags and conditionals.
data CabalData = CabalData
    { name                      :: PackageName
    , version                   :: String
    , synopsis                  :: String
    , description               :: String
    , packageDependencies       :: [Package]
    , genericPackageDescription :: GenericPackageDescription
    } deriving (Eq, Generic, Show, Typeable)

-- | Haskell package metadata obtained after resolving package configuration
-- flags and conditionals. See 'CabalData' for metadata obtained without
-- resolving package configuration flags and conditionals.
data PackageData = PackageData
    { dependencies    :: [PackageName]
    , componentId     :: String
    , mainIs          :: Maybe (String, FilePath)  -- ("Main", filepath)
    , modules         :: [String]
    , otherModules    :: [String]
    , srcDirs         :: [String]
    , deps            :: [String]
    , depIpIds        :: [String]
    , depNames        :: [String]
    , depCompIds      :: [String]
    , includeDirs     :: [String]
    , includes        :: [String]
    , installIncludes :: [String]
    , extraLibs       :: [String]
    , extraLibDirs    :: [String]
    , asmSrcs         :: [String]
    , cSrcs           :: [String]
    , cmmSrcs         :: [String]
    , dataFiles       :: [String]
    , hcOpts          :: [String]
    , asmOpts         :: [String]
    , ccOpts          :: [String]
    , cmmOpts         :: [String]
    , cppOpts         :: [String]
    , ldOpts          :: [String]
    , depIncludeDirs  :: [String]
    , depCcOpts       :: [String]
    , depLdOpts       :: [String]
    , buildGhciLib    :: Bool
    } deriving (Eq, Generic, Show, Typeable)

instance Binary   CabalData
instance Hashable CabalData where hashWithSalt salt = hashWithSalt salt . show
instance NFData   CabalData

instance Binary   PackageData
instance Hashable PackageData
instance NFData   PackageData
