-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Package
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- A /package/ is a collection of files. We currently only support C and Haskell
-- packages and treat a package as either a library or a program. The latter is
-- a gross oversimplification as, for example, Haskell packages can be both.
-- This works for now, but should be improved in future.
-----------------------------------------------------------------------------
module Hadrian.Package (
    -- * Data types
    Package (..), PackageName, PackageType,

    -- * Construction and properties
    library, program, dummyPackage, isLibrary, isProgram,

    -- * Package directory structure
    pkgCabalFile
    ) where

import Development.Shake.FilePath

import Hadrian.Package.Type
import Hadrian.Utilities

-- | Construct a library package.
library :: PackageName -> FilePath -> Package
library = Package Library

-- | Construct a program package.
program :: PackageName -> FilePath -> Package
program = Package Program

-- TODO: Remove this hack.
-- | A dummy package that we never try to build but use when we need a 'Package'
-- to construct a 'Context' but do not need to access the package field.
dummyPackage :: Package
dummyPackage = library "dummy" "dummy/path/"

-- | Is this a library package?
isLibrary :: Package -> Bool
isLibrary (Package Library _ _) = True
isLibrary _ = False

-- | Is this a program package?
isProgram :: Package -> Bool
isProgram (Package Program _ _) = True
isProgram _ = False

-- | The path to the Cabal file of a Haskell package, e.g. @ghc/ghc-bin.cabal@.
pkgCabalFile :: Package -> FilePath
pkgCabalFile p = pkgPath p -/- pkgName p <.> "cabal"
