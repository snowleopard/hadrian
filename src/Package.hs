{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Package (
    Package (..), PackageName (..), PackageType (..),
    -- * Queries
    pkgNameString,
    pkgCabalFile,
    matchPackageNames,
    -- * Helpers for constructing and using 'Package's
    setPath, topLevel, library, utility, setType, setWrapper, isLibrary,
    isProgram, isWrapped
    ) where

import Base
import GHC.Generics (Generic)
import Data.String

-- | The name of a Cabal package
newtype PackageName = PackageName { getPackageName :: String }
    deriving (Eq, Ord, IsString, Generic, Binary, Hashable, Typeable, NFData)

instance Show PackageName where
    show (PackageName name) = name

-- | We regard packages as either being libraries or programs. This is
-- bit of a convenient lie as Cabal packages can be both, but it works
-- for now.
data PackageType = Program | Library deriving Generic

type PackageWrapper = String

data Package = Package
     {
         pkgName :: PackageName, -- ^ Examples: "ghc", "Cabal"
         pkgPath :: FilePath,    -- ^ pkgPath is the path to the source code relative to the root.
                                 -- e.g. "compiler", "libraries/Cabal/Cabal"
         pkgType :: PackageType,
         pkgWrapper :: Maybe PackageWrapper
     }
     deriving Generic

pkgNameString :: Package -> String
pkgNameString = getPackageName . pkgName

-- | Relative path to cabal file, e.g.: "libraries/Cabal/Cabal/Cabal.cabal"
pkgCabalFile :: Package -> FilePath
pkgCabalFile pkg = pkgPath pkg -/- getPackageName (pkgName pkg) <.> "cabal"

topLevel :: PackageName -> Package
topLevel name = Package name (getPackageName name) Library Nothing

library :: PackageName -> Package
library name = Package name ("libraries" -/- getPackageName name) Library Nothing

utility :: PackageName -> Package
utility name = Package name ("utils" -/- getPackageName name) Program Nothing

setPath :: Package -> FilePath -> Package
setPath pkg path = pkg { pkgPath = path }

setType :: Package -> PackageType -> Package
setType pkg ty = pkg { pkgType = ty }

setWrapper :: Package -> PackageWrapper -> Package
setWrapper pkg wrapper = pkg { pkgWrapper = Just wrapper }

isLibrary :: Package -> Bool
isLibrary (Package {pkgType=Library}) = True
isLibrary _ = False

isProgram :: Package -> Bool
isProgram (Package {pkgType=Program}) = True
isProgram _ = False

isWrapped :: Package -> Bool
isWrapped (Package {pkgWrapper=Nothing}) = False
isWrapped _ = True

instance Show Package where
    show = show . pkgName

instance Eq Package where
    (==) = (==) `on` pkgName

instance Ord Package where
    compare = compare `on` pkgName

-- Given a sorted list of packages and a sorted list of package names, returns
-- packages whose names appear in the list of names
matchPackageNames :: [Package] -> [PackageName] -> [Package]
matchPackageNames = intersectOrd (\pkg name -> compare (pkgName pkg) name)

-- Instances for storing in the Shake database
instance Binary Package
instance Hashable Package where
    hashWithSalt salt = hashWithSalt salt . show
instance NFData Package

instance Binary PackageType
instance Hashable PackageType
instance NFData PackageType
