module Hadrian.Haskell.Cabal.Type where

import Development.Shake.Classes
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription)
import GHC.Generics
import Hadrian.Package.Type

data Cabal = Cabal
    { name         :: PackageName
    , version      :: String
    , synopsis     :: String
    , genericPackageDescription :: GenericPackageDescription
    , packageDescription :: PackageDescription -- ^ the configured generic package description
    , packageDependencies :: [Package]
    } deriving (Eq, Show, Typeable, Generic)

instance Binary Cabal

instance Hashable Cabal where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData Cabal where
    rnf (Cabal a b c d e f)
      = a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` ()
