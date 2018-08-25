module Hadrian.Haskell.Cabal.Parse where

import Context.Type
import Development.Shake
import Hadrian.Haskell.Cabal.CabalData
import Hadrian.Haskell.Cabal.PackageData

parseCabal :: Context -> Action CabalData
parsePackageData :: Context -> Action PackageData
