module Hadrian.Haskell.Cabal.Parse where

import Context.Type (Context)
import Development.Shake (Action)
import Hadrian.Haskell.Cabal.PackageData (PackageData)
import Hadrian.Haskell.Cabal.Type (CabalData)

parseCabal :: Context -> Action CabalData
parsePackageData :: Context -> Action PackageData
