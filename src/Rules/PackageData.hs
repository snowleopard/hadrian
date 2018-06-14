module Rules.PackageData (buildPackageData) where

import Base
import Context

import Hadrian.Haskell.Cabal.Parse (configurePackage)

-- TODO: Shall we drop this tiny module?
-- | Build @setup-config@ files for packages. Look at the "Rules" module to see
-- this instantiated against all the packages.
buildPackageData :: Context -> Rules ()
buildPackageData context@Context {..} = do
    root <- buildRootRules
    root -/- contextDir context -/- "setup-config" %> \_ -> configurePackage context
