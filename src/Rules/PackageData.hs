module Rules.PackageData (buildPackageData) where

import Base
import Context
import Expression
import GHC.Packages
import Settings.Packages.Rts
import Target
import Utilities

import Hadrian.Haskell.Cabal.Parse (configurePackage)

-- | Build @package-data.mk@ by using ghc-cabal utility to process .cabal files.
buildPackageData :: Context -> Rules ()
buildPackageData context@Context {..} = do
    root <- buildRootRules
    let dir = root -/- contextDir context
    -- TODO: Get rid of hardcoded file paths.
    dir -/- "setup-config" %> \_ -> configurePackage context

    dir -/- "inplace-pkg-config" %> \conf -> do
      dataFile <- pkgDataFile context
      need [dataFile]
      when (package == rts) $ do
        genPath <- buildRoot <&> (-/- generatedDir)
        rtsPath <- rtsBuildPath
        need [rtsConfIn]
        build $ target context HsCpp [rtsConfIn] [conf]
        fixFile conf $ unlines
                     . map
                     ( replace "\"\"" ""
                     . replace "rts/dist/build" rtsPath
                     . replace "includes/dist-derivedconstants/header" genPath )
                     . lines
