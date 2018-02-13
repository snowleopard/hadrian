{-# LANGUAGE CPP #-}
module Settings (
    getArgs, getLibraryWays, getRtsWays, flavour, knownPackages,
    findPackageByName, isLibrary, stagePackages,
    programContext, getIntegerPackage, getDestDir
    ) where

import CommandLine
import Expression
import Types.Flavour
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Development
import Settings.Flavours.Performance
import Settings.Flavours.Profiled
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Flavours.QuickCross
import UserSettings
import GHC.Packages
#if defined(LLVMNG)
import Settings.Flavours.QuickCrossNG
import Settings.Flavours.QuickWithNG
#endif

getArgs :: Args
getArgs = expr flavour >>= args

getLibraryWays :: Ways
getLibraryWays = expr flavour >>= libraryWays

getRtsWays :: Ways
getRtsWays = expr flavour >>= rtsWays

stagePackages :: Stage -> Action [Package]
stagePackages stage = do
    f <- flavour
    packages f stage

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ defaultFlavour, developmentFlavour Stage1, developmentFlavour Stage2
    , performanceFlavour, profiledFlavour, quickFlavour, quickestFlavour
    , quickCrossFlavour
    -- TODO: if we have flavours that refer to packages
    --       we incorrectly eagerly load those packages
    --       and cabal files; which will fail if said
    --       package does not exist.
#if defined(LLVMNG)
    , quickCrossNGFlavour, quickWithNGFlavour
#endif
    ]

extraFlavourPackages :: [Package]
extraFlavourPackages = nub . sort $ concatMap extraPackages hadrianFlavours

flavour :: Action Flavour
flavour = do
    flavourName <- fromMaybe "default" <$> cmdFlavour
    let unknownFlavour = error $ "Unknown build flavour: " ++ flavourName
        flavours       = hadrianFlavours ++ userFlavours
    return $ fromMaybe unknownFlavour $ find ((== flavourName) . name) flavours

getIntegerPackage :: Expr Package
getIntegerPackage = expr (integerLibrary =<< flavour)

programContext :: Stage -> Package -> Action Context
programContext stage pkg = do
    profiled <- ghcProfiled <$> flavour
    return $ if pkg == ghc && profiled && stage > Stage0
             then Context stage pkg profiling
             else vanillaContext stage pkg

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ ghcPackages ++ userPackages ++ extraFlavourPackages

-- TODO: Speed up? Switch to Set?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findPackageByName :: PackageName -> Maybe Package
findPackageByName name = find (\pkg -> pkgName pkg == name) knownPackages

-- | Install's DESTDIR setting.
getDestDir :: Action FilePath
getDestDir = fromMaybe "" <$> cmdInstallDestDir
