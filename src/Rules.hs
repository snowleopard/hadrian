module Rules (buildRules, oracleRules, packageTargets, topLevelTargets) where

import qualified Hadrian.Oracles.ArgsHash
import qualified Hadrian.Oracles.DirectoryContents
import qualified Hadrian.Oracles.Path
import qualified Hadrian.Oracles.TextFile

import Context
import Expression
import Flavour
import GHC
import qualified Oracles.ModuleFiles
import qualified Rules.Compile
import qualified Rules.Data
import qualified Rules.Dependencies
import qualified Rules.Documentation
import qualified Rules.Generate
import qualified Rules.Configure
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Library
import qualified Rules.Perl
import qualified Rules.Program
import qualified Rules.Register
import Settings
import UserSettings (stage1Only)
import Target
import Utilities

allStages :: [Stage]
allStages = [minBound ..]

-- | This rule calls 'need' on all top-level build targets, respecting the
-- 'Stage1Only' flag.
topLevelTargets :: Rules ()
topLevelTargets = action $ do
    let libraryPackages = filter isLibrary (knownPackages \\ [rts, libffi])
    need =<< if stage1Only
             then do
                 libs <- concatForM [Stage0, Stage1] $ \stage ->
                     concatForM libraryPackages $ packageTargets False stage
                 prgs <- concatForM programsStage1Only $ packageTargets False Stage0
                 return $ libs ++ prgs ++ inplaceLibCopyTargets
             else do
                 targets <- concatForM allStages $ \stage ->
                     concatForM (knownPackages \\ [rts, libffi]) $
                        packageTargets False stage
                 return $ targets ++ inplaceLibCopyTargets


-- TODO: Get rid of the @includeGhciLib@ hack.
-- | Return the list of targets associated with a given 'Stage' and 'Package'.
-- By setting the Boolean parameter to False it is possible to exclude the GHCi
-- library from the targets, and avoid running @ghc-cabal@ to determine wether
-- GHCi library needs to be built for this package. We typically want to set
-- this parameter to True, however it is important to set it to False when
-- computing 'topLevelTargets', as otherwise the whole build gets sequentialised
-- because we need to run @ghc-cabal@ in the order respecting package dependencies.
packageTargets :: Bool -> Stage -> Package -> Action [FilePath]
packageTargets includeGhciLib stage pkg = do
    let context = vanillaContext stage pkg
    activePackages <- stagePackages stage
    if pkg `notElem` activePackages
    then return [] -- Skip inactive packages.
    else if isLibrary pkg
        then do -- Collect all targets of a library package.
            ways    <- interpretInContext context getLibraryWays
            libs    <- mapM (pkgLibraryFile . Context stage pkg) ways
            docs    <- interpretInContext context =<< buildHaddock <$> flavour
            more    <- libraryTargets includeGhciLib context
            setup   <- pkgSetupConfigFile context
            haddock <- pkgHaddockFile     context
            return $ [ setup   | nonCabalContext context ]
                  ++ [ haddock | docs && stage == Stage1 ]
                  ++ libs ++ more
        else do -- The only target of a program package is the executable.
            prgContext <- programContext stage pkg
            prgPath    <- programPath prgContext
            return [prgPath]

packageRules :: Rules ()
packageRules = do
    -- We cannot register multiple GHC packages in parallel. Also we cannot run
    -- GHC when the package database is being mutated by "ghc-pkg". This is a
    -- classic concurrent read exclusive write (CREW) conflict.
    let maxConcurrentReaders = 1000
    packageDb <- newResource "package-db" maxConcurrentReaders
    let readPackageDb  = [(packageDb, 1)]
        writePackageDb = [(packageDb, maxConcurrentReaders)]

    let contexts        = liftM3 Context        allStages knownPackages allWays
        vanillaContexts = liftM2 vanillaContext allStages knownPackages

    forM_ contexts $ mconcat
        [ Rules.Compile.compilePackage readPackageDb
        , Rules.Library.buildPackageLibrary ]

    let dynamicContexts = liftM3 Context [Stage1 ..] knownPackages [dynamic]
    forM_ dynamicContexts Rules.Library.buildDynamicLib

    forM_ vanillaContexts $ mconcat
        [ Rules.Data.buildPackageData
        , Rules.Dependencies.buildPackageDependencies readPackageDb
        , Rules.Documentation.buildPackageDocumentation
        , Rules.Library.buildPackageGhciLibrary
        , Rules.Generate.generatePackageCode
        , Rules.Program.buildProgram readPackageDb
        , Rules.Register.registerPackage writePackageDb ]

buildRules :: Rules ()
buildRules = do
    Rules.Configure.configureRules
    Rules.Generate.copyRules
    Rules.Generate.generateRules
    Rules.Gmp.gmpRules
    Rules.Libffi.libffiRules
    packageRules
    Rules.Perl.perlScriptRules

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Hadrian.Oracles.DirectoryContents.directoryContentsOracle
    Hadrian.Oracles.Path.pathOracle
    Hadrian.Oracles.TextFile.textFileOracle
    Oracles.ModuleFiles.moduleFilesOracle

programsStage1Only :: [Package]
programsStage1Only = [ deriveConstants, dllSplit, genapply, genprimopcode, ghc
                     , ghcCabal, ghcPkg, hp2ps, hpc, hsc2hs, runGhc ]
