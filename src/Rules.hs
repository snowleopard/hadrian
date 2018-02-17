module Rules (buildRules, oracleRules, packageTargets, topLevelTargets) where

import qualified Hadrian.Oracles.ArgsHash
import qualified Hadrian.Oracles.DirectoryContents
import qualified Hadrian.Oracles.Path
import qualified Hadrian.Oracles.TextFile

import Expression
import qualified Oracles.ModuleFiles
import qualified Rules.Compile
import qualified Rules.PackageData
import qualified Rules.Dependencies
import qualified Rules.Documentation
import qualified Rules.Generate
import qualified Rules.Configure
import qualified Rules.Gmp
import qualified Rules.Libffi
import qualified Rules.Library
import qualified Rules.Program
import qualified Rules.Register
import Settings
import Target
import Utilities
import GHC.Packages
import GHC

import System.Directory (getCurrentDirectory)

import Oracles.Setting

allStages :: [Stage]
allStages = [minBound .. maxBound]

-- | This rule calls 'need' on all top-level build targets, respecting the
-- 'Stage1Only' flag.
topLevelTargets :: Rules ()
topLevelTargets = do
    phony "binary-dist" $ do
      -- This is kind of incorrect.  We should not "need" a phony rule.
      -- Instead we should *need* the libraries and binaries we want to
      -- put into the binary distribution.  For now we will just *need*
      -- stage2 and package up bin and lib.
      need ["stage2", "docs"]
      version <- setting ProjectVersion
      cwd <- liftIO getCurrentDirectory
      binDistDir <- getEnvWithDefault cwd "BINARY_DIST_DIR"
      baseDir <- buildRoot <&> (-/- stageString Stage1)
      targetPlatform <- setting TargetPlatformFull

      -- prepare binary distribution configure script
      copyFile (cwd -/- "aclocal.m4") (cwd -/- "distrib" -/- "aclocal.m4")
      buildWithCmdOptions [Cwd $ cwd -/- "distrib"] $
        target (vanillaContext Stage1 ghc) (Autoreconf $ cwd -/- "distrib") [] []

      let ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform
          bindistFilesDir  = baseDir -/- ghcVersionPretty
      createDirectory bindistFilesDir

      -- copy config.sub, config.guess, install-sh, Makefile files, etc
      -- from the source of the tree to the bindist dir
      copyFile (cwd -/- "distrib" -/- "configure") (bindistFilesDir -/- "configure")
      copyFile (cwd -/- "distrib" -/- "Makefile") (bindistFilesDir -/- "Makefile")
      copyFile (cwd -/- "install-sh") (bindistFilesDir -/- "install-sh")
      copyFile (cwd -/- "config.sub") (bindistFilesDir -/- "config.sub")
      copyFile (cwd -/- "config.guess") (bindistFilesDir -/- "config.guess")
      copyFile (cwd -/- "settings.in") (bindistFilesDir -/- "settings.in")
      copyFile (cwd -/- "mk" -/- "config.mk.in") (bindistFilesDir -/- "mk" -/- "config.mk.in")
      copyFile (cwd -/- "mk" -/- "install.mk.in") (bindistFilesDir -/- "mk" -/- "install.mk.in")
      copyDirectory (baseDir -/- "bin") bindistFilesDir
      copyDirectory (baseDir -/- "lib") bindistFilesDir
      copyDirectory (takeDirectory baseDir -/- "docs") bindistFilesDir

      -- TODO: move stage1/bin, stage1/lib and all the files above to some
      --       other (temporary?) directory, and invoke tar there
      -- TODO: test with another flavour than quick-with-ng

      buildWithCmdOptions [Cwd baseDir] $
        -- ghc is a fake package here.
        target (vanillaContext Stage1 ghc) (Tar Create)
               [ ghcVersionPretty ]
               {- [ "bin", "lib", "docs", "configure", "config.sub", "config.guess"
               , "install-sh", "settings.in", "mk/config.mk.in", "mk/install.mk.in"
               , "Makefile"
               ] -}
               [binDistDir -/- ghcVersionPretty ++ ".tar.xz"]

    phony "stage2" $ do
      putNormal "Building stage2"
      (programs, libraries) <- partition isProgram <$> stagePackages Stage1
      pgmNames <- mapM (g Stage1) programs
      libNames <- mapM (g Stage1) libraries
      putNormal . unlines $
        ["| Building Programs:  " ++ intercalate ", " pgmNames
        ,"| Building Libraries: " ++ intercalate ", " libNames]

      targets <- mapM (f Stage1) =<< stagePackages Stage1
      need targets
      where
        -- either the package databae config file for libraries or
        -- the programPath for programs. However this still does
        -- not support multiple targets, where a cabal package has
        -- a library /and/ a program.
        f :: Stage -> Package -> Action FilePath
        f stage pkg | isLibrary pkg = pkgConfFile (Context stage pkg (read "v"))
                    | otherwise     = programPath =<< programContext stage pkg
        g :: Stage -> Package -> Action String
        g stage pkg | isLibrary pkg = return $ pkgName pkg
                    | otherwise     = programName (Context stage pkg (read "v"))

-- TODO: Get rid of the @includeGhciLib@ hack.
-- | Return the list of targets associated with a given 'Stage' and 'Package'.
-- By setting the Boolean parameter to False it is possible to exclude the GHCi
-- library from the targets, and avoid running @ghc-cabal@ to determine whether
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
            let pkgWays = if pkg == rts then getRtsWays else getLibraryWays
            ways    <- interpretInContext context pkgWays
            libs    <- mapM (pkgLibraryFile . Context stage pkg) ways
            more    <- libraryTargets includeGhciLib context
            setup   <- pkgSetupConfigFile context
            return $ [ setup | not (nonCabalContext context) ] ++ libs ++ more
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

    Rules.Program.buildProgram readPackageDb

    forM_ [Stage0 .. ] $ \stage -> do
      -- we create a dummy context, that has the correct state, but contains
      -- @base@ as a dummy package. The package isn't accessed but the record
      -- need to be set properly. @undefined@ is not an option as it ends up
      -- being forced.
      Rules.Register.registerPackages writePackageDb (Context stage base vanilla)

    forM_ vanillaContexts $ mconcat
        [ Rules.PackageData.buildPackageData
        , Rules.Dependencies.buildPackageDependencies readPackageDb
        , Rules.Documentation.buildPackageDocumentation
        , Rules.Library.buildPackageGhciLibrary
        , Rules.Generate.generatePackageCode ]

buildRules :: Rules ()
buildRules = do
    Rules.Configure.configureRules
    Rules.Generate.copyRules
    Rules.Generate.generateRules
    Rules.Gmp.gmpRules
    Rules.Libffi.libffiRules
    packageRules

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Hadrian.Oracles.DirectoryContents.directoryContentsOracle
    Hadrian.Oracles.Path.pathOracle
    Hadrian.Oracles.TextFile.textFileOracle
    Oracles.ModuleFiles.moduleFilesOracle

-- programsStage1Only :: [Package]
-- programsStage1Only = [ deriveConstants, genapply, genprimopcode, ghc, ghcCabal
--                      , ghcPkg, hp2ps, hpc, hsc2hs, runGhc ]
