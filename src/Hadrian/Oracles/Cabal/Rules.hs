-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.Cabal.Rules
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines Shake rules corresponing to the /Cabal oracle/; see
-- the module "Hadrian.Oracles.Cabal" for various supported queries.
-----------------------------------------------------------------------------
module Hadrian.Oracles.Cabal.Rules where

import Control.Monad
import Data.Maybe
import Development.Shake
import Distribution.Simple.GHC
import Distribution.Simple.Program.Db
import Distribution.Verbosity

import Builder
import Context.Type
import Hadrian.Haskell.Cabal.Parse
import Hadrian.Oracles.Cabal.Type
import Hadrian.Package
import Hadrian.Utilities

-- | This oracle reads and parses Cabal files to answer various queries, caching
-- and tracking the results.
cabalOracle :: Rules ()
cabalOracle = do
    packageData <- newCache $ \package -> do
        let file = pkgCabalFile package
        need [file]
        putLoud $ "| PackageData oracle: parsing " ++ quote file ++ "..."
        parsePackageData package
    void $ addOracleCache $ \(PackageDataKey package) -> packageData package

    contextData <- newCache $ \(context@Context {..}) -> do
        putLoud $ "| ContextData oracle: resolving data for "
               ++ quote (pkgName package) ++ " (" ++ show stage
               ++ ", " ++ show way ++ ")..."
        resolveContextData context
    void $ addOracleCache $ \(ContextDataKey context) -> contextData context

    conf <- newCache $ \(pkg, stage) -> do
        putLoud $ "| PackageConfiguration oracle: configuring "
               ++ quote (pkgName pkg) ++ " (" ++ show stage ++ ")..."
        -- Configure the package with the GHC corresponding to the given stage
        hcPath <- builderPath (Ghc CompileHs stage)
        (compiler, maybePlatform, _pkgdb) <- liftIO $
            configure silent (Just hcPath) Nothing emptyProgramDb
        let platform = fromMaybe (error msg) maybePlatform
            msg      = "PackageConfiguration oracle: cannot detect platform"
        return $ PackageConfiguration (compiler, platform)
    void $ addOracleCache $ \(PackageConfigurationKey pkgStage) -> conf pkgStage
