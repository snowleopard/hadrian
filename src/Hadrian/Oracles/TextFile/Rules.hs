-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.TextFile.Rules
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines Shake rules corresponing to the /text file oracle/; see
-- the module "Hadrian.Oracles.TextFile" for various supported queries.
-----------------------------------------------------------------------------
module Hadrian.Oracles.TextFile.Rules (textFileOracle) where

import Control.Monad
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.Config

import Context.Type
import Hadrian.Haskell.Cabal.Parse
import Hadrian.Oracles.TextFile.Type
import Hadrian.Package
import Hadrian.Utilities

-- | This oracle reads and parses text files to answer various queries, caching
-- and tracking the results.
textFileOracle :: Rules ()
textFileOracle = do
    text <- newCache $ \file -> do
        need [file]
        putLoud $ "| TextFile oracle: reading " ++ quote file ++ "..."
        liftIO $ readFile file
    void $ addOracle $ \(TextFile file) -> text file

    kv <- newCache $ \file -> do
        need [file]
        putLoud $ "| KeyValue oracle: reading " ++ quote file ++ "..."
        liftIO $ readConfigFile file
    void $ addOracle $ \(KeyValue (file, key)) -> Map.lookup key <$> kv file

    kvs <- newCache $ \file -> do
        need [file]
        putLoud $ "| KeyValues oracle: reading " ++ quote file ++ "..."
        contents <- map words <$> readFileLines file
        return $ Map.fromList [ (key, values) | (key:values) <- contents ]
    void $ addOracle $ \(KeyValues (file, key)) -> Map.lookup key <$> kvs file

    packageData <- newCache $ \package -> do
        let file = pkgCabalFile package
        need [file]
        putLoud $ "| PackageData oracle: parsing " ++ quote file ++ "..."
        parsePackageData package
    void $ addOracle $ \(PackageDataKey package) -> packageData package

    contextData <- newCache $ \(context@Context {..}) -> do
        putLoud $ "| ContextData oracle: resolving data for "
               ++ quote (pkgName package) ++ " (" ++ show stage
               ++ ", " ++ show way ++ ")..."
        resolveContextData context
    void $ addOracle $ \(ContextDataKey context) -> contextData context
