{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.TextFile
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Read and parse text files, tracking their contents. This oracle can be used
-- to read configuration or package metadata files and cache the parsing.
-----------------------------------------------------------------------------
module Hadrian.Oracles.TextFile (
    readTextFile, lookupValue, lookupValueOrEmpty, lookupValueOrError,
    lookupValues, lookupValuesOrEmpty, lookupValuesOrError, lookupDependencies,
    readCabalData, unsafeReadCabalData, readPackageData, textFileOracle
    ) where

import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import GHC.Stack

import Context.Type
import Hadrian.Haskell.Cabal.CabalData
import Hadrian.Haskell.Cabal.PackageData
import {-# SOURCE #-} Hadrian.Haskell.Cabal.Parse
import Hadrian.Package
import Hadrian.Utilities
import Stage

newtype TextFile = TextFile FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult TextFile = String

newtype CabalFile = CabalFile Context
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult CabalFile = Maybe CabalData

newtype PackageDataFile = PackageDataFile Context
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult PackageDataFile = Maybe PackageData

newtype KeyValue = KeyValue (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValue = Maybe String

newtype KeyValues = KeyValues (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValues = Maybe [String]

-- | Read a text file, caching and tracking the result. To read and track
-- individual lines of a text file use 'lookupValue' and its derivatives.
readTextFile :: FilePath -> Action String
readTextFile = askOracle . TextFile

-- | Lookup a value in a text file, tracking the result. Each line of the file
-- is expected to have @key = value@ format.
lookupValue :: FilePath -> String -> Action (Maybe String)
lookupValue file key = askOracle $ KeyValue (file, key)

-- | Like 'lookupValue' but returns the empty string if the key is not found.
lookupValueOrEmpty :: FilePath -> String -> Action String
lookupValueOrEmpty file key = fromMaybe "" <$> lookupValue file key

-- | Like 'lookupValue' but raises an error if the key is not found.
lookupValueOrError :: FilePath -> String -> Action String
lookupValueOrError file key = fromMaybe (error msg) <$> lookupValue file key
  where
    msg = "Key " ++ quote key ++ " not found in file " ++ quote file

-- | Lookup a list of values in a text file, tracking the result. Each line of
-- the file is expected to have @key value1 value2 ...@ format.
lookupValues :: FilePath -> String -> Action (Maybe [String])
lookupValues file key = askOracle $ KeyValues (file, key)

-- | Like 'lookupValues' but returns the empty list if the key is not found.
lookupValuesOrEmpty :: FilePath -> String -> Action [String]
lookupValuesOrEmpty file key = fromMaybe [] <$> lookupValues file key

-- | Like 'lookupValues' but raises an error if the key is not found.
lookupValuesOrError :: FilePath -> String -> Action [String]
lookupValuesOrError file key = fromMaybe (error msg) <$> lookupValues file key
  where
    msg = "Key " ++ quote key ++ " not found in file " ++ quote file

-- | The 'Action' @lookupDependencies depFile file@ looks up dependencies of a
-- @file@ in a (typically generated) dependency file @depFile@. The action
-- returns a pair @(source, files)@, such that the @file@ can be produced by
-- compiling @source@, which in turn also depends on a number of other @files@.
lookupDependencies :: FilePath -> FilePath -> Action (FilePath, [FilePath])
lookupDependencies depFile file = do
    deps <- lookupValues depFile file
    case deps of
        Nothing -> error $ "No dependencies found for file " ++ quote file
        Just [] -> error $ "No source file found for file " ++ quote file
        Just (source : files) -> return (source, files)

-- | Read and parse a @.cabal@ file, caching and tracking the result.
readCabalData :: Context -> Action (Maybe CabalData)
readCabalData = askOracle . CabalFile

-- | Like 'readCabalData' but raises an error on a non-Cabal context.
unsafeReadCabalData :: HasCallStack => Context -> Action CabalData
unsafeReadCabalData context = fromMaybe (error msg) <$> readCabalData context
  where
    msg = "[unsafeReadCabalData] Non-Cabal context: " ++ show context

-- | Read and parse a @.cabal@ file recording the obtained 'PackageData',
-- caching and tracking the result. Note that unlike 'readCabalData' this
-- function resolves all Cabal configuration flags.
readPackageData :: Context -> Action (Maybe PackageData)
readPackageData = askOracle . PackageDataFile

-- | This oracle reads and parses text files to answer 'readTextFile' and
-- 'lookupValue' queries, as well as their derivatives, tracking the results.
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

    cabal <- newCache $ \(ctx@Context {..}) ->
        case pkgCabalFile package of
            Just file -> do
                need [file]
                putLoud $ "| CabalFile oracle: reading " ++ quote file
                       ++ " (Stage: " ++ stageString stage ++ ")..."
                Just <$> parseCabal ctx
            Nothing -> return Nothing
    void $ addOracle $ \(CabalFile ctx) -> cabal ctx

    confCabal <- newCache $ \(ctx@Context {..}) ->
        case pkgCabalFile package of
            Just file -> do
                need [file]
                putLoud $ "| PackageDataFile oracle: reading " ++ quote file
                       ++ " (Stage: " ++ stageString stage ++ ")..."
                Just <$> parsePackageData ctx
            Nothing -> return Nothing
    void $ addOracle $ \(PackageDataFile ctx) -> confCabal ctx
