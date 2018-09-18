{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.TextFile.Type
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the types of keys used by the /text file oracle/. See the
-- module "Hadrian.Oracles.TextFile" for various supported queries, and the
-- module "Hadrian.Oracles.TextFile.Rules" for the corresponing Shake rules.
-----------------------------------------------------------------------------
module Hadrian.Oracles.TextFile.Type where

import Development.Shake
import Development.Shake.Classes

import Context.Type
import Hadrian.Haskell.Cabal.Type
import Hadrian.Package

newtype TextFile = TextFile FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult TextFile = String

newtype PackageDataKey = PackageDataKey Package
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult PackageDataKey = PackageData

newtype ContextDataKey = ContextDataKey Context
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult ContextDataKey = ContextData

newtype KeyValue = KeyValue (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValue = Maybe String

newtype KeyValues = KeyValues (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValues = Maybe [String]
