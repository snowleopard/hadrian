{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.Cabal.Type
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the types of keys used by the /Cabal oracle/. See the
-- module "Hadrian.Oracles.Cabal" for various supported queries, and the
-- module "Hadrian.Oracles.Cabal.Rules" for the corresponing Shake rules.
-----------------------------------------------------------------------------
module Hadrian.Oracles.Cabal.Type where

import Development.Shake
import Development.Shake.Classes
import qualified Distribution.Simple as C
import qualified Distribution.System as C

import Context.Type
import Hadrian.Haskell.Cabal.Type
import Hadrian.Package
import Stage

newtype PackageDataKey = PackageDataKey Package
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult PackageDataKey = PackageData

newtype PackageConfiguration = PackageConfiguration (C.Compiler, C.Platform)
    deriving (Binary, Eq, Show, Typeable)

instance NFData PackageConfiguration where
    rnf (PackageConfiguration (c, p)) =
        rnf (C.compilerId c)                      `seq`
        rnf (C.abiTagString $ C.compilerAbiTag c) `seq`
        rnf (C.compilerCompat c)                  `seq`
        rnf (C.compilerLanguages c)               `seq`
        rnf (C.compilerExtensions c)              `seq`
        rnf (C.compilerProperties c)              `seq`
        rnf p

instance Hashable PackageConfiguration where
    hashWithSalt _ = hash . show

newtype PackageConfigurationKey = PackageConfigurationKey (Package, Stage)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
-- TODO: Should @RuleResult PackageConfigurationKey@ be simply ()? Presumably
-- the pair @(Compiler, Maybe Platform)@ is fully determined by the Stage.
type instance RuleResult PackageConfigurationKey = PackageConfiguration

newtype ContextDataKey = ContextDataKey Context
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult ContextDataKey = ContextData
