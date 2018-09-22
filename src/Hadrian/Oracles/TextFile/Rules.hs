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
module Hadrian.Oracles.TextFile.Rules where

import Control.Monad
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.Config

import Hadrian.Oracles.TextFile.Type
import Hadrian.Utilities

