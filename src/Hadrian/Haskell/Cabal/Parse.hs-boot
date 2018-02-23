module Hadrian.Haskell.Cabal.Parse where

import Context.Type (Context)
import Development.Shake (Action)
import Hadrian.Haskell.Cabal.Type (Cabal)
import Hadrian.Haskell.Cabal.Configured (ConfiguredCabal)

parseCabal :: Context -> Action Cabal
parseConfiguredCabal :: Context -> Action ConfiguredCabal
