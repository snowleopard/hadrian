module Hadrian.Haskell.Cabal.Parse where

import Types.Context
import Types.Cabal (Cabal)
import Types.ConfiguredCabal (ConfiguredCabal)
import Development.Shake

parseCabal :: Context -> Action Cabal
parseConfiguredCabal :: Context -> Action ConfiguredCabal