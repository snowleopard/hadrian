module Types.Context where

import Types.Stage
import Types.Package
import Way

import GHC.Generics
import Development.Shake.Classes

-- | Build context for a currently built 'Target'. We generate potentially
-- different build rules for each 'Context'.
data Context = Context
    { stage   :: Stage   -- ^ Currently build Stage
    , package :: Package -- ^ Currently build Package
    , way     :: Way     -- ^ Currently build Way (usually 'vanilla')
    } deriving (Eq, Generic, Show)

instance Binary   Context
instance Hashable Context
instance NFData   Context

