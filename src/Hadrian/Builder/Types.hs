module Hadrian.Builder.Types where

import GHC.Generics
import Development.Shake.Classes

-- | We support packing and unpacking archives with @ar@.
data ArMode = Pack | Unpack deriving (Eq, Generic, Show)

instance Binary   ArMode
instance Hashable ArMode
instance NFData   ArMode

-- | Sphinx can be used in three different modes to convert reStructuredText
-- documents into HTML, LaTeX or Man pages.
data SphinxMode = Html | Latex | Man deriving (Eq, Generic, Show)

instance Binary   SphinxMode
instance Hashable SphinxMode
instance NFData   SphinxMode

-- | Tar can be used to 'Create' an archive or 'Extract' from it.
data TarMode = Create | Extract deriving (Eq, Generic, Show)

instance Binary   TarMode
instance Hashable TarMode
instance NFData   TarMode

