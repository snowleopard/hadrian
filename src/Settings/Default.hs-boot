module Settings.Default (
    SourceArgs (..), sourceArgs, defaultBuilderArgs, defaultPackageArgs,
    defaultArgs, defaultLibraryWays, defaultRtsWays, quickRtsWays,
    quickestRtsWays, defaultFlavour, defaultSplitObjects
    ) where

import Flavour
import Expression

data SourceArgs = SourceArgs
    { hsDefault  :: Args
    , hsLibrary  :: Args
    , hsCompiler :: Args
    , hsGhc      :: Args }

sourceArgs :: SourceArgs -> Args

defaultBuilderArgs, defaultPackageArgs, defaultArgs :: Args
defaultLibraryWays, defaultRtsWays, quickRtsWays, quickestRtsWays :: Ways
defaultFlavour :: Flavour
defaultSplitObjects :: Predicate
