module Settings.User (
    userArgs, userPackages, userLibWays, userRtsWays, userTargetDirectory,
    userProgramPath, userKnownPackages, integerLibrary,
    trackBuildSystem, buildHaddock, validating, ghciWithDebugger, ghcProfiled,
    ghcDebugged, dynamicGhcPrograms, laxDependencies, buildSystemConfigFile,
    verboseCommands, warnAll, warnTabs, turnWarningsIntoErrors, splitObjects
    ) where

import GHC
import Expression

-- No user-specific settings by default
-- TODO: rename to userArgs
userArgs :: Args
userArgs = mempty

-- Control which packages get to be built
userPackages :: Packages
userPackages = mempty

-- Add new user-defined packages
userKnownPackages :: [Package]
userKnownPackages = []

-- Control which ways libraries and rts are built
-- TODO: skip profiling for speed, skip dynamic since it's currently broken
userLibWays :: Ways
userLibWays = remove [profiling, dynamic]

userRtsWays :: Ways
userRtsWays = mempty

-- Control where build results go (see GHC.hs for defaults)
userTargetDirectory :: Stage -> Package -> FilePath
userTargetDirectory = defaultTargetDirectory

-- Control how built programs are called (see GHC.hs for defaults)
userProgramPath :: Stage -> Package -> Maybe FilePath
userProgramPath = defaultProgramPath

-- Choose integer library: integerGmp, integerGmp2 or integerSimple
integerLibrary :: Package
integerLibrary = integerGmp

-- User-defined flags. Note the following type semantics:
-- * Bool: a plain Boolean flag whose value is known at compile time
-- * Action Bool: a flag whose value can depend on the build environment
-- * Predicate: a flag depending on the build environment and the current target

-- Set this to True if you are making any changes in the build system and want
-- appropriate rebuilds to be initiated. Switching this to False speeds things
-- up a little (particularly zero builds).
-- WARNING: a complete rebuild is required when changing this setting.
trackBuildSystem :: Bool
trackBuildSystem = True

validating :: Bool
validating = False

-- To switch off split objects change to 'return False'
splitObjects :: Predicate
splitObjects = return False -- FIXME: should be defaultSplitObjects, see #84.

-- | switch @-Wall@ on during ghc compilation passes.
warnAll :: Predicate
warnAll = return False

-- | switch @-fwarn-tabs@ on during ghc compiliation passes.
warnTabs :: Predicate
warnTabs = return False

dynamicGhcPrograms :: Bool
dynamicGhcPrograms = False

ghciWithDebugger :: Bool
ghciWithDebugger = False

ghcProfiled :: Bool
ghcProfiled = False

-- TODO: do we need to be able to set this from command line?
ghcDebugged :: Bool
ghcDebugged = False

-- When laxDependencies flag is set to True, dependencies on the GHC executable
-- are turned into order-only dependencies to avoid needless recompilation when
-- making changes to GHC's sources. In certain situations this can lead to build
-- failures, in which case you should reset the flag (at least temporarily).
laxDependencies :: Bool
laxDependencies = False

buildHaddock :: Predicate
buildHaddock = return False -- FIXME: should be return True, see #98

buildSystemConfigFile :: Bool
buildSystemConfigFile = False

-- Set to True to print full command lines during the build process. Note, this
-- is a Predicate, hence you can enable verbose output for a chosen package
-- only, e.g.: verboseCommands = package ghcPrim
verboseCommands :: Predicate
verboseCommands = return False

-- | To enable -Werror in Stage2 set turnWarningsIntoErrors = stage2.
turnWarningsIntoErrors :: Predicate
turnWarningsIntoErrors = return False
