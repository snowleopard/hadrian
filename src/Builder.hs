{-# LANGUAGE InstanceSigs #-}
module Builder (
    -- * Data types
    ArMode (..), CcMode (..), ConfigurationInfo (..), GhcMode (..),
    GhcPkgMode (..), HaddockMode (..), SphinxMode (..), TarMode (..),
    Builder (..),

    -- * Builder properties
    builderProvenance, systemBuilderPath, builderPath, isSpecified, needBuilder,
    runBuilder, runBuilderWith, runBuilderWithCmdOptions, getBuilderPath,
    builderEnvironment,

    -- * Ad hoc builder invocation
    applyPatch
    ) where

import Development.Shake.Classes
import GHC.Generics
import qualified Hadrian.Builder as H
import Hadrian.Builder hiding (Builder)
import Hadrian.Builder.Ar
import Hadrian.Builder.Sphinx
import Hadrian.Builder.Tar
import Hadrian.Oracles.Path
import Hadrian.Oracles.TextFile
import Hadrian.Utilities

import Base
import Context
import Oracles.Flag
import Oracles.Setting
import Packages

-- | C compiler can be used in two different modes:
-- * Compile or preprocess a source file.
-- * Extract source dependencies by passing @-MM@ command line argument.
data CcMode = CompileC | FindCDependencies deriving (Eq, Generic, Show)

instance Binary   CcMode
instance Hashable CcMode
instance NFData   CcMode

-- | GHC can be used in four different modes:
-- * Compile a Haskell source file.
-- * Compile a C source file.
-- * Extract source dependencies by passing @-M@ command line argument.
-- * Link object files & static libraries into an executable.
data GhcMode = CompileHs | CompileCWithGhc | FindHsDependencies | LinkHs
    deriving (Eq, Generic, Show)

instance Binary   GhcMode
instance Hashable GhcMode
instance NFData   GhcMode

-- | To configure a package we need two pieces of information, which we choose
-- to record separately for convenience.
--
-- * Command line arguments to be passed to the setup script.
--
-- * Package configuration flags that enable/disable certain package features.
--   Here is an example from "Settings.Packages":
--
--   > package rts
--   >   ? builder (Cabal Flags)
--   >   ? any (wayUnit Profiling) rtsWays
--   >   ? arg "profiling"
--
--   This instructs package configuration functions (such as 'configurePackage')
--   to enable the @profiling@ Cabal flag when processing @rts.cabal@ and
--   building RTS with profiling information.
data ConfigurationInfo = Setup | Flags deriving (Eq, Generic, Show)

instance Binary   ConfigurationInfo
instance Hashable ConfigurationInfo
instance NFData   ConfigurationInfo

-- TODO: Do we really need all these modes? Why do we need 'Dependencies'? We
-- can extract dependencies using the Cabal library.
-- | 'GhcPkg' can initialise a package database and register packages in it.
data GhcPkgMode = Init         -- ^ Initialize a new database.
                | Update       -- ^ Update a package.
                | Copy         -- ^ Copy a package from one database to another.
                | Unregister   -- ^ Unregister a package.
                | Dependencies -- ^ Compute package dependencies.
                deriving (Eq, Generic, Show)

instance Binary   GhcPkgMode
instance Hashable GhcPkgMode
instance NFData   GhcPkgMode

-- | Haddock can be used in two different modes:
-- * Generate documentation for a single package
-- * Generate an index page for a collection of packages
data HaddockMode = BuildPackage | BuildIndex deriving (Eq, Generic, Show)

instance Binary   HaddockMode
instance Hashable HaddockMode
instance NFData   HaddockMode

-- | A 'Builder' is a (usually external) command invoked in a separate process
-- via 'cmd'. Here are some examples:
-- * 'Alex' is a lexical analyser generator that builds @Lexer.hs@ from @Lexer.x@.
-- * 'Ghc' 'Stage0' is the bootstrapping Haskell compiler used in 'Stage0'.
-- * 'Ghc' @StageN@ (N > 0) is the GHC built in stage (N - 1) and used in @StageN@.
--
-- The 'Cabal' builder is unusual in that it does not correspond to an external
-- program but instead relies on the Cabal library for package configuration.
data Builder = Alex
             | Ar ArMode Stage
             | Autoreconf FilePath
             | DeriveConstants
             | Cabal ConfigurationInfo Stage
             | Cc CcMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcPkg GhcPkgMode Stage
             | Haddock HaddockMode
             | Happy
             | Hpc
             | Hp2Ps
             | HsCpp
             | Hsc2Hs Stage
             | Ld Stage
             | Make FilePath
             | Nm
             | Objdump
             | Patch
             | Perl
             | Python
             | Ranlib
             | RunTest
             | Sphinx SphinxMode
             | Tar TarMode
             | Unlit
             | Xelatex
             deriving (Eq, Generic, Show)

instance Binary   Builder
instance Hashable Builder
instance NFData   Builder

-- | Some builders are built by this very build system, in which case
-- 'builderProvenance' returns the corresponding build 'Context' (which includes
-- 'Stage' and GHC 'Package').
builderProvenance :: Builder -> Maybe Context
builderProvenance = \case
    DeriveConstants  -> context Stage0 deriveConstants
    GenApply         -> context Stage0 genapply
    GenPrimopCode    -> context Stage0 genprimopcode
    Ghc _ Stage0     -> Nothing
    Ghc _ stage      -> context (pred stage) ghc
    GhcPkg _ Stage0  -> Nothing
    GhcPkg _ _       -> context Stage0 ghcPkg
    Haddock _        -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hp2Ps            -> context Stage0 hp2ps
    Hsc2Hs _         -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

instance H.Builder Builder where
    builderPath :: Builder -> Action FilePath
    builderPath builder = case builderProvenance builder of
        Nothing      -> systemBuilderPath builder
        Just context -> programPath context

    runtimeDependencies :: Builder -> Action [FilePath]
    runtimeDependencies = \case
        Autoreconf dir -> return [dir -/- "configure.ac"]
        Configure  dir -> return [dir -/- "configure"]

        Ghc _ Stage0 -> return []
        Ghc _ stage -> do
            root <- buildRoot
            win <- windowsHost
            touchyPath <- programPath (vanillaContext Stage0 touchy)
            unlitPath  <- builderPath Unlit
            ghcdeps <- ghcDeps stage
            return $ [ root -/- ghcSplitPath stage -- TODO: Make conditional on --split-objects
                     , unlitPath ]
                  ++ ghcdeps
                  ++ [ touchyPath | win ]

        Hsc2Hs stage -> (\p -> [p]) <$> templateHscPath stage
        Make dir  -> return [dir -/- "Makefile"]
        _         -> return []

    -- query the builder for some information.
    -- contrast this with runBuilderWith, which returns @Action ()@
    -- this returns the @stdout@ from running the builder.
    -- For now this only implements asking @ghc-pkg@ about package
    -- dependencies.
    askBuilderWith :: Builder -> BuildInfo -> Action String
    askBuilderWith builder BuildInfo {..} = case builder of
        GhcPkg Dependencies _ -> do
            let input  = fromSingleton msgIn buildInputs
                msgIn  = "[askBuilder] Exactly one input file expected."
            needBuilder builder
            path <- H.builderPath builder
            need [path]
            Stdout stdout <- cmd [path] ["--no-user-package-db", "field", input, "depends"]
            return stdout
        _ -> error $ "Builder " ++ show builder ++ " can not be asked!"

    runBuilderWith :: Builder -> BuildInfo -> Action ()
    runBuilderWith builder BuildInfo {..} = do
        path <- builderPath builder
        withResources buildResources $ do
            verbosity <- getVerbosity
            let input  = fromSingleton msgIn buildInputs
                msgIn  = "[runBuilderWith] Exactly one input file expected."
                output = fromSingleton msgOut buildOutputs
                msgOut = "[runBuilderWith] Exactly one output file expected."
                -- Suppress stdout depending on the Shake's verbosity setting.
                echo = EchoStdout (verbosity >= Loud)
                -- Capture stdout and write it to the output file.
                captureStdout = do
                    Stdout stdout <- cmd [path] buildArgs
                    writeFileChanged output stdout
            case builder of
                Ar Pack _ -> do
                    useTempFile <- flag ArSupportsAtFile
                    if useTempFile then runAr                path buildArgs
                                   else runArWithoutTempFile path buildArgs

                Ar Unpack _ -> cmd echo [Cwd output] [path] buildArgs

                Autoreconf dir -> cmd echo [Cwd dir] [path] buildArgs
                Configure  dir -> do
                    -- Inject /bin/bash into `libtool`, instead of /bin/sh,
                    -- otherwise Windows breaks. TODO: Figure out why.
                    bash <- bashPath
                    let env = AddEnv "CONFIG_SHELL" bash
                    cmd echo env [Cwd dir] ["sh", path] buildOptions buildArgs

                HsCpp    -> captureStdout
                GenApply -> captureStdout

                GenPrimopCode -> do
                    stdin <- readFile' input
                    Stdout stdout <- cmd (Stdin stdin) [path] buildArgs
                    writeFileChanged output stdout

                Make dir -> cmd echo path ["-C", dir] buildArgs

                Xelatex -> do
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] ["makeindex"] (input -<.> "idx")
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] [path]        buildArgs

                GhcPkg Copy _ -> do
                    Stdout pkgDesc <- cmd [path]
                      [ "--expand-pkgroot"
                      , "--no-user-package-db"
                      , "describe"
                      , input -- the package name
                      ]
                    cmd (Stdin pkgDesc) [path] (buildArgs ++ ["-"])

                GhcPkg Unregister _ -> do
                    Exit _ <- cmd echo [path] (buildArgs ++ [input])
                    return ()

                _  -> cmd echo [path] buildArgs

-- TODO: Some builders are required only on certain platforms. For example,
-- 'Objdump' is only required on OpenBSD and AIX. Add support for platform
-- specific optional builders as soon as we can reliably test this feature.
-- See https://github.com/snowleopard/hadrian/issues/211.
isOptional :: Builder -> Bool
isOptional = \case
    Objdump  -> True
    _        -> False

-- | Determine the location of a system 'Builder'.
systemBuilderPath :: Builder -> Action FilePath
systemBuilderPath builder = case builder of
    Alex            -> fromKey "alex"
    Ar _ Stage0     -> fromKey "system-ar"
    Ar _ _          -> fromKey "ar"
    Autoreconf _    -> fromKey "autoreconf"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsCpp           -> fromKey "hs-cpp"
    Ld _            -> fromKey "ld"
    Make _          -> fromKey "make"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Perl            -> fromKey "perl"
    Python          -> fromKey "python"
    Ranlib          -> fromKey "ranlib"
    RunTest         -> fromKey "python"
    Sphinx _        -> fromKey "sphinx-build"
    Tar _           -> fromKey "tar"
    Xelatex         -> fromKey "xelatex"
    _               -> error $ "No entry for " ++ show builder ++ inCfg
  where
    inCfg = " in " ++ quote configFile ++ " file."
    fromKey key = do
        let unpack = fromMaybe . error $ "Cannot find path to builder "
                ++ quote key ++ inCfg ++ " Did you skip configure?"
        path <- unpack <$> lookupValue configFile key
        if null path
        then do
            unless (isOptional builder) . error $ "Non optional builder "
                ++ quote key ++ " is not specified" ++ inCfg
            return "" -- TODO: Use a safe interface.
        else do
            win <- windowsHost
            fullPath <- lookupInPath path
            case (win, hasExtension fullPath) of
                (False, _    ) -> return fullPath
                (True , True ) -> fixAbsolutePathOnWindows fullPath
                (True , False) -> fixAbsolutePathOnWindows fullPath <&> (<.> exe)

-- | Was the path to a given system 'Builder' specified in configuration files?
isSpecified :: Builder -> Action Bool
isSpecified = fmap (not . null) . systemBuilderPath

-- | Apply a patch by executing the 'Patch' builder in a given directory.
applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    needBuilder Patch
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd [Cwd dir, FileStdin file] [path, "-p0"]
