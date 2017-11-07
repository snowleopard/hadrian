-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Parse
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Extracting Haskell package metadata stored in Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Parse ( ConfiguredCabal (..)
                                   , parseCabal, parseConfiguredCabal, parseCabalPkgId

                                   -- XXX This should be Haskell.Cabal actually
                                   , configurePackage, copyPackage, registerPackage

                                   ) where

import Stage
import Types.Context
import {-# SOURCE #-} Builder hiding (Builder)
-- import Hadrian.Builder as H
import Data.List.Extra
import Development.Shake                                      hiding (doesFileExist)
import Development.Shake.Classes
import qualified Distribution.Package                  as C
import qualified Distribution.PackageDescription       as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.PackageDescription.Configuration as C
import qualified Distribution.Text                     as C
import qualified Distribution.Types.CondTree           as C
import qualified Distribution.Types.Dependency         as C
import qualified Distribution.Types.MungedPackageId    as C (mungedName)
import qualified Distribution.Verbosity                as C
import qualified Distribution.Simple.Compiler          as C (packageKeySupported, languageToFlags, extensionsToFlags)
import qualified Distribution.Simple.GHC               as GHC
import qualified Distribution.Simple.Program.Db        as Db
import qualified Distribution.Simple                   as Hooks (simpleUserHooks, autoconfUserHooks)
import qualified Distribution.Simple.UserHooks         as Hooks
import qualified Distribution.Simple.Program.Builtin   as C
import qualified Distribution.Simple.Program.Types     as C (programDefaultArgs, programOverrideArgs)
import qualified Distribution.Simple.Configure         as C (getPersistBuildConfig)
import qualified Distribution.Simple.Build             as C (initialBuildSteps)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Simple.LocalBuildInfo    as LBI
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Types.LocalBuildInfo as C
import Distribution.Text (display)
import Distribution.Simple (defaultMainWithHooksNoReadArgs, compilerFlavor, CompilerFlavor( GHC ))
import Distribution.Simple.Compiler (compilerInfo)
import Hadrian.Package
import Hadrian.Utilities
import System.FilePath
import System.Directory
import GHC.Generics
import qualified Distribution.ModuleName as ModuleName
import Data.Maybe (maybeToList, fromMaybe, fromJust)
import GHC.Packages (rts)
import Hadrian.Expression
import Hadrian.Target
import Types.Cabal ( Cabal( Cabal ) )
import Types.ConfiguredCabal

import Settings
import Oracles.Setting

import Hadrian.Haskell.Cabal

import Context.Paths

import Settings.Builders.GhcCabal
import Settings.Default
import Context

import Hadrian.Oracles.TextFile


-- TODO: Use fine-grain tracking instead of tracking the whole Cabal file.
-- | Haskell package metadata extracted from a Cabal file.

parseCabalPkgId :: FilePath -> IO String
parseCabalPkgId file = C.display . C.package . C.packageDescription <$> C.readGenericPackageDescription C.silent file


biModules :: C.PackageDescription -> (C.BuildInfo, [ModuleName.ModuleName])
biModules pd = go [ comp | comp@(bi,_) <- (map libBiModules . maybeToList $ C.library pd)
                                         ++ (map exeBiModules $ C.executables pd)
                        , C.buildable bi ]
  where libBiModules lib = (C.libBuildInfo lib, C.libModules lib)
        exeBiModules exe = (C.buildInfo exe, ModuleName.main : C.exeModules exe)
        go [] = error "no buildable component found"
        go [x] = x
        go _  = error "can not handle more than one buildinfo yet!"


-- TODO: Taken from Context, but Context depends on Oracles.Settings, and this
--       would then lead to recursive imports.
contextPath :: Context -> Action FilePath
contextPath context = buildRoot <&> (-/- contextDir context)

buildDir :: Context -> FilePath
buildDir context = contextDir context -/- "build"

parseCabal :: Context -> Action Cabal
parseCabal context@Context {..} = do
    let (Just file) = pkgCabalFile package

    -- read the package description from the cabal file
    gpd <- liftIO $ C.readGenericPackageDescription C.silent file

    -- configure the package with the ghc compiler for this stage.
    hcPath <- builderPath' (Ghc CompileHs stage)
    (compiler, Just platform, _pgdb) <- liftIO $ GHC.configure C.silent (Just hcPath) Nothing Db.emptyProgramDb


    flagList <- interpret (target context (CabalFlags stage) [] []) defaultPackageArgs
    let flags = foldr addFlag mempty flagList
          where addFlag :: String -> C.FlagAssignment -> C.FlagAssignment
                addFlag ('-':name) = C.insertFlagAssignment (C.mkFlagName name) False
                addFlag ('+':name) = C.insertFlagAssignment (C.mkFlagName name) True
                addFlag name       = C.insertFlagAssignment (C.mkFlagName name) True

    let (Right (pd,_)) = C.finalizePackageDescription flags (const True) platform (compilerInfo compiler) [] gpd
    -- depPkgs are all those packages that are needed. These should be found in
    -- the known build packages.  Even if they are not build in this stage.
    let depPkgs = map (findPackageByName' . C.unPackageName . C.depPkgName) . C.buildDepends $ pd
          where findPackageByName' p = case findPackageByName p of
                  Just p' -> p'
                  Nothing -> error $ "Failed to find package: " ++ show p
    return $ Cabal (C.unPackageName . C.pkgName . C.package $ pd)
                   (C.display . C.pkgVersion . C.package $ pd)
                   (C.synopsis pd)
                   gpd
                   pd
                   depPkgs

configurePackage :: Context -> Action ()
configurePackage context@Context {..} = do
    Just (Cabal _ _ _ gpd pd depPkgs) <- readCabalFile context

    -- Stage packages are those we have in this stage.
    stagePkgs <- stagePackages stage
    -- we'll need those package in our package database.
    need =<< sequence [ pkgConfFile (context { package = pkg }) | pkg <- depPkgs, pkg `elem` stagePkgs ]

    -- figure out what hooks we need.
    hooks <- case C.buildType (C.flattenPackageDescription gpd) of
          Just C.Configure -> pure Hooks.autoconfUserHooks
          -- time has a "Custom" Setup.hs, but it's actually Configure
          -- plus a "./Setup test" hook. However, Cabal is also
          -- "Custom", but doesn't have a configure script.
          Just C.Custom ->
              do configureExists <- liftIO $ doesFileExist (replaceFileName (unsafePkgCabalFile package) "configure")
                 if configureExists
                     then pure Hooks.autoconfUserHooks
                     else pure Hooks.simpleUserHooks
          -- not quite right, but good enough for us:
          _ | package == rts ->
              -- don't try to do post conf validation for rts.
              -- this will simply not work, due to the ld-options,
              -- and the Stg.h.
              pure $ Hooks.simpleUserHooks { Hooks.postConf = \_ _ _ _ -> return () }
            | otherwise -> pure Hooks.simpleUserHooks


    case pkgCabalFile package of
      Nothing -> error "No a cabal package!"
      Just f -> do
        -- compute the argList. This reuses the GhcCabal Conf builder for now.
        -- and will include the flags for this context as well.
        flagList <- interpret (target context (CabalFlags stage) [] []) defaultPackageArgs
        argList <- interpret (target context (GhcCabal Conf stage) [] []) ghcCabalBuilderArgs
        liftIO $ do
          putStrLn $ "running main... for " ++ show (pkgPath package)
          putStrLn $ show $ argList ++ ["--flags=" ++ unwords flagList ]
          defaultMainWithHooksNoReadArgs hooks gpd (argList ++ ["--flags=" ++ unwords flagList])

-- XXX: move this somewhere else. This is logic from ghc-cabal
copyPackage :: Context -> Action ()
copyPackage context@Context {..} = do
  -- original invocation
        -- build $ target context (GhcCabal Copy stage) [ (pkgPath package) -- <directory>
        --                                          , ctxPath -- <distdir>
        --                                          , ":" -- no strip. ':' special marker
        --                                          , stgPath -- <destdir>
        --                                          , ""      -- <prefix>
        --                                          , "lib"   -- <libdir>
        --                                          , "share" -- <docdir>
        --                                          , "v"     -- TODO: <way> e.g. "v dyn" for dyn way.
        --                                          ] []

   -- ghc-cabal logic
-- doCopy directory distDir
--        strip myDestDir myPrefix myLibdir myDocdir withSharedLibs
--        args
--  = withCurrentDirectory directory $ do
--      let copyArgs = ["copy", "--builddir", distDir]
--                  ++ (if null myDestDir
--                      then []
--                      else ["--destdir", myDestDir])
--                  ++ args
--          copyHooks = userHooks {
--                          copyHook = noGhcPrimHook
--                                   $ modHook False
--                                   $ copyHook userHooks
--                      }

--      defaultMainWithHooksArgs copyHooks copyArgs
--     where
--       noGhcPrimHook f pd lbi us flags
--               = let pd'
--                      | packageName pd == mkPackageName "ghc-prim" =
--                         case library pd of
--                         Just lib ->
--                             let ghcPrim = fromJust (simpleParse "GHC.Prim")
--                                 ems = filter (ghcPrim /=) (exposedModules lib)
--                                 lib' = lib { exposedModules = ems }
--                             in pd { library = Just lib' }
--                         Nothing ->
--                             error "Expected a library, but none found"
--                      | otherwise = pd
--                 in f pd' lbi us flags
--       modHook relocatableBuild f pd lbi us flags
--        = do let verbosity = normal
--                 idts = updateInstallDirTemplates relocatableBuild
--                                                  myPrefix myLibdir myDocdir
--                                                  (installDirTemplates lbi)
--                 progs = withPrograms lbi
--                 stripProgram' = stripProgram {
--                     programFindLocation = \_ _ -> return (Just (strip,[])) }

--             progs' <- configureProgram verbosity stripProgram' progs
--             let lbi' = lbi {
--                                withPrograms = progs',
--                                installDirTemplates = idts,
--                                configFlags = cfg,
--                                stripLibs = fromFlag (configStripLibs cfg),
--                                withSharedLib = withSharedLibs
--                            }

--                 -- This hack allows to interpret the "strip"
--                 -- command-line argument being set to ':' to signify
--                 -- disabled library stripping
--                 cfg | strip == ":" = (configFlags lbi) { configStripLibs = toFlag False }
--                     | otherwise    = configFlags lbi

--             f pd lbi' us flags

    Just (Cabal _ _ _ gpd _ _) <- readCabalFile context

    top     <- topDirectory
    ctxPath <- (top -/-) <$> Context.contextPath context
    stgPath <- (top -/-) <$> stagePath context
    libPath <- (top -/-) <$> libPath context

    let userHooks = Hooks.autoconfUserHooks
        copyHooks = userHooks
        hooks = copyHooks

    -- we would need `withCurrentDirectory (pkgPath package)`
    liftIO $ defaultMainWithHooksNoReadArgs hooks gpd ["copy", "--builddir", ctxPath, "--destdir", stgPath]

registerPackage :: Context -> Action ()
registerPackage context@Context {..} = do
    top     <- topDirectory
    ctxPath <- (top -/-) <$> Context.contextPath context
    Just (Cabal _ _ _ gpd _ _) <- readCabalFile context
    let userHooks = Hooks.autoconfUserHooks
        regHooks = userHooks
        hooks = regHooks {
          Hooks.regHook = \pd lbi us flags ->
              let lbi' = lbi { C.installDirTemplates = updateInstallDirTemplates (C.installDirTemplates lbi) }
              in (Hooks.regHook regHooks) pd lbi' us flags
          }

    liftIO $ defaultMainWithHooksNoReadArgs hooks gpd ["register", "--builddir", ctxPath]

  -- XXX: allow configure to set a prefix with a known variable. $topdir or $pkgroot
  --      that would elivate the need for this hack.
  where updateInstallDirTemplates :: LBI.InstallDirTemplates -> LBI.InstallDirTemplates
        updateInstallDirTemplates idts = idts { LBI.prefix = LBI.toPathTemplate "${pkgroot}/.." }

-- | Parse a ConfiguredCabal file.
parseConfiguredCabal :: Context -> Action ConfiguredCabal
parseConfiguredCabal context@Context {..} = do

    Just (Cabal _ _ _ gpd pd depPkgs) <- readCabalFile context

    -- XXX: need the setup-config here, which would trigger the configure Package
    configurePackage context

    cPath <- Context.contextPath context
    liftIO $ putStrLn $ "trying to obtain the persitendBuildConfig at " ++ show cPath
    lbi <- liftIO $ C.getPersistBuildConfig cPath

    -- XXX: move this into it's own rule for build/autogen/cabal_macros.h, and build/autogen/Path_*.hs
    --      and "need" them here.
    -- create the cabal_macros.h, ...
    -- Note: the `cPath` is ignored. The path that's used is the `buildDir` path from the local build info (lbi).
    liftIO $ C.initialBuildSteps cPath pd lbi C.silent

    let extDeps = C.externalPackageDeps lbi
        deps    = map (display . snd) extDeps
        dep_direct = map (fromMaybe (error "dep_keys failed")
                          . PackageIndex.lookupUnitId (C.installedPkgs lbi)
                          . fst) extDeps
        dep_ipids = map (display . Installed.installedUnitId) dep_direct

        Just ghcProg = Db.lookupProgram C.ghcProgram (C.withPrograms lbi)

        dep_pkgs = PackageIndex.topologicalOrder (packageHacks (C.installedPkgs lbi))
        forDeps f = concatMap f dep_pkgs

        -- copied from Distribution.Simple.PreProcess.ppHsc2Hs
        packageHacks = case compilerFlavor (C.compiler lbi) of
          GHC | C.pkgName (C.package pd) /= (C.mkPackageName "rts") -> hackRtsPackage
          _   -> id
        -- We don't link in the actual Haskell libraries of our
        -- dependencies, so the -u flags in the ldOptions of the rts
        -- package mean linking fails on OS X (it's ld is a tad
        -- stricter than gnu ld). Thus we remove the ldOptions for
        -- GHC's rts package:
        hackRtsPackage index =
          case PackageIndex.lookupPackageName index (C.mkPackageName "rts") of
            [(_,[rts])] ->
              PackageIndex.insert rts{
                Installed.ldOptions = [],
                Installed.libraryDirs = filter (not . ("gcc-lib" `isSuffixOf`)) (Installed.libraryDirs rts)} index
                    -- GHC <= 6.12 had $topdir/gcc-lib in their
                    -- library-dirs for the rts package, which causes
                    -- problems when we try to use the in-tree mingw,
                    -- due to accidentally picking up the incompatible
                    -- libraries there.  So we filter out gcc-lib from
                    -- the RTS's library-dirs here.
            _ -> error "No (or multiple) ghc rts package is registered!!"

        wrap = map wrap1
        wrap1 s
          | null s        = error $ "Wrapping empty value"
          | '\'' `elem` s = error $ "Single quote in value to be wrapped:" ++ s
          -- We want to be able to assume things like <space><quote> is the
          -- start of a value, so check there are no spaces in confusing
          -- positions
          | head s == ' ' = error "Leading space in value to be wrapped:" ++ s
          | last s == ' ' = error "Trailing space in value to be wrapped:" ++ s
          | otherwise     = ("\'" ++ s ++ "\'")
      in return $ ConfiguredCabal
      { dependencies = deps
      , name     = C.unPackageName . C.pkgName . C.package $ pd
      , version  = C.display . C.pkgVersion . C.package $ pd
      -- , packageDesc = pd
      , componentId = C.localCompatPackageKey lbi
      , modules  = map C.display . snd . biModules $ pd
      , otherModules = map C.display . C.otherModules . fst . biModules $ pd
      , synopsis = C.synopsis pd
      , srcDirs = C.hsSourceDirs . fst . biModules $ pd
      , deps = deps
      , depIpIds = dep_ipids
      , depNames = map (C.display . C.mungedName . snd) extDeps
      , depCompIds = if C.packageKeySupported (C.compiler lbi)
                     then dep_ipids
                     else deps
      , includeDirs = C.includeDirs . fst . biModules $ pd
      , includes    = C.includes . fst . biModules $ pd
      , installIncludes = C.installIncludes . fst . biModules $ pd
      , extraLibs = C.extraLibs . fst . biModules $ pd
      , extraLibDirs = C.extraLibDirs . fst . biModules $ pd
      , asmSrcs = C.asmSources . fst . biModules $ pd
      , cSrcs   = C.cSources . fst . biModules $ pd
      , cmmSrcs = C.cmmSources . fst . biModules $ pd
      , dataFiles = C.dataFiles pd
      , hcOpts    =    C.programDefaultArgs ghcProg
                    ++ (C.hcOptions GHC . fst . biModules $ pd)
                    ++ C.languageToFlags (C.compiler lbi) (C.defaultLanguage . fst . biModules $ pd)
                    ++ C.extensionsToFlags (C.compiler lbi) (C.usedExtensions . fst . biModules $ pd)
                    ++ C.programOverrideArgs ghcProg
      , asmOpts   = C.asmOptions . fst . biModules $ pd
      , ccOpts    = C.ccOptions . fst . biModules $ pd
      , cmmOpts   = C.cmmOptions . fst . biModules $ pd
      , cppOpts   = C.cppOptions . fst . biModules $ pd
      , ldOpts    = C.ldOptions . fst . biModules $ pd
      , depIncludeDirs = wrap $ forDeps Installed.includeDirs
      , depCcOpts = forDeps Installed.ccOptions
      , depLdOpts = forDeps Installed.ldOptions
      , buildGhciLib = C.withGHCiLib lbi
      }
      where

collectDeps :: Maybe (C.CondTree v [C.Dependency] a) -> [C.Dependency]
collectDeps Nothing = []
collectDeps (Just (C.CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (C.CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
