module Rules.Library (
    buildPackageLibrary, buildPackageGhciLibrary, buildDynamicLib
    ) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Parse as Cabal
import qualified System.Directory as IO

import Base
import Context
import Expression hiding (way, package)
import Flavour
import Oracles.ModuleFiles
import Oracles.PackageData
import Oracles.Setting
import Rules.Gmp
import Settings
import Target
import Utilities

archive :: Way -> String -> String
archive way pkgId = "libHS" ++ pkgId ++ (waySuffix way <.> "a")

pkgObject :: Way -> String -> String
pkgObject way pkgId = "HS" ++ pkgId ++ (waySuffix way <.> "o")

-- | Building a library consist of building
-- the artefacts, and copying it somewhere
-- with cabal, and finally registering it
-- with the compiler via cabal in the
-- package database.
--
-- So we'll assume rules to build all the
-- package artifacts, and provide rules for
-- the any of the lirbary artifacts.
library :: Context -> Rules ()
library context@Context{..} = do
    pkgId <- case pkgCabalFile package of
      Just file -> do
        cabal <- liftIO $ parseCabal file
        return $ if (null $ version cabal)
          then Cabal.name cabal
          else Cabal.name cabal ++ "-" ++ version cabal
      Nothing   -> return (pkgName package)

    "//" ++ libDir context -/- pkgId -/- archive way pkgId %> \a -> do
      -- ghc-cabal copy libraries/terminfo $PWD/_build/stage0/libraries/terminfo : $PWD/_build/stage1 "" "lib" "share" "v"
      -- ghc-cabal register libraries/terminfo $PWD/_build/stage0/libraries/terminfo ghc ghc-pkg $PWD/_build/stage1/lib $PWD/_build_stage1 "" "lib" "share" YES
      _a <- buildPath context <&> (-/- archive way pkgId)
      _o <- buildPath context <&> (-/- pkgObject way pkgId)

      need [_a, _o]

      -- might need some package-db resource to limit read/write,
      -- see packageRules
      top     <- topDirectory
      ctxPath <- (top -/-) <$> contextPath context
      stgPath <- (top -/-) <$> stagePath context
      libPath <- (top -/-) <$> libPath context
      build $ target context (GhcCabal Copy stage) [ "libraries" -/- (pkgName package) -- <directory>
                                                   , ctxPath -- <distdir>
                                                   , ":" -- no strip. ':' special marker
                                                   , stgPath -- <destdir>
                                                   , ""      -- <prefix>
                                                   , "lib"   -- <libdir>
                                                   , "share" -- <docdir>
                                                   , "v"     -- TODO: <way> e.g. "v dyn" for dyn way.
                                                   ] []
      build $ target context (GhcCabal Reg stage)  [ "libraries" -/- (pkgName package)
                                                   , ctxPath
                                                   , "ghc"     -- TODO: path to staged ghc.
                                                   , "ghc-pkg" -- TODO: path to staged ghc-pkg.
                                                   , libPath
                                                   , stgPath
                                                   , ""
                                                   , "lib"
                                                   , "share"
                                                   , "YES"   -- <relocatable>
                                                   ] [a]

libraryObjects :: Context -> Action [FilePath]
libraryObjects context@Context{..} = do
    hsObjs   <- hsObjects    context
    noHsObjs <- nonHsObjects context

    -- This will create split objects if required (we don't track them
    -- explicitly as this would needlessly bloat the Shake database).
    need $ noHsObjs ++ hsObjs

    split <- interpretInContext context =<< splitObjects <$> flavour
    let getSplitObjs = concatForM hsObjs $ \obj -> do
            let dir = dropExtension obj ++ "_" ++ osuf way ++ "_split"
            contents <- liftIO $ IO.getDirectoryContents dir
            return . map (dir -/-) $ filter (not . all (== '.')) contents

    (noHsObjs ++) <$> if split then getSplitObjs else return hsObjs

buildDynamicLib :: Context -> Rules ()
buildDynamicLib context@Context{..} = do
    pkgId <- case pkgCabalFile package of
      Just file -> do
        cabal <- liftIO $ parseCabal file
        return $ if (null $ version cabal)
          then Cabal.name cabal
          else Cabal.name cabal ++ "-" ++ version cabal
      Nothing   -> return (pkgName package)

    let libPrefix = "//" ++ buildDir context -/- "libHS" ++ pkgId
    -- OS X
    libPrefix ++ "*.dylib" %> buildDynamicLibUnix
    -- Linux
    libPrefix ++ "*.so"    %> buildDynamicLibUnix
    -- TODO: Windows
  where
    buildDynamicLibUnix lib = do
        deps <- contextDependencies context
        need =<< mapM pkgLibraryFile deps
        objs <- libraryObjects context
        build $ target context (Ghc LinkHs stage) objs [lib]

buildPackageLibrary :: Context -> Rules ()
buildPackageLibrary context@Context {..} = do
    pkgId <- case pkgCabalFile package of
      Just file -> do
        cabal <- liftIO $ parseCabal file
        return $ if (null $ version cabal)
          then Cabal.name cabal
          else Cabal.name cabal ++ "-" ++ version cabal
      Nothing   -> return (pkgName package)

    let libPrefix = "//" ++ buildDir context -/- "libHS" ++ pkgId
        archive = libPrefix ++ (waySuffix way <.> "a")
    archive %%> \a -> do
        objs <- libraryObjects context
        asuf <- libsuf way
        let isLib0 = ("//*-0" ++ asuf) ?== a
        removeFile a
        if isLib0 then build $ target context (Ar Pack stage) []   [a] -- TODO: Scan for dlls
                  else build $ target context (Ar Pack stage) objs [a]

        synopsis <- traverse pkgSynopsis (pkgCabalFile package)
        unless isLib0 . putSuccess $ renderLibrary
            (quote (pkgName package) ++ " (" ++ show stage ++ ", way "
            ++ show way ++ ").") a synopsis

    library context

buildPackageGhciLibrary :: Context -> Rules ()
buildPackageGhciLibrary context@Context {..} = priority 2 $ do
    pkgId <- case pkgCabalFile package of
      Just file -> do
        cabal <- liftIO $ parseCabal file
        return $ if (null $ version cabal)
          then Cabal.name cabal
          else Cabal.name cabal ++ "-" ++ version cabal
      Nothing   -> return (pkgName package)

    let libPrefix = "//" ++ buildDir context -/- "HS" ++ pkgId
        o = libPrefix ++ "*" ++ (waySuffix way <.> "o")
    o %> \obj -> do
        objs <- allObjects context
        need objs
        build $ target context Ld objs [obj]

allObjects :: Context -> Action [FilePath]
allObjects context = (++) <$> nonHsObjects context <*> hsObjects context

nonHsObjects :: Context -> Action [FilePath]
nonHsObjects context = do
    path    <- contextPath context
    cObjs   <- cObjects context
    cmmSrcs <- pkgDataList (CmmSrcs path)
    cmmObjs <- mapM (objectPath context) cmmSrcs
    eObjs   <- extraObjects context
    return $ cObjs ++ cmmObjs ++ eObjs

cObjects :: Context -> Action [FilePath]
cObjects context = do
    path <- contextPath context
    srcs <- pkgDataList (CSrcs path)
    objs <- mapM (objectPath context) srcs
    return $ if way context == threaded
        then objs
        else filter ((`notElem` ["Evac_thr", "Scav_thr"]) . takeBaseName) objs

extraObjects :: Context -> Action [FilePath]
extraObjects context
    | package context == integerGmp = do
        gmpPath <- gmpBuildPath
        need [gmpPath -/- gmpLibraryH]
        map unifyPath <$> getDirectoryFiles "" [gmpPath -/- gmpObjectsDir -/- "*.o"]
    | otherwise         = return []
