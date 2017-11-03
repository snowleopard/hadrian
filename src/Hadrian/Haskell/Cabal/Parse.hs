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
module Hadrian.Haskell.Cabal.Parse ( Cabal (..)
                                   , parseCabal, parseCabalPkgId
                                   , cabalCcArgs, cabalIncludeDirs, cabalCSrcs
                                   , cabalModules, cabalOtherModules
                                   , cabalSrcDirs, cabalCmmSrcs
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
import qualified Distribution.Verbosity                as C
import qualified Distribution.Simple.GHC               as GHC
import qualified Distribution.Simple.Program.Db        as Db
import qualified Distribution.Simple                   as Hooks (simpleUserHooks, autoconfUserHooks)
import qualified Distribution.Simple.UserHooks         as Hooks
import Distribution.Text (display)
import Distribution.Simple (defaultMainWithHooksNoReadArgs)
import Distribution.Simple.Compiler (compilerInfo)
import Hadrian.Package
import Hadrian.Utilities
import System.FilePath
import System.Directory
import GHC.Generics
import qualified Distribution.ModuleName as ModuleName
import Data.Maybe (maybeToList)
import GHC.Packages (rts)

import Context.Paths

-- TODO: Use fine-grain tracking instead of tracking the whole Cabal file.
-- | Haskell package metadata extracted from a Cabal file.
data Cabal = Cabal
    { dependencies :: [PackageName]
    , name         :: PackageName
    , synopsis     :: String
    , version      :: String
    , packageDesc  :: C.PackageDescription
    } deriving (Eq, Read, Show, Typeable, Generic)

instance Binary Cabal

instance Hashable Cabal where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData Cabal where
    rnf (Cabal a b c d e) = a `seq` b `seq` c `seq` d `seq` e `seq` ()

parseCabalPkgId :: FilePath -> IO String
parseCabalPkgId file = C.display . C.package . C.packageDescription <$> C.readGenericPackageDescription C.silent file


biModules :: Cabal -> (C.BuildInfo, [ModuleName.ModuleName])
biModules c = go [ comp | comp@(bi,_) <- (map libBiModules . maybeToList $ C.library (packageDesc c))
                                         ++ (map exeBiModules $ C.executables (packageDesc c))
                        , C.buildable bi ]
  where libBiModules lib = (C.libBuildInfo lib, C.libModules lib)
        exeBiModules exe = (C.buildInfo exe, ModuleName.main : C.exeModules exe)
        go [] = error "no buildable component found"
        go [x] = x
        go _  = error "can not handle more than one buildinfo yet!"

cabalCcArgs :: Cabal -> [String]
cabalCcArgs c = C.ccOptions (fst (biModules c))
cabalCSrcs :: Cabal -> [String]
cabalCSrcs c = C.cSources (fst (biModules c))
cabalCmmSrcs :: Cabal -> [String]
cabalCmmSrcs c = C.cmmSources (fst (biModules c))
cabalIncludeDirs :: Cabal -> [String]
cabalIncludeDirs c = C.includeDirs (fst (biModules c))
cabalModules :: Cabal -> [String]
cabalModules c = map display $ snd (biModules c)

cabalSrcDirs :: Cabal -> [String]
cabalSrcDirs c = C.hsSourceDirs $ fst (biModules c)

cabalOtherModules :: Cabal -> [String]
cabalOtherModules c = map display $ C.otherModules (fst (biModules c))
--cabalDepIncludeDirs


-- TODO: Taken from Context, but Context depends on Oracles.Settings, and this
--       would then lead to recursive imports.
contextPath :: Context -> Action FilePath
contextPath context = buildRoot <&> (-/- contextDir context)

buildDir :: Context -> FilePath
buildDir context = contextDir context -/- "build"

-- | Parse a Cabal file.
parseCabal :: Context -> Action Cabal
parseCabal context@Context {..} = do
    let (Just file) = pkgCabalFile package

    -- read the package description from the cabal file
    gpd <- liftIO $ C.readGenericPackageDescription C.silent file

    -- figure out what hooks we need.
    hooks <- case C.buildType (C.flattenPackageDescription gpd) of
          Just C.Configure -> pure Hooks.autoconfUserHooks
          -- time has a "Custom" Setup.hs, but it's actually Configure
          -- plus a "./Setup test" hook. However, Cabal is also
          -- "Custom", but doesn't have a configure script.
          Just C.Custom ->
              do configureExists <- liftIO $ doesFileExist (replaceFileName file "configure")
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

    bPath <- buildPath context
    liftIO $
      withCurrentDirectory (pkgPath package) $
        defaultMainWithHooksNoReadArgs hooks gpd ["configure", "--distdir", bPath, "--ipid", "$pkg-$version"]

    hcPath <- builderPath' (Ghc CompileHs stage)
    (compiler, Just platform, _pgdb) <- liftIO $ GHC.configure C.silent (Just hcPath) Nothing Db.emptyProgramDb

    let (Right (pd,_)) = C.finalizePackageDescription mempty (const True) platform (compilerInfo compiler) [] gpd
    let -- pd      = C.packageDescription gpd
        pkgId   = C.package pd
        name    = C.unPackageName (C.pkgName pkgId)
        version = C.display (C.pkgVersion pkgId)
        libDeps = collectDeps (C.condLibrary gpd)
        exeDeps = map (collectDeps . Just . snd) (C.condExecutables gpd)
        allDeps = concat (libDeps : exeDeps)
        sorted  = sort [ C.unPackageName p | C.Dependency p _ <- allDeps ]
        deps    = nubOrd sorted \\ [name]
    return $ Cabal deps name (C.synopsis pd) version pd

collectDeps :: Maybe (C.CondTree v [C.Dependency] a) -> [C.Dependency]
collectDeps Nothing = []
collectDeps (Just (C.CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (C.CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
