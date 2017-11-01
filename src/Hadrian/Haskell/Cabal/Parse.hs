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
module Hadrian.Haskell.Cabal.Parse (Cabal (..), parseCabal, parseCabalPkgId, cabalCcArgs, cabalIncludeDirs) where

import Stage
import {-# SOURCE #-} Builder hiding (Builder)
-- import Hadrian.Builder as H
import Data.List.Extra
import Development.Shake
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
import Distribution.Simple.Compiler (compilerInfo)
import Hadrian.Package
import GHC.Generics

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

cabalCcArgs :: Cabal -> [String]
cabalCcArgs c = concatMap C.ccOptions [ C.libBuildInfo lib | (Just lib) <- [C.library (packageDesc c)]
                                                           , C.buildable . C.libBuildInfo $ lib ]


cabalIncludeDirs :: Cabal -> [String]
cabalIncludeDirs c = concatMap C.includeDirs [ C.libBuildInfo lib | (Just lib) <- [C.library (packageDesc c)]
                                                                  , C.buildable . C.libBuildInfo $ lib ]

--cabalDepIncludeDirs

-- | Parse a Cabal file.
parseCabal :: Stage -> FilePath -> Action Cabal
parseCabal stage file = do
    hcPath <- builderPath' (Ghc CompileHs stage)
    (compiler, Just platform, _pgdb) <- liftIO $ GHC.configure C.silent (Just hcPath) Nothing Db.emptyProgramDb
    gpd <- liftIO $ C.readGenericPackageDescription C.silent file
    let (Right (pd,_)) = C.finalizePackageDescription [] (const True) platform (compilerInfo compiler) [] gpd
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
