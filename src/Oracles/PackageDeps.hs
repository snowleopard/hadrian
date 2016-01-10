{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.PackageDeps  where

import Base
import qualified Data.HashMap.Strict as Map
import Package

newtype PackageDepsKey = PackageDepsKey Package.PackageName
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- packageDeps name is an action that given a package looks up its dependencies
-- in Base.packageDependencies file. The dependencies need to be computed by
-- scanning package cabal files (see Rules.Cabal).
packageDeps :: Package.Package -> Action [Package.PackageName]
packageDeps pkg = do
    res <- askOracle . PackageDepsKey . Package.pkgName $ pkg
    return . fromMaybe [] $ res

-- Oracle for the package dependencies file
packageDepsOracle :: Rules ()
packageDepsOracle = do
    deps <- newCache $ \_ -> do
        putOracle $ "Reading package dependencies..."
        contents <- readFileLines packageDependencies
        return . Map.fromList
               $ [ (head ps, tail ps) | line <- contents, let ps = map Package.PackageName $ words line ]
    _ <- addOracle $ \(PackageDepsKey pkg) -> Map.lookup pkg <$> deps ()
    return ()

