module Settings.Packages.Haddock (haddockPackageArgs) where

import Expression
import GHC.Packages

haddockPackageArgs :: Args
haddockPackageArgs =
  package haddock ? builder CabalFlags ? arg "in-ghc-tree"
