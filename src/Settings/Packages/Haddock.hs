module Settings.Packages.Haddock (haddockPackageArgs) where

import Expression

haddockPackageArgs :: Args
haddockPackageArgs =
  package haddock ? builder CabalFlags ? arg "in-ghc-tree"
