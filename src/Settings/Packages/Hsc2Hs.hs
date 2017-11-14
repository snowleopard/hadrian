module Settings.Packages.Hsc2Hs (hsc2hsPackageArgs) where

import Expression

hsc2hsPackageArgs :: Args
hsc2hsPackageArgs =
  package hsc2hs ? builder CabalFlags ? arg "in-ghc-tree"
