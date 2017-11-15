module Settings.Packages.Hsc2Hs (hsc2hsPackageArgs) where

import Expression
import GHC.Packages

hsc2hsPackageArgs :: Args
hsc2hsPackageArgs =
  package hsc2hs ? builder CabalFlags ? arg "in-ghc-tree"
