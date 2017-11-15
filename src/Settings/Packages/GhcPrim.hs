module Settings.Packages.GhcPrim (ghcPrimPackageArgs) where

import Oracles.Flag
import Expression
import GHC.Packages

ghcPrimPackageArgs :: Args
ghcPrimPackageArgs = package ghcPrim ? mconcat
    [ builder CabalFlags ? arg "include-ghc-prim"

    , builder (Cc CompileC)     ?
      (not <$> flag GccIsClang) ?
      input "//cbits/atomic.c"  ? arg "-Wno-sync-nand" ]
