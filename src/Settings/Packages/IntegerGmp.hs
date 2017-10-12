module Settings.Packages.IntegerGmp (integerGmpPackageArgs) where

import Base
import Expression
import Oracles.Setting
import Rules.Gmp

-- TODO: Is this needed?
-- ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
-- libraries/integer-gmp_CONFIGURE_OPTS += --with-gmp-framework-preferred
-- endif
integerGmpPackageArgs :: Args
integerGmpPackageArgs = package integerGmp ? do
    path <- expr gmpBuildPath
    topDir <- expr topDirectory
    let includeGmp = "-I" ++ path -/- "include"
    gmpIncludeDir <- getSetting GmpIncludeDir
    gmpLibDir     <- getSetting GmpLibDir
    let -- are we building an in-tree GMP?
        inTreeGmp = null gmpIncludeDir && null gmpLibDir
    mconcat [ builder Cc ? arg includeGmp

            , builder GhcCabal ? mconcat
              [ inTreeGmp ?
                arg "--configure-option=--with-intree-gmp"
              , arg ("--configure-option=CFLAGS=" ++ includeGmp)
              , arg ("--extra-include-dirs="++ topDir -/- path -/- "include")
              ] ]
