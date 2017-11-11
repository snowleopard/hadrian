module Settings.Packages.IntegerGmp (integerGmpPackageArgs) where

import Base
import Expression
import Rules.Gmp

-- TODO: Is this needed?
-- ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
-- libraries/integer-gmp_CONFIGURE_OPTS += --with-gmp-framework-preferred
-- endif
integerGmpPackageArgs :: Args
integerGmpPackageArgs = package integerGmp ? do
    path <- expr gmpBuildPath
    let includeGmp = "-I" ++ path -/- "include"
    mconcat [ builder Cc ? arg includeGmp

            , builder (GhcCabal Conf) ? mconcat
              [ -- (null gmpIncludeDir && null gmpLibDir) ? -- XXX: this should respect some settings flag "InTreeGmp".
                                                            --      depending on include and lib dir, is bound to fail
                                                            --      these are only set if ./configure was explicilty
                                                            --      called with gmp include and lib dirs.  Their absense
                                                            --      as such does not imply in-tree-gmp
                -- arg "--configure-option=--with-intree-gmp"
                arg ("--configure-option=CFLAGS=" ++ includeGmp)
              , arg ("--gcc-options="             ++ includeGmp) ] ]
