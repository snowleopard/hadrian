{-# LANGUAGE OverloadedStrings #-}
module GHC (
    array, base, binary, bytestring, cabal, compiler, containers, compareSizes,
    deepseq, deriveConstants, directory, dllSplit, filepath, genapply,
    genprimopcode, ghc, ghcBoot, ghci, ghcPkg, ghcPrim, ghcTags, ghcSplit,
    haddock, haskeline, hsc2hs, hoopl, hp2ps, hpc, hpcBin, integerGmp,
    integerSimple, iservBin, libffi, mkUserGuidePart, parallel, pretty,
    primitive, process, rts, runGhc, stm, templateHaskell, terminfo, time,
    touchy, transformers, unlit, unix, win32, xhtml,

    defaultKnownPackages, programPath, targetDirectory
    ) where

import Base
import Package
import Stage

-- | These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package /win32/ is built only on Windows.
-- "Packages" defines default conditions for building each package, which can
-- be overridden in "User".
defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binary, bytestring, cabal, compiler, containers, compareSizes
    , deepseq, deriveConstants, directory, dllSplit, filepath, genapply
    , genprimopcode, ghc, ghcBoot, ghci, ghcPkg, ghcPrim, ghcTags, haddock
    , haskeline, hsc2hs, hoopl, hp2ps, hpc, hpcBin, integerGmp, integerSimple
    , iservBin, libffi, mkUserGuidePart, parallel, pretty, primitive, process
    , rts, runGhc, stm, templateHaskell, terminfo, time, touchy, transformers
    , unlit, unix, win32, xhtml ]

-- Package definitions (see "Package")
array, base, binary, bytestring, cabal, compiler, containers, compareSizes,
    deepseq, deriveConstants, directory, dllSplit, filepath, genapply,
    genprimopcode, ghc, ghcBoot, ghci, ghcPkg, ghcPrim, ghcTags, haddock,
    haskeline, hsc2hs, hoopl, hp2ps, hpc, hpcBin, integerGmp, integerSimple,
    iservBin, libffi, mkUserGuidePart, parallel, pretty, primitive, process,
    rts, runGhc, stm, templateHaskell, terminfo, time, touchy, transformers,
    unlit, unix, win32, xhtml :: Package

array           = library  "array"
base            = library  "base"
binary          = library  "binary"
bytestring      = library  "bytestring"
cabal           = library  "Cabal"          `setPath` "libraries/Cabal/Cabal"
compiler        = topLevel "ghc"            `setPath` "compiler"
containers      = library  "containers"
compareSizes    = utility  "compareSizes"   `setPath` "utils/compare_sizes"
deepseq         = library  "deepseq"
deriveConstants = utility  "deriveConstants"
directory       = library  "directory"
dllSplit        = utility  "dll-split"
filepath        = library  "filepath"
genapply        = utility  "genapply"
genprimopcode   = utility  "genprimopcode"
ghc             = topLevel "ghc-bin"        `setPath` "ghc"   `setType` Program
ghcBoot         = library  "ghc-boot"
ghci            = library  "ghci"
ghcPkg          = utility  "ghc-pkg"
ghcPrim         = library  "ghc-prim"
ghcTags         = utility  "ghctags"
haddock         = utility  "haddock"
haskeline       = library  "haskeline"
hsc2hs          = utility  "hsc2hs"
hoopl           = library  "hoopl"
hp2ps           = utility  "hp2ps"
hpc             = library  "hpc"
hpcBin          = utility  "hpc-bin"        `setPath` "utils/hpc"
integerGmp      = library  "integer-gmp"
integerSimple   = library  "integer-simple"
iservBin        = topLevel "iserv-bin"      `setPath` "iserv" `setType` Program
libffi          = topLevel "libffi"
mkUserGuidePart = utility  "mkUserGuidePart"
parallel        = library  "parallel"
pretty          = library  "pretty"
primitive       = library  "primitive"
process         = library  "process"
rts             = topLevel "rts"
runGhc          = utility  "runghc"
stm             = library  "stm"
templateHaskell = library  "template-haskell"
terminfo        = library  "terminfo"
time            = library  "time"
touchy          = utility  "touchy"
transformers    = library  "transformers"
unlit           = utility  "unlit"
unix            = library  "unix"
win32           = library  "Win32"
xhtml           = library  "xhtml"

-- | ghc-split is a perl script used by GHC with @-split-objs@ flag. It is
-- generated in "Rules.Generators.GhcSplit".
ghcSplit :: FilePath
ghcSplit = "inplace/lib/bin/ghc-split"

-- TODO: The following utils are not included into the build system because
-- they seem to be unused or unrelated to the build process: checkUniques,
-- completion, count_lines, coverity, debugNGC, describe-unexpected, genargs,
-- lndir, mkdirhier, testremove, vagrant

-- TODO: move to buildRootPath, see #113
-- TODO: simplify, add programInplaceLibPath
-- | The relative path to the program executable
programPath :: Stage -> Package -> Maybe FilePath
programPath stage pkg
    | pkg == ghc = Just . inplaceProgram $ "ghc-stage" ++ show (fromEnum stage + 1)
    | pkg == haddock || pkg == ghcTags = case stage of
        Stage2 -> Just . inplaceProgram $ pkgNameString pkg
        _      -> Nothing
    | pkg `elem` [touchy, unlit] = case stage of
        Stage0 -> Just $ "inplace/lib/bin" -/- pkgNameString pkg <.> exe
        _      -> Nothing
    | isProgram pkg = case stage of
        Stage0 -> Just . inplaceProgram $ pkgNameString pkg
        _      -> Just . installProgram $ pkgNameString pkg
    | otherwise = Nothing
  where
    inplaceProgram name = programInplacePath -/- name <.> exe
    installProgram name = pkgPath pkg -/- targetDirectory stage pkg
                                      -/- "build/tmp" -/- name <.> exe

-- | GHC build results will be placed into target directories with the
-- following typical structure:

-- * @build/@ contains compiled object code
-- * @doc/@ is produced by haddock
-- * @package-data.mk@ contains output of ghc-cabal applied to pkgCabal
targetDirectory :: Stage -> Package -> FilePath
targetDirectory stage _ = stageString stage

