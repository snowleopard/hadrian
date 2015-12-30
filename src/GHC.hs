{-# LANGUAGE OverloadedStrings #-}
module GHC (
    array, base, binary, bytestring, cabal, compiler, containers, compareSizes,
    deepseq, deriveConstants, directory, dllSplit, filepath, genapply,
    genprimopcode, ghc, ghcBoot, ghcCabal, ghci, ghcPkg, ghcPrim, ghcPwd, ghcTags,
    haddock, haskeline, hsc2hs, hoopl, hp2ps, hpc, hpcBin, integerGmp,
    integerSimple, iservBin, mkUserGuidePart, parallel, pretty, primitive, process,
    runGhc, stm, templateHaskell, terminfo, time, transformers, unix, win32, xhtml,

    defaultKnownPackages, defaultTargetDirectory, defaultProgramPath
    ) where

import Base
import Package
import Stage

-- These are all GHC packages we know about. Build rules will be generated for
-- all of them. However, not all of these packages will be built. For example,
-- package 'win32' is built only on Windows.
-- Settings/Packages.hs defines default conditions for building each package,
-- which can be overridden in Settings/User.hs.
defaultKnownPackages :: [Package]
defaultKnownPackages =
    [ array, base, binary, bytestring, cabal, compiler, containers, compareSizes
    , deepseq, deriveConstants, directory, dllSplit, filepath, genapply
    , genprimopcode, ghc, ghcBoot, ghcCabal, ghci, ghcPkg, ghcPrim
    , ghcPwd, ghcTags, haddock, haskeline, hsc2hs, hoopl, hp2ps, hpc, hpcBin
    , integerGmp, integerSimple, iservBin, mkUserGuidePart, parallel, pretty
    , primitive , process, runGhc, stm, templateHaskell, terminfo, time
    , transformers, unix, win32, xhtml ]

-- Package definitions (see Package.hs)
array, base, binary, bytestring, cabal, compiler, containers, compareSizes,
    deepseq, deriveConstants, directory, dllSplit, filepath, genapply,
    genprimopcode, ghc, ghcBoot, ghcCabal, ghci, ghcPkg, ghcPrim, ghcPwd,
    ghcTags, haddock, haskeline, hsc2hs, hoopl, hp2ps, hpc, hpcBin, integerGmp,
    integerSimple, iservBin, mkUserGuidePart, parallel, pretty, primitive, process,
    runGhc, stm, templateHaskell, terminfo, time, transformers, unix, win32, xhtml :: Package

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
ghc             = topLevel "ghc-bin"        `setPath` "ghc"   `setType` Program `setWrapper` "dummy"
ghcBoot         = library  "ghc-boot"
ghcCabal        = utility  "ghc-cabal"
ghci            = library  "ghci"
ghcPkg          = utility  "ghc-pkg"
ghcPrim         = library  "ghc-prim"
ghcPwd          = utility  "ghc-pwd"
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
mkUserGuidePart = utility  "mkUserGuidePart"
parallel        = library  "parallel"
pretty          = library  "pretty"
primitive       = library  "primitive"
process         = library  "process"
runGhc          = utility  "runGhc"
stm             = library  "stm"
templateHaskell = library  "template-haskell"
terminfo        = library  "terminfo"
time            = library  "time"
transformers    = library  "transformers"
unix            = library  "unix"
win32           = library  "Win32"
xhtml           = library  "xhtml"

-- TODO: The following utils are not implemented yet: unlit, driver/ghc-split
-- TODO: The following utils are not included into the build system because
-- they seem to be unused or unrelated to the build process: checkUniques,
-- completion, count_lines, coverity, debugNGC, describe-unexpected, genargs,
-- lndir, mkdirhier, testremove, touchy, vagrant

-- GHC build results will be placed into target directories with the following
-- typical structure:
-- * build/          : contains compiled object code
-- * doc/            : produced by haddock
-- * package-data.mk : contains output of ghc-cabal applied to pkgCabal
defaultTargetDirectory :: Stage -> Package -> FilePath
defaultTargetDirectory stage _ = stageString stage

-- TODO: simplify
-- | Returns a relative path to the program executable
defaultProgramPath :: Stage -> Package -> Maybe FilePath
defaultProgramPath stage pkg
    | pkg == ghc = Just . inplaceProgram $ "ghc-stage" ++ show (fromEnum stage + 1)
    | pkg == haddock || pkg == ghcTags = case stage of
        Stage2 -> Just . inplaceProgram $ pkgNameString pkg
        _      -> Nothing
    | isProgram pkg = case stage of
        Stage0 -> Just . inplaceProgram $ pkgNameString pkg
        _      -> Just . installProgram $ pkgNameString pkg
    | otherwise = Nothing
  where
    inplaceProgram name = programInplacePath -/- name <.> exe
    installProgram name = pkgPath pkg -/- defaultTargetDirectory stage pkg
                                      -/- "build/tmp" -/- name <.> exe
