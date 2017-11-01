module Builder where

import Stage
import Hadrian.Builder.Ar
import Development.Shake

data CcMode = CompileC | FindCDependencies
data GhcMode = Settings | CompileHs | CompileCWithGhc | FindHsDependencies | LinkHs
data GhcCabalMode = Conf | Copy | Reg | HsColour | Check | Sdist
data GhcPkgMode = Init | Update | Clone | Dependencies
data HaddockMode = BuildPackage | BuildIndex
data SphinxMode = Html | Latex | Man
data TarMode = Create | Extract
data Builder = Alex
             | Ar ArMode Stage
             | DeriveConstants
             | Cc CcMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcCabal GhcCabalMode Stage
             | GhcPkg GhcPkgMode Stage
             | Haddock HaddockMode
             | Happy
             | Hpc
             | HsCpp
             | Hsc2Hs Stage
             | Ld
             | Make FilePath
             | Nm
             | Objdump
             | Patch
             | Perl
             | Ranlib
             | Sphinx SphinxMode
             | Tar TarMode
             | Unlit
             | Xelatex

builderPath' :: Builder -> Action FilePath