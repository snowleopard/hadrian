module Builder where

import Stage
import Hadrian.Builder.Types
import Development.Shake

data CcMode = CompileC | FindCDependencies
data GhcMode =  CompileHs | CompileCWithGhc | FindHsDependencies | LinkHs
data GhcCabalMode = Conf | Copy | Reg | HsColour | Check | Sdist
data GhcPkgMode = Init | Update | Clone | Dependencies
data HaddockMode = BuildPackage | BuildIndex
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
             | CabalFlags Stage

instance Eq Builder
instance Show Builder

builderPath' :: Builder -> Action FilePath
