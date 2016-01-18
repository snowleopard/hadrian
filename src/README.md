# Shaking up GHC

The purpose of this project is to create an alternate build system to the `make`
base build system currently used for the Glorious Glasgow Haskell Compiler. The
use of [shake](shakebuild.com) as a haskell based build system should come as no
surprise.

## Understanding the Build System

This document will try to outline the overall interaction of the build system.
Please report any deficiencies as issues, such that they can be addresses with
care.

As the build system is a haskell program in it's own right, the entry point is
`Main.hs`. Here we generate the *Rules* that govern the dependencies of the
build process. We also generate special *Rules*: the *Targets*. *Targets* as
special in that these are the rules we mark as **needed**. Thus the build system
tries to produce those *Target Rules*. Notably these include the final install
artifacts (`installTargets`), the targets for each stage as well as the
Run Time System Library (`rtsLib`). See `generateTargets` in `Rules.hs`.
`GHC.hs` defines all the packages that constitute GHC, and
`Settigns/Packages.hs` defines which packages are built in which stage.

##  Oracles, Rules, Action and Expressions

The build system makes use of three core ideas:

- Oracles

  Oracles as can be found in the same named folder, provide a facility to
  provide knowledge about the build system.  Each oracle file therefore contains
  a rule that generates that oracle.  The advantage of using oracles over
  looking up the information every time is that oracles are cached.

- Rules

  The basic building blocks of every build system are rules that describe how
  partial artifacts are constructed.  As some programs, libraries, and utilities
  need custom some custom rules to be build, these are in their respective files.

  There are two special set of rules, one is `Generators`, which are rules that
  are used to generate artifacts that are not the result of applying a generic
  rule to some input.

  The other special set of rules are `Wrappers`. Notably windows does not use
  wrappers, but on posix systems wrappers are used to inject default arguments
  or environment variables. As these are usually artifact specific, those
  are named after the artifact they wrap.

- Actions

  While rules govern the build system, they usually perform actions to turn the
  input into the output artifact, hence most of the build system are actually
  actions, that are run as the result of rules.

- Expressions

  As large parts of the build system deal with building a specific target, and
  carrying the target around explicitly, the shakeing-up-ghc build system makes
  use of Expressions, which represent computations that produce actions, and
  can enquire about the current target that is being build.

## Stages

GHC uses a staged build system. This is required as large parts of GHC are
written in Haskell itself (as the shake base build system). Thus we have to rely
on an external Haskell compiler (`bootstrap`) to build the first stage
(`stage1`) compiler and the accompanying libraries to build the second stage
(`stage2`), which is then a compiler compiled almost by itself.  This is also
the compiler that is usually the final build artifact that is installed.

For cross compilers we use the external `bootstrap` compiler that naturally
runs and targets the host platform and compile a `stage1` compiler that runs on
the host platform, but targets the cross compilation target. We can therefor
not produce a `stage2` cross compiler as that `stage1` compiler targets an
architecture that the host usually can not execute. As the `bootstrap` compiler
may not be the same version as the produced `stage1` compiler, for cross
compilation it may make sense to first produce a `stage2` compiler using a
legacy `bootstrap` compiler for the host platform and then use that compiler
to produce the `stage1` cross compiler.

To build a full `stage2` compiler on a different architecture, one can use the
`stage1` cross compiler as the `bootstrap` compiler.

## Packages

The Glorious Glasgow Haskell Compiler is comprised of a larger set of packages
that together form the final compiler, runtime and toolchain.

### Libraries

- **array** provides the `Data.Array` module.
- **base** contains the supporting libraries for the *Prelude*, also the `GHC` module.
- **binary** provides `Data.Binay` for serialization of Haskell values into `ByteString`s
- **bytestring** provides `Data.ByteString` module
- **cabal** provides primarily the `Distribution` module, which contains the cabal library.
- **compiler** contains the actual Glorious Glasgow Haskell Compiler library
- containers
  TBD
- deepseq
  TBD
- directory
  TBD
- filepath
  TBD
- ghc-boot
  TBD
- ghci
  TBD
- ghc-prim
  TBD
- haskeline
  TBD
- hoopl
  TBD
- hpc
  TBD
- integer-gmp
  TBD
- integer-simple
  TBD
- libffi
  TBD
- parallel
  TBD
- primitive
  TBD
- process
  TBD
- rts
  TBD
- stm
  TBD
- template-haskell
  TBD
- terminfo
  TBD
- time
  TBD
- transformers
  TBD
- unix
  TBD
- win32
  TBD
- xhtml
  TBD

### Programs

- ghc-bin
  TBD
- iserv-bin
  TBD

### Utilities

- compare-sizes
  TBD
- deriveConstants
  TBD
- dll-split
  TBD
- genapply
  TBD
- genprimopcode
  TBD
- ghc-cabal
  TBD
- ghc-pkg
  TBD
- ghctags
  TBD
- haddock
  TBD
- hsc2hs
  TBD
- hp2ps
  TBD
- hpc-bin
  TBD
- mkUserGuidePart
  TBD
- runghc
  TBD
- touchy
  TBD
- unlit
  TBD
- ghc-split
  TBD

## Builders
TBD

## The Glorious Glasgow Haskell Compiler (GHC)
TBD, not sure if we actually need this. This will just be a longer version of
the introduction.
