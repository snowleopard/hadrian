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
  TBD
- Rules
  TBD
- Actions
  TBD
- Expressions
  TBD


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
TBD
### Libraries
TBD
### Programs
TBD

## Builders
TBD

## The Glorious Glasgow Haskell Compiler (GHC)
TBD, not sure if we actually need this. This will just be a longer version of
the introduction.
