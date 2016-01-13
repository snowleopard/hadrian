module Rules.Package (buildPackage) where

import Base
import Rules.Compile
import Rules.Dependencies
import Rules.Documentation
import Rules.Generate
import Rules.Library
import Rules.Program
import Rules.Resources
import Target

buildPackage :: Resources -> PartialTarget -> Rules ()
buildPackage = mconcat
    [ buildPackageDependencies
    , generatePackageCode
    , compilePackage
    , buildPackageLibrary
    , buildPackageDocumentation
    , buildProgram ]
