module Rules.Program (buildProgram) where

import Data.Char

import Base
import Expression
import GHC hiding (ghci)
import Oracles
import Rules.Actions
import Rules.Library
import Rules.Resources
import Settings
import Settings.Builders.GhcCabal

-- TODO: Get rid of the Paths_hsc2hs.o hack.
-- TODO: Do we need to consider other ways when building programs?
buildProgram :: Resources -> PartialTarget -> Rules ()
buildProgram _ target @ (PartialTarget stage pkg) = do
    let path       = targetPath stage pkg
        buildPath  = path -/- "build"
        match file = case programPath stage pkg of
            Nothing      -> False
            Just prgPath -> ("//" ++ prgPath) ?== file
        matchWrapper file = case defaultWrapperPath stage pkg of
            Nothing      -> False
            Just wrpPath -> ("//" ++ wrpPath) ?== file
    matchWrapper ?> \bin -> do
        let Just wrappedProgram = programPath stage pkg
        need $ [wrappedProgram]
        let wrapper = unlines [ "#!/bin/bash"
                              , "exec " ++ wrappedProgram ++ " ${1+\"$@\"}"
                              ]
        writeFileChanged bin wrapper
        () <- cmd "chmod +x " [bin]
        putSuccess $ renderBox
            [ "Successfully created wrapper '"
              ++ pkgNameString pkg ++ "' (" ++ show stage ++ ")."
            , "Executable: " ++ bin
            ]

    match ?> \bin -> do
        cSrcs <- cSources target -- TODO: remove code duplication (Library.hs)
        hSrcs <- hSources target
        let cObjs = [ buildPath -/- src -<.> osuf vanilla | src <- cSrcs   ]
            hObjs = [ buildPath -/- src  <.> osuf vanilla | src <- hSrcs   ]
                 ++ [ buildPath -/- "Paths_hsc2hs.o"      | pkg == hsc2hs  ]
                 ++ [ buildPath -/- "Paths_haddock.o"     | pkg == haddock ]
            objs  = cObjs ++ hObjs
        ways     <- interpretPartial target getWays
        depNames <- interpretPartial target $ getPkgDataList TransitiveDepNames
        let libStage  = min stage Stage1 -- libraries are built only in Stage0/1
            libTarget = PartialTarget libStage pkg
        pkgs     <- interpretPartial libTarget getPackages
        ghciFlag <- interpretPartial libTarget $ getPkgData BuildGhciLib
        let deps = matchPackageNames (sort pkgs) (map PackageName $ sort depNames)
            ghci = ghciFlag == "YES" && stage == Stage1
        libs <- fmap concat . forM deps $ \dep -> do
            let depTarget = PartialTarget libStage dep
            compId <- interpretPartial depTarget $ getPkgData ComponentId
            libFiles <- fmap concat . forM ways $ \way -> do
                libFile  <- pkgLibraryFile libStage dep compId           way
                lib0File <- pkgLibraryFile libStage dep (compId ++ "-0") way
                dll0     <- needDll0 libStage dep
                return $ [ libFile ] ++ [ lib0File | dll0 ]
            return $ libFiles ++ [ pkgGhciLibraryFile libStage dep compId | ghci ]
        let binDeps = if pkg == ghcCabal && stage == Stage0
                      then [ pkgPath pkg -/- src <.> "hs" | src <- hSrcs ]
                      else objs
        need $ binDeps ++ libs
        build $ fullTargetWithWay target (Ghc stage) vanilla binDeps [bin]
        synopsis <- interpretPartial target $ getPkgData Synopsis
        putSuccess $ renderBox
            [ "Successfully built program '"
              ++ pkgNameString pkg ++ "' (" ++ show stage ++ ")."
            , "Executable: " ++ bin
            , "Package synopsis: " ++ dropWhileEnd isPunctuation synopsis ++ "." ]
