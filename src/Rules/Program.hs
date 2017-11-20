module Rules.Program (buildProgram) where

import Hadrian.Haskell.Cabal
import Types.ConfiguredCabal as ConfCabal

import Base
import Context
import Expression hiding (stage, way)
import Oracles.ModuleFiles
--import Oracles.Setting
import Oracles.Flag (crossCompiling)
--import Rules.Wrappers
import Settings
import Settings.Packages.Rts
import Target
import Utilities
import GHC.Packages
import GHC

-- | TODO: Drop code duplication
buildProgram :: [(Resource, Int)] -> Rules ()
buildProgram rs = do
    root <- buildRootRules
    forM_ [Stage0 ..] $ \stage ->
      root -/- stageString stage -/- "bin" -/- "*" %> \bin -> do

          -- quite inefficient. But we can't access the programName from
          -- Rules, as it's an Action, due to being backed by an Oracle.
          activeProgramPackages <- filter isProgram <$> stagePackages stage
          nameToCtxList <- forM activeProgramPackages $ \pkg -> do
            let ctx = vanillaContext stage pkg
            name <- programName ctx
            return (name <.> exe, ctx)

          case lookup (takeFileName bin) nameToCtxList of
            Nothing -> fail "Unknown program"
            Just (Context {..}) -> do
              -- Rules for programs built in 'buildRoot'

              -- Custom dependencies: this should be modeled better in the cabal file somehow.

              when (package == hsc2hs) $ do
                -- hsc2hs needs the template-hsc.h file
                tmpl <- templateHscPath stage
                need [tmpl]
              when (package == ghc) $ do
                -- ghc depends on settings, platformConstants, llvm-targets
                --     ghc-usage.txt, ghci-usage.txt
                need =<< ghcDeps stage

              cross <- crossCompiling
              -- for cross compiler. copy the stage0/bin/<pgm>
              -- into stage1/bin/
              case (package, cross, stage) of
                (p, True, s) | s > Stage0 && p `elem` [ghc, ghcPkg, hsc2hs] -> do
                                 srcDir <- buildRoot <&> (-/- (stageString Stage0 -/- "bin"))
                                 copyFile (srcDir -/- takeFileName bin) bin
                _ -> buildBinary rs bin =<< programContext stage package
          -- Rules for the GHC package, which is built 'inplace'

-- TODO: Get rid of the Paths_hsc2hs.o hack.
buildBinary :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinary rs bin context@Context {..} = do
    binDeps <- if stage == Stage0 && package == ghcCabal
        then hsSources context
        -- then do -- this is a hack, but he ghc-cabal packge only list's it's Main
        --         -- it does however depend on the Lexer in lib:Cabal, and the
        --         -- cbits file in libraries/text.

        --         -- it also depends on essnetially the content of all the following
        --         -- libraries: Cabal/Cabal, binary, filepath, hpc, mtl, text, parsec
        --         --
        --         -- We can not use the hsSource or other queries on those pacakges as
        --         -- they require the package-data.mk, which in turn requires ghc-cabal.
        --         --
        --         -- As such, we will ignore this for now, even though it will mean
        --         -- that hadrian will not properly track the dependencies of
        --         -- ghc-cabal properly.

        --      ghcCabalPath <- contextPath (context { Context.package = ghcCabal })
        --      cabalPath    <- contextPath (context { Context.package = cabal    })
        --      textPath     <- contextPath (context { Context.package = text     })
        --      return $ [ ghcCabalPath -/- "build" -/- "Main.o"
        --               , cabalPath -/- "build" -/- "Cabal/Distribution/Parsec/Lexer.o"
        --               , textPath -/- "build" -/- "c/cbits/cbits.o"
        --               ]
        else do
            needLibrary =<< contextDependencies context
            when (stage > Stage0) $ do
                ways <- interpretInContext context (getLibraryWays <> getRtsWays)
                needLibrary [ rtsContext { way = w } | w <- ways ]
            cSrcs  <- interpretInContext context (getConfiguredCabalData ConfCabal.cSrcs)
            cObjs  <- mapM (objectPath context) cSrcs
            hsObjs <- hsObjects context
            return $ cObjs ++ hsObjs
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- pkgSynopsis context
    putSuccess $ renderProgram
        (quote (pkgName package) ++ " (" ++ show stage ++ ").") bin synopsis
