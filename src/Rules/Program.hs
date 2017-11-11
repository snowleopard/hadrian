module Rules.Program (buildProgram) where

import Hadrian.Haskell.Cabal
import Types.ConfiguredCabal as ConfCabal

import Base
import Context
import Expression hiding (stage, way)
import Oracles.ModuleFiles
import Oracles.Setting
import Rules.Wrappers
import Settings
import Settings.Packages.Rts
import Target
import Utilities

-- | TODO: Drop code duplication
buildProgram :: [(Resource, Int)] -> Package -> Rules ()
buildProgram rs package = do
    forM_ [Stage0 ..] $ \stage -> do
        let context = vanillaContext stage package

        -- Rules for programs built in 'buildRoot'
        "//" ++ stageString stage -/- "bin" -/- programName context <.> exe %> \bin -> do
            when (package == hsc2hs) $ do
              -- hsc2hs needs the template-hsc.h file
              tmpl <- templateHscPath stage
              need [tmpl]
            when (package == ghc) $ do
              -- ghc depends on settings, platformConstants, llvm-targets
              --     ghc-usage.txt, ghci-usage.txt
              need =<< ghcDeps stage
            buildBinary rs bin =<< programContext stage package

        -- Rules for the GHC package, which is built 'inplace'



    -- Rules for other programs built in inplace directories
    when (package /= ghc) $ do
        let context0 = vanillaContext Stage0 package -- TODO: get rid of context0
        inplaceBinPath -/- programName context0 <.> exe %> \bin -> do
            stage <- installStage package -- TODO: get rid of fromJust
            buildBinaryAndWrapper rs bin =<< programContext (fromJust stage) package

        inplaceLibBinPath -/- programName context0 <.> exe %> \bin -> do
            stage   <- installStage package -- TODO: get rid of fromJust
            context <- programContext (fromJust stage) package
            if package /= iservBin then
                -- We *normally* build only unwrapped binaries in inplace/lib/bin
                buildBinary rs bin context
            else
                -- Build both binary and wrapper in inplace/lib/bin for iservBin
                buildBinaryAndWrapperLib rs bin context

        inplaceLibBinPath -/- programName context0 <.> "bin" %> \bin -> do
            stage <- installStage package -- TODO: get rid of fromJust
            buildBinary rs bin =<< programContext (fromJust stage) package

buildBinaryAndWrapperLib :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinaryAndWrapperLib rs bin context = do
    windows <- windowsHost
    if windows
    then buildBinary rs bin context -- We don't build wrappers on Windows
    else case lookup context inplaceWrappers of
        Nothing      -> buildBinary rs bin context -- No wrapper found
        Just wrapper -> do
            top <- topDirectory
            let libdir = top -/- inplaceLibPath
            let wrappedBin = inplaceLibBinPath -/- programName context <.> "bin"
            need [wrappedBin]
            buildWrapper context wrapper bin (WrappedBinary libdir (takeFileName wrappedBin))

buildBinaryAndWrapper :: [(Resource, Int)] -> FilePath -> Context -> Action ()
buildBinaryAndWrapper rs bin context = do
    windows <- windowsHost
    if windows
    then buildBinary rs bin context -- We don't build wrappers on Windows
    else case lookup context inplaceWrappers of
        Nothing      -> buildBinary rs bin context -- No wrapper found
        Just wrapper -> do
            top <- topDirectory
            let libPath    = top -/- inplaceLibPath
                wrappedBin = inplaceLibBinPath -/- takeFileName bin
            need [wrappedBin]
            buildWrapper context wrapper bin (WrappedBinary libPath (takeFileName bin))

buildWrapper :: Context -> Wrapper -> FilePath -> WrappedBinary -> Action ()
buildWrapper context@Context {..} wrapper wrapperPath wrapped = do
    contents <- interpretInContext context $ wrapper wrapped
    writeFileChanged wrapperPath contents
    makeExecutable wrapperPath
    putSuccess $ "| Successfully created wrapper for " ++
        quote (pkgName package) ++ " (" ++ show stage ++ ")."

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
            path   <- contextPath context
            cSrcs  <- interpretInContext context (getConfiguredCabalData ConfCabal.cSrcs)
            cObjs  <- mapM (objectPath context) cSrcs
            hsObjs <- hsObjects context
            return $ cObjs ++ hsObjs
                  ++ [ path -/- "build" -/- "Paths_hsc2hs.o"  | package == hsc2hs  ]
                  ++ [ path -/- "build" -/- "Paths_haddock.o" | package == haddock ]
    need binDeps
    buildWithResources rs $ target context (Ghc LinkHs stage) binDeps [bin]
    synopsis <- pkgSynopsis context
    putSuccess $ renderProgram
        (quote (pkgName package) ++ " (" ++ show stage ++ ").") bin synopsis
