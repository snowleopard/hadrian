module Settings.Packages.Rts (
    rtsContext, rtsBuildPath, rtsConfIn, rtsPackageArgs, rtsLibffiLibrary
    ) where

import Base
import Expression
import Oracles.Flag
import Oracles.Setting
import Settings

-- | RTS is considered a Stage1 package. This determines RTS build directory.
rtsContext :: Context
rtsContext = vanillaContext Stage1 rts

-- | Path to the RTS build directory.
rtsBuildPath :: Action FilePath
rtsBuildPath = buildPath rtsContext

-- | Path to RTS package configuration file, to be processed by HsCpp.
rtsConfIn :: FilePath
rtsConfIn = pkgPath rts -/- "package.conf.in"

-- | Minimum supported Windows version.
-- These numbers can be found at:
-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa383745(v=vs.85).aspx
-- If we're compiling on windows, enforce that we only support Vista SP1+
-- Adding this here means it doesn't have to be done in individual .c files
-- and also centralizes the versioning.
rtsWindowsVersion :: String
rtsWindowsVersion = "0x06000100"

rtsLibffiLibraryName :: Action FilePath
rtsLibffiLibraryName = do
    useSystemFfi <- flag UseSystemFfi
    windows      <- windowsHost
    return $ case (useSystemFfi, windows) of
        (True , False) -> "ffi"
        (False, False) -> "Cffi"
        (_    , True ) -> "Cffi-6"

rtsLibffiIncludeArgs :: Args
rtsLibffiIncludeArgs = package libffi ? builder (Ghc CompileCWithGhc) ? do
  useSystemFfi <- expr $ flag UseSystemFfi
  ffiIncludeDir <- getSetting FfiIncludeDir
  mconcat [
    useSystemFfi ? pure (map ("-I" ++) $ words ffiIncludeDir),
    -- ffi.h triggers prototype warnings, so disable them here:
    inputs [ "//Interpreter.c", "//Storage.c", "//Adjustor.c" ] ?
    arg "-Wno-strict-prototypes" ]

rtsLibffiLibrary :: Way -> Action FilePath
rtsLibffiLibrary way = do
    name    <- rtsLibffiLibraryName
    suf     <- libsuf way
    rtsPath <- rtsBuildPath
    return $ rtsPath -/- "lib" ++ name ++ suf

-- ref: mk/config.mk.in
ghcRtsWithLibDw :: Action Bool
ghcRtsWithLibDw = do
    goodArch <- anyTargetArch ["i386", "x86_64"]
    withLibDw <- flag HaveLibMingwEx
    return $ goodArch && withLibDw

-- Compile various performance-critical pieces *without* -fPIC -dynamic
-- even when building a shared library.  If we don't do this, then the
-- GC runs about 50% slower on x86 due to the overheads of PIC.  The
-- cost of doing this is a little runtime linking and less sharing, but
-- not much.
--
-- On x86_64 this doesn't work, because all objects in a shared library
-- must be compiled with -fPIC (since the 32-bit relocations generated
-- by the default small memory can't be resolved at runtime).  So we
-- only do this on i386.
--
-- This apparently doesn't work on OS X (Darwin) nor on Solaris.
-- On Darwin we get errors of the form
--
--  ld: absolute addressing (perhaps -mdynamic-no-pic) used in _stg_ap_0_fast from rts/dist/build/Apply.dyn_o not allowed in slidable image
--
-- and lots of these warnings:
--
--  ld: warning codegen in _stg_ap_pppv_fast (offset 0x0000005E) prevents image from loading in dyld shared cache
--
-- On Solaris we get errors like:
--
-- Text relocation remains                         referenced
--     against symbol                  offset      in file
-- .rodata (section)                   0x11        rts/dist/build/Apply.dyn_o
--   ...
-- ld: fatal: relocations remain against allocatable but non-writable sections
-- collect2: ld returned 1 exit status
speedHack :: Action Bool
speedHack = do
    i386 <- anyTargetArch ["i386"]
    goodOS <- not <$> anyTargetOs ["darwin", "solaris2"]
    return $ i386 && goodOS

rtsPackageArgs :: Args
rtsPackageArgs = package rts ? do
    projectVersion <- getSetting ProjectVersion
    hostPlatform   <- getSetting HostPlatform
    hostArch       <- getSetting HostArch
    hostOs         <- getSetting HostOs
    hostVendor     <- getSetting HostVendor
    buildPlatform  <- getSetting BuildPlatform
    buildArch      <- getSetting BuildArch
    buildOs        <- getSetting BuildOs
    buildVendor    <- getSetting BuildVendor
    targetPlatform <- getSetting TargetPlatform
    targetArch     <- getSetting TargetArch
    targetOs       <- getSetting TargetOs
    targetVendor   <- getSetting TargetVendor
    ghcUnreg       <- expr $ yesNo <$> flag GhcUnregisterised
    ghcEnableTNC   <- expr $ yesNo <$> ghcEnableTablesNextToCode
    way            <- getWay
    path           <- getBuildPath
    top            <- expr topDirectory
    libffiName     <- expr rtsLibffiLibraryName
    ffiIncludeDir  <- getSetting FfiIncludeDir
    ffiLibraryDir  <- getSetting FfiLibDir
    ghclibDir      <- expr installGhcLibDir
    destDir        <- expr getDestDir
    let cArgs =
          [ arg "-Irts"
          , arg $ "-I" ++ path
          , arg $ "-DRtsWay=\"rts_" ++ show way ++ "\""
          -- rts *must* be compiled with optimisations. The INLINE_HEADER macro
          -- requires that functions are inlined to work as expected. Inlining
          -- only happens for optimised builds. Otherwise we can assume that
          -- there is a non-inlined variant to use instead. But rts does not
          -- provide non-inlined alternatives and hence needs the function to
          -- be inlined. See also #90.
          , arg "-O2"

          , Debug     `wayUnit` way          ? arg "-DDEBUG"
          , way `elem` [debug, debugDynamic] ? arg "-DTICKY_TICKY"
          , Profiling `wayUnit` way          ? arg "-DPROFILING"
          , Threaded  `wayUnit` way          ? arg "-DTHREADED_RTS"

          , inputs ["//RtsMessages.c", "//Trace.c"] ?
            arg ("-DProjectVersion=" ++ show projectVersion)

          , input "//RtsUtils.c" ? pure
            [ "-DProjectVersion="            ++ show projectVersion
            , "-DHostPlatform="              ++ show hostPlatform
            , "-DHostArch="                  ++ show hostArch
            , "-DHostOS="                    ++ show hostOs
            , "-DHostVendor="                ++ show hostVendor
            , "-DBuildPlatform="             ++ show buildPlatform
            , "-DBuildArch="                 ++ show buildArch
            , "-DBuildOS="                   ++ show buildOs
            , "-DBuildVendor="               ++ show buildVendor
            , "-DTargetPlatform="            ++ show targetPlatform
            , "-DTargetArch="                ++ show targetArch
            , "-DTargetOS="                  ++ show targetOs
            , "-DTargetVendor="              ++ show targetVendor
            , "-DGhcUnregisterised="         ++ show ghcUnreg
            , "-DGhcEnableTablesNextToCode=" ++ show ghcEnableTNC ]

            , inputs ["//Evac.c", "//Evac_thr.c"] ? arg "-funroll-loops"

            , speedHack ?
              inputs [ "//Evac.c", "//Evac_thr.c"
                     , "//Scav.c", "//Scav_thr.c"
                     , "//Compact.c", "//GC.c" ] ? arg "-fno-PIC"
              -- -static is also necessary for these bits, otherwise the NCG
              -- generates dynamic references:
            , speedHack ?
              inputs [ "//Updates.c", "//StgMiscClosures.c"
                     , "//PrimOps.c", "//Apply.c"
                     , "//AutoApply.c" ] ? pure [ "-fno-PIC", "-static" ]
              -- inlining warnings happen in Compact
            , inputs ["//Compact.c"] ? arg "-Wno-inline"
              -- emits warnings about call-clobbered registers on x86_64
            , inputs [ "//StgCRun.c", "//RetainerProfile.c"
                     , "//win32/ConsoleHandler.c", "//win32/ThrIOManager.c"] ? arg "-w"
            , inputs ["//RetainerSet.c"] ? arg "-Wno-format"
              -- The above warning suppression flags are a temporary kludge.
              -- While working on this module you are encouraged to remove it and fix
              -- any warnings in the module. See
              --     http://ghc.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
              -- for details

            , (not <$> flag GccIsClang) ?
              inputs ["//Compact.c"] ? arg "-finline-limit=2500"

            , inputs ["//Evac_thr.c", "//Scav_thr.c"] ?
              pure [ "-DPARALLEL_GC", "-Irts/sm" ]

            , input "//StgCRun.c" ? windowsHost ? arg "-Wno-return-local-addr"
            , input "//RetainerProfile.c" ? flag GccIsClang ?
              pure [ "-Wno-incompatible-pointer-types" ]
            , targetOs == "mingw32" ? arg ("-DWINVER=" ++ rtsWindowsVersion)
            , ghcRtsWithLibDw ? arg "-DUSE_LIBDW" ]

    mconcat
        [ builder (Cc FindCDependencies) ? mconcat cArgs
        , builder (Ghc CompileCWithGhc) ? mconcat (map (map ("-optc" ++) <$>) cArgs)
        , builder Ghc ? arg "-Irts"
        , builder HsCpp ? pure
          [ "-DTOP="             ++ show top
          , "-DFFI_INCLUDE_DIR=" ++ show ffiIncludeDir
          , "-DFFI_LIB_DIR="     ++ show ffiLibraryDir
          , "-DFFI_LIB="         ++ show libffiName ]

        , builder HsCpp ?
          input "//package.conf.in" ?
          output "//package.conf.install.raw" ?
          pure [ "-DINSTALLING"
               , "-DLIB_DIR=\"" ++ destDir ++ ghclibDir ++ "\""
               , "-DINCLUDE_DIR=\"" ++ destDir ++ ghclibDir -/- "include\"" ]
        , builder HsCpp ? mconcat
            [ ghcRtsWithLibDw ? arg "-DUSE_LIBDW"
            , flag HaveLibMingwEx ? arg "-DHAVE_LIBMINGWEX" ] ]
