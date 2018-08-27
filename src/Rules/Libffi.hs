module Rules.Libffi (libffiRules, libffiDependencies) where

import Hadrian.Utilities

import Packages
import Settings.Builders.Common
import Target
import Utilities

libffiDependencies :: [FilePath]
libffiDependencies = ["ffi.h", "ffitarget.h"]

libffiLibrary :: FilePath
libffiLibrary = "inst/lib/libffi.a"

rtsLibffiLibrary :: Way -> Action FilePath
rtsLibffiLibrary way = do
    name    <- libffiLibraryName
    suf     <- libsuf way
    rtsPath <- rtsBuildPath
    return $ rtsPath -/- "lib" ++ name ++ suf

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretInContext libffiContext $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext libffiContext ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC Stage1
             , builderEnvironment "CXX" $ Cc CompileC Stage1
             , builderEnvironment "LD" (Ld Stage1)
             , builderEnvironment "AR" (Ar Unpack Stage1)
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

libffiRules :: Rules ()
libffiRules = do
    root <- buildRootRules
    fmap ((root <//> "rts/build") -/-) libffiDependencies &%> \_ -> do
        libffiPath <- libffiBuildPath
        need [libffiPath -/- libffiLibrary]

    -- we set a higher priority because this overlaps
    -- with the static lib rule from Rules.Library.libraryRules.
    priority 2.0 $ root <//> libffiLibrary %> \_ -> do
        useSystemFfi <- flag UseSystemFfi
        rtsPath      <- rtsBuildPath
        if useSystemFfi
        then do
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ ["ffi.h", "ffitarget.h"] $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsPath -/- file)
            putSuccess "| Successfully copied system FFI library header files"
        else do
            libffiPath <- libffiBuildPath
            build $ target libffiContext (Make libffiPath) [] []

            hs <- getDirectoryFiles "" [libffiPath -/- "inst/include/*"]
            forM_ hs $ \header ->
                copyFile header (rtsPath -/- takeFileName header)

            ways <- interpretInContext libffiContext (getLibraryWays <> getRtsWays)
            forM_ (nubOrd ways) $ \way -> do
                rtsLib <- rtsLibffiLibrary way
                copyFileUntracked (libffiPath -/- libffiLibrary) rtsLib

            putSuccess "| Successfully built custom library 'libffi'"

    root <//> "libffi/build/Makefile.in" %> \mkIn -> do
        libffiPath <- libffiBuildPath
        removeDirectory libffiPath
        tarball <- unifyPath . fromSingleton "Exactly one LibFFI tarball is expected"
               <$> getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]

        need [tarball]
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball

        root <- buildRoot
        removeDirectory (root -/- libname)
        -- TODO: Simplify.
        actionFinally (do
            build $ target libffiContext (Tar Extract) [tarball] [root]
            moveDirectory (root -/- libname) libffiPath) $
                removeFiles root [libname <//> "*"]

        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

    -- TODO: Get rid of hard-coded @libffi@.
    root <//> "libffi/build/Makefile" %> \mk -> do
        need [mk <.> "in"]
        libffiPath <- libffiBuildPath
        forM_ ["config.guess", "config.sub"] $ \file ->
            copyFile file (libffiPath -/- file)

        env <- configureEnvironment
        buildWithCmdOptions env $
            target libffiContext (Configure libffiPath) [mk <.> "in"] [mk]
