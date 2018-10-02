module Settings.Builders.Cabal (cabalBuilderArgs) where

import Hadrian.Builder (getBuilderPath, needBuilder)
import Hadrian.Haskell.Cabal

import Builder
import Context
import Flavour
import Packages
import Settings.Builders.Common

cabalBuilderArgs :: Args
cabalBuilderArgs = builder (Cabal Setup) ? do
    verbosity <- expr getVerbosity
    top       <- expr topDirectory
    path      <- getContextPath
    stage     <- getStage
    mconcat [ arg "configure"
            -- Don't strip libraries when cross compiling.
            -- TODO: We need to set @--with-strip=(stripCmdPath :: Action FilePath)@,
            -- and if it's @:@ disable stripping as well. As it is now, I believe
            -- we might have issues with stripping on Windows, as I can't see a
            -- consumer of 'stripCmdPath'.
            -- TODO: See https://github.com/snowleopard/hadrian/issues/549.
            , flag CrossCompiling ? pure [ "--disable-executable-stripping"
                                         , "--disable-library-stripping" ]
            , arg "--cabal-file"
            , arg =<< pkgCabalFile <$> getPackage
            , arg "--distdir"
            , arg $ top -/- path
            , arg "--ipid"
            , arg "$pkg-$version"
            , arg "--prefix"
            , arg "${pkgroot}/.."
            , withStaged $ Ghc CompileHs
            , withStaged (GhcPkg Update)
            , withBuilderArgs (GhcPkg Update stage)
            , bootPackageDatabaseArgs
            , libraryArgs
            , configureArgs
            , bootPackageConstraints
            , withStaged $ Cc CompileC
            , notStage0 ? with (Ld stage)
            , withStaged (Ar Pack)
            , with Alex
            , with Happy
            , verbosity < Chatty ?
              pure [ "-v0", "--configure-option=--quiet"
                   , "--configure-option=--disable-option-checking" ] ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
-- TODO: Need compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci?
-- TODO: should `elem` be `wayUnit`?
-- This approach still doesn't work. Previously libraries were build only in the
-- Default flavours and not using context.
libraryArgs :: Args
libraryArgs = do
    flavourWays <- getLibraryWays
    contextWay  <- getWay
    withGhci    <- expr ghcWithInterpreter
    dynPrograms <- expr (flavour >>= dynamicGhcPrograms)
    let ways = flavourWays ++ [contextWay]
    pure [ if vanilla `elem` ways
           then  "--enable-library-vanilla"
           else "--disable-library-vanilla"
         , if vanilla `elem` ways && withGhci && not dynPrograms
           then  "--enable-library-for-ghci"
           else "--disable-library-for-ghci"
         , if or [Profiling `wayUnit` way | way <- ways]
           then  "--enable-library-profiling"
           else "--disable-library-profiling"
         , if or [Dynamic `wayUnit` way | way <- ways]
           then  "--enable-shared"
           else "--disable-shared" ]

-- TODO: LD_OPTS?
configureArgs :: Args
configureArgs = do
    top  <- expr topDirectory
    root <- getBuildRoot
    pkg  <- getPackage
    let conf key expr = do
            values <- unwords <$> expr
            not (null values) ?
                arg ("--configure-option=" ++ key ++ "=" ++ values)
        cFlags   = mconcat [ remove ["-Werror"] cArgs
                           , getStagedSettingList ConfCcArgs
                           , arg $ "-I" ++ top -/- root -/- generatedDir
                           -- See https://github.com/snowleopard/hadrian/issues/523
                           , arg $ "-I" ++ top -/- pkgPath pkg
                           , arg $ "-I" ++ top -/- "includes" ]
        ldFlags  = ldArgs  <> (getStagedSettingList ConfGccLinkerArgs)
        cppFlags = cppArgs <> (getStagedSettingList ConfCppArgs)
    cldFlags <- unwords <$> (cFlags <> ldFlags)
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "CPPFLAGS" cppFlags
        , not (null cldFlags) ? arg ("--gcc-options=" ++ cldFlags)
        , conf "--with-iconv-includes"    $ arg =<< getSetting IconvIncludeDir
        , conf "--with-iconv-libraries"   $ arg =<< getSetting IconvLibDir
        , conf "--with-gmp-includes"      $ arg =<< getSetting GmpIncludeDir
        , conf "--with-gmp-libraries"     $ arg =<< getSetting GmpLibDir
        , conf "--with-curses-libraries"  $ arg =<< getSetting CursesLibDir
        , flag CrossCompiling ? (conf "--host" $ arg =<< getSetting TargetPlatformFull)
        , conf "--with-cc" $ arg =<< getBuilderPath . (Cc CompileC) =<< getStage
        , notStage0 ? (arg =<< ("--ghc-option=-ghcversion-file=" ++) <$> expr ((-/-) <$> topDirectory <*> ghcVersionH))]

bootPackageConstraints :: Args
bootPackageConstraints = stage0 ? do
    bootPkgs <- expr $ stagePackages Stage0
    let pkgs = filter (\p -> p /= compiler && isLibrary p) bootPkgs
    constraints <- expr $ forM (sort pkgs) $ \pkg -> do
        version <- pkgVersion pkg
        return $ ((pkgName pkg ++ " == ") ++) version
    pure $ concat [ ["--constraint", c] | c <- constraints ]

cppArgs :: Args
cppArgs = do
    root <- getBuildRoot
    arg $ "-I" ++ root -/- generatedDir

withBuilderKey :: Builder -> String
withBuilderKey b = case b of
    Ar _ _     -> "--with-ar="
    Ld _       -> "--with-ld="
    Cc  _ _    -> "--with-gcc="
    Ghc _ _    -> "--with-ghc="
    Alex       -> "--with-alex="
    Happy      -> "--with-happy="
    GhcPkg _ _ -> "--with-ghc-pkg="
    _          -> error $ "withBuilderKey: not supported builder " ++ show b

-- | Add arguments to builders if needed.
withBuilderArgs :: Builder -> Args
withBuilderArgs b = case b of
    GhcPkg _ stage -> do
      top   <- expr topDirectory
      pkgDb <- expr $ packageDbPath stage
      notStage0 ? arg ("--ghc-pkg-option=--global-package-db=" ++ top -/- pkgDb)
    _          -> return [] -- no arguments

-- | Expression 'with Alex' appends "--with-alex=/path/to/alex" and needs Alex.
with :: Builder -> Args
with b = do
    path <- getBuilderPath b
    if null path then mempty else do
        top <- expr topDirectory
        expr $ needBuilder b
        arg  $ withBuilderKey b ++ unifyPath (top </> path)

withStaged :: (Stage -> Builder) -> Args
withStaged sb = with . sb =<< getStage
