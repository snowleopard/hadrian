{-# LANGUAGE TupleSections #-}
module Rules.Register (copyBootPackages, registerPackage) where

import Base
import Context
import GHC
import Target
import Utilities
import Oracles.Setting

import Distribution.ParseUtils
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Version (Version)

import Hadrian.Haskell.Cabal.Parse as Cabal

parseCabalName :: String -> Maybe (String, Version)
parseCabalName = readPToMaybe parse
  where parse = (,) <$> (parsePackageName <* Parse.char '-') <*> parseOptVersion

-- | This rule provides rules for copying packges into the
-- boot packages db from the installed compiler.
copyBootPackages :: [(Resource, Int)] -> Context -> Rules ()
copyBootPackages rs context@Context {..} = do
    "//" ++ stage0PackageDbDir -/- "*.conf" %> \conf -> do
      settings <- libPath context <&> (-/- "settings")
      platformConstants <- libPath context <&> (-/- "platformConstants")
      need [settings, platformConstants]
      copyConf rs context conf

-- TODO: Simplify.
-- | Build rules for registering packages and initialising package databases
-- by running the @ghc-pkg@ utility.
registerPackage :: [(Resource, Int)] -> Context -> Rules ()
registerPackage rs context@Context {..} = do
    pkgId <- case pkgCabalFile package of
        Just file -> do
            cabal <- liftIO $ parseCabal file
            return $ if (null $ version cabal)
                     then Cabal.name cabal
                     else Cabal.name cabal ++ "-" ++ version cabal
        Nothing   -> return (pkgName package)

    -- 'rts' has no version. As such we should never generate a rule for the
    -- rts in stage0. The rts is also not expected to be built for stage0.
    -- We intend to copy over the pkg from the bootstrap compiler.
    --
    -- This usually works if packges have <name>-<version> identifier. As
    -- dependencies will pick from the bootstrap compiler as needed. For
    -- packages without version though, this results duplicated rules for
    -- the copyBootPackage and the packge.
    --
    -- TODO: HACK
    -- This should really come from the flavour's packages. But those are
    -- currently not available at rule time...
    let bootpackages = [ binary, text, transformers, mtl, parsec, cabal, hpc
                       , ghcBootTh, ghcBoot, templateHaskell, compiler, ghci
                       , terminfo -- TODO: only if Windows_HOST == NO
                       ]
    when (stage == Stage0 && package `elem` bootpackages) $ do
        -- Packages @ghc-boot@ and @ghc-boot-th@ both match the @ghc-boot*@
        -- pattern, therefore we need to use priorities to match the right rule.
        -- TODO: Get rid of this hack.
        "//" ++ stage0PackageDbDir -/- pkgId ++ ".conf" %%>
            buildConf rs context

        -- This is hack. This check is only here so we build it at most once.
        when (package == binary) $ "//" ++ stage0PackageDbDir -/- packageDbStamp %>
            buildStamp rs context

    when (stage == Stage1) $ do
        "//" ++ inplacePackageDbPath stage -/- pkgId ++ ".conf" %%>
            buildConf rs context

        when (package == ghc) $ "//" ++ inplacePackageDbPath stage -/- packageDbStamp %>
            buildStamp rs context

copyConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
copyConf rs context@Context {..} conf = do
    let Just pkgName | takeBaseName conf == "rts" = Just "rts"
                     | otherwise = fst <$> parseCabalName (takeBaseName conf)
    depPkgIds <- fmap stdOutToPkgIds . askWithResources rs $
      target context (GhcPkg Dependencies stage) [pkgName] []
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds
    buildWithResources rs $ do
      target context (GhcPkg Clone stage) [pkgName] [conf]

  where
    stdOutToPkgIds :: String -> [String]
    stdOutToPkgIds = drop 1 . concatMap words . lines

archive :: Way -> String -> String
archive way pkgId = "libHS" ++ pkgId ++ (waySuffix way <.> "a")

pkgObject :: Way -> String -> String
pkgObject way pkgId = "HS" ++ pkgId ++ (waySuffix way <.> "o")

buildConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConf rs context@Context {..} conf = do
    pkgId <- case pkgCabalFile package of
      Just file -> do
        cabal <- liftIO $ parseCabal file
        return $ if (null $ version cabal)
          then Cabal.name cabal
          else Cabal.name cabal ++ "-" ++ version cabal
      Nothing   -> return (pkgName package)

    depPkgIds <- cabalDependencies context
    confIn <- pkgInplaceConfig context
    need [confIn]

    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds

    -- ghc-cabal copy libraries/terminfo $PWD/_build/stage0/libraries/terminfo : $PWD/_build/stage1 "" "lib" "share" "v"
    -- ghc-cabal register libraries/terminfo $PWD/_build/stage0/libraries/terminfo ghc ghc-pkg $PWD/_build/stage1/lib $PWD/_build_stage1 "" "lib" "share" YES
    _a <- buildPath context <&> (-/- archive way pkgId)
    _o <- buildPath context <&> (-/- pkgObject way pkgId)

    need [_a, _o]

    -- might need some package-db resource to limit read/write,
    -- see packageRules
    top     <- topDirectory
    ctxPath <- (top -/-) <$> contextPath context
    stgPath <- (top -/-) <$> stagePath context
    libPath <- (top -/-) <$> libPath context
    build $ target context (GhcCabal Copy stage) [ (pkgPath package) -- <directory>
                                                 , ctxPath -- <distdir>
                                                 , ":" -- no strip. ':' special marker
                                                 , stgPath -- <destdir>
                                                 , ""      -- <prefix>
                                                 , "lib"   -- <libdir>
                                                 , "share" -- <docdir>
                                                 , "v"     -- TODO: <way> e.g. "v dyn" for dyn way.
                                                 ] []
    build $ target context (GhcCabal Reg stage)  [ -- <directory> <distdir> <ghc> <ghc-pkg> are provided by the ghcCabalBuilderArgs
                                                   libPath
                                                 , stgPath
                                                 , ""
                                                 , libPath
                                                 , "share"
                                                 , if stage == Stage0 then "NO" else "YES"  -- <relocatable>
                                                 ] [conf]

buildStamp :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildStamp rs Context {..} stamp = do
    let path = takeDirectory stamp
    removeDirectory path
    buildWithResources rs $
        target (vanillaContext stage ghc) (GhcPkg Init stage) [] [path]
    writeFileLines stamp []
    putSuccess $ "| Successfully initialised " ++ path
