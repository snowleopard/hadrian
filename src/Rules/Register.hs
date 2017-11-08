{-# LANGUAGE TupleSections #-}
module Rules.Register (registerPackages) where

import Base
import Context
import GHC
import Target
import Utilities
import Oracles.Setting
import Hadrian.Expression
import Settings

import Distribution.ParseUtils
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Version (Version)

import Hadrian.Haskell.Cabal.Parse as Cabal

parseCabalName :: String -> Maybe (String, Version)
parseCabalName = readPToMaybe parse
  where parse = (,) <$> (parsePackageName <* Parse.char '-') <*> parseOptVersion

-- | This rule provides rules for copying packges into the
-- boot packages db from the installed compiler.
-- | Build rules for registering packages and initialising package databases
-- by running the @ghc-pkg@ utility.
registerPackages :: [(Resource, Int)] -> Context -> Rules ()
registerPackages rs context@Context {..} = do
    "//" ++ inplacePackageDbPath stage %>
      buildStamp rs context

    "//" ++ inplacePackageDbPath stage -/- packageDbStamp %> \stamp -> do
      writeFileLines stamp []

    "//" ++ inplacePackageDbPath stage -/- "*.conf" %> \conf -> do
      settings <- libPath context <&> (-/- "settings")
      platformConstants <- libPath context <&> (-/- "platformConstants")

      need [settings, platformConstants]
      let Just pkgName | takeBaseName conf == "rts" = Just "rts"
                       | otherwise = fst <$> parseCabalName (takeBaseName conf)
      let Just pkg = findPackageByName pkgName
      bootLibs <- filter isLibrary <$> (defaultPackages Stage0)
      case stage of
        Stage0 | not (pkg `elem` bootLibs) -> copyConf rs (context { package = pkg }) conf
        _                                  -> buildConf rs (context { package = pkg }) conf

copyConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
copyConf rs context@Context {..} conf = do
    depPkgIds <- fmap stdOutToPkgIds . askWithResources rs $
      target context (GhcPkg Dependencies stage) [pkgName package] []
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds
    buildWithResources rs $ do
      target context (GhcPkg Clone stage) [pkgName package] [conf]

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
      Just file -> liftIO $ parseCabalPkgId file
      Nothing   -> return (pkgName package)

    depPkgIds <- cabalDependencies context
    -- confIn <- pkgInplaceConfig context

    -- setup-config, triggers `ghc-cabal configure`
    -- everything of a package should depend on that
    -- in the first place.
    setupConfig <- (contextPath context) <&> (-/- "setup-config")
    need [setupConfig]

    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds

    ways <- interpretInContext context (getLibraryWays <> if package == rts then getRtsWays else mempty)
    need =<< concatMapM (libraryTargets True) [ context { way = w } | w <- ways ]

    -- might need some package-db resource to limit read/write,
    -- see packageRules
    top     <- topDirectory
    ctxPath <- (top -/-) <$> contextPath context
    bldPath <- (top -/-) <$> buildPath context
    stgPath <- (top -/-) <$> stagePath context
    libPath <- (top -/-) <$> libPath context

    -- special package cases (these should ideally be rolled into cabal one way or the other)
    when (package == rts) $
      -- iif cabal new about "generated-headers", we could read them from the configuredCabal
      -- information, and just "need" them here.
      need [bldPath -/- "DerivedConstants.h"]

    -- copy and register the package
    copyPackage context
    registerPackage context

buildStamp :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildStamp rs Context {..} path = do
    buildWithResources rs $
        target (vanillaContext stage ghc) (GhcPkg Init stage) [] [path]
    putSuccess $ "| Successfully initialised " ++ path
