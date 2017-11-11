module Utilities (
    build, buildWithResources, buildWithCmdOptions, runBuilder, runBuilderWith,
    needLibrary, contextDependencies, stage1Dependencies, libraryTargets,
    topsortPackages, ask, askWithResources, cabalDependencies
    ) where

import qualified Hadrian.Builder as H
import Hadrian.Haskell.Cabal
import Hadrian.Utilities

import Context
import Expression hiding (stage)
import Settings
import Target
import Types.ConfiguredCabal as ConfCabal

build :: Target -> Action ()
build target = H.build target getArgs

buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = H.buildWithResources rs target getArgs

buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions opts target = H.buildWithCmdOptions opts target getArgs

askWithResources :: [(Resource, Int)] -> Target -> Action String
askWithResources rs target = H.askWithResources rs target getArgs

ask :: Target -> Action String
ask target = H.ask target getArgs

-- | Given a 'Context' this 'Action' look up the package dependencies and wrap
-- the results in appropriate contexts. The only subtlety here is that we never
-- depend on packages built in 'Stage2' or later, therefore the stage of the
-- resulting dependencies is bounded from above at 'Stage1'. To compute package
-- dependencies we scan package @.cabal@ files, see 'pkgDependencies' defined
-- in "Hadrian.Haskell.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies ctx@Context {..} = pkgDependencies ctx >>= \case
    Nothing        -> return [] -- Non-Cabal packages have no dependencies.
    Just deps -> do
        let depStage   = min stage Stage1
            depContext = \pkg -> Context depStage pkg way
        pkgs <- sort <$> stagePackages depStage
        return . map depContext $ intersectOrd (compare . pkgName) pkgs deps

cabalDependencies :: Context -> Action [String]
cabalDependencies ctx = interpretInContext ctx $ getConfiguredCabalData ConfCabal.depIpIds

-- | Lookup dependencies of a 'Package' in the vanilla Stage1 context.
stage1Dependencies :: Package -> Action [Package]
stage1Dependencies =
    fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Given a library 'Package' this action computes all of its targets. See
-- 'packageTargets' for the explanation of the @includeGhciLib@ parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context = do
    libFile  <- pkgLibraryFile     context
    lib0File <- pkgLibraryFile0    context
    lib0     <- buildDll0          context
    ghciLib  <- pkgGhciLibraryFile context
    ghci     <- if includeGhciLib
                then interpretInContext context $ getConfiguredCabalData ConfCabal.buildGhciLib
                else return False
    return $ [ libFile ] ++ [ lib0File | lib0 ] ++ [ ghciLib | ghci ]

-- | Coarse-grain 'need': make sure all given libraries are fully built.
needLibrary :: [Context] -> Action ()
needLibrary cs = need =<< mapM pkgConfFile cs

-- HACK (izgzhen), see https://github.com/snowleopard/hadrian/issues/344.
-- | Topological sort of packages according to their dependencies.
topsortPackages :: [Package] -> Action [Package]
topsortPackages pkgs = do
    elems <- mapM (\p -> (p,) <$> stage1Dependencies p) pkgs
    return $ map fst $ topSort elems
  where
    annotateInDeg es e =
     (foldr (\e' s -> if fst e' `elem` snd e then s + 1 else s) (0 :: Int) es, e)
    topSort [] = []
    topSort es =
      let annotated = map (annotateInDeg es) es
          inDegZero = map snd $ filter ((== 0). fst) annotated
      in  inDegZero ++ topSort (es \\ inDegZero)
