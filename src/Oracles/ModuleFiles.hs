{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Oracles.ModuleFiles (moduleFiles, haskellModuleFiles, moduleFilesOracle) where

import Base
import Oracles.PackageData
import Package
import Stage
import Settings.Paths

import Distribution.ModuleName as ModuleName

newtype ModuleFilesKey = ModuleFilesKey ([ModuleName], [FilePath])
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- used in generatePackageCode (Generate.hs)
moduleFiles :: Stage -> Package -> Action [FilePath]
moduleFiles stage pkg = do
    allPkgData <- askAllPackageData stage pkg
    let srcDirs = sort . pdHsSourceDirs $ allPkgData
        modules = sort . pdModules      $ allPkgData -- Do we need pdHiddenModules here as well?
    let dirs = [ pkgPath pkg -/- dir | dir <- srcDirs ]
    found :: [(String, FilePath)] <- askOracle $ ModuleFilesKey (modules, dirs)
    return $ map snd found

-- used in getPackageSources (Settings.hs)
haskellModuleFiles :: Stage -> Package -> Action ([FilePath], [String])
haskellModuleFiles stage pkg = do
    let autogen     = targetPath stage pkg -/- "build/autogen"
        dropPkgPath = drop $ length (pkgPath pkg) + 1

    allPkgData <- askAllPackageData stage pkg
    let srcDirs = sort . pdHsSourceDirs $ allPkgData
        modules = sort . pdModules      $ allPkgData -- Do we need pdHiddenModules here as well?    let dirs = [ pkgPath pkg -/- dir | dir <- srcDirs ]
    let dirs = [ pkgPath pkg -/- dir | dir <- srcDirs ]

    foundSrcDirs <- askOracle $ ModuleFilesKey (modules, dirs     )
    foundAutogen <- askOracle $ ModuleFilesKey (modules, [autogen])

    let found          = foundSrcDirs ++ foundAutogen
        missingMods    = (map toFilePath modules) `minusOrd` (sort $ map fst found)
        otherFileToMod = replaceEq '/' '.' . dropExtension . dropPkgPath
        (haskellFiles, otherFiles) = partition ("//*hs" ?==) (map snd found)

    return (haskellFiles, missingMods ++ map otherFileToMod otherFiles)

moduleFilesOracle :: Rules ()
moduleFilesOracle = do
    answer <- newCache $ \(modules, dirs) -> do
        let decodedPairs = map (decodeModule . toFilePath) modules
            modDirFiles  = map (bimap head sort . unzip)
                         . groupBy ((==) `on` fst) $ decodedPairs

        result <- fmap concat . forM dirs $ \dir -> do
            todo <- filterM (doesDirectoryExist . (dir -/-) . fst) modDirFiles
            forM todo $ \(mDir, mFiles) -> do
                let fullDir = dir -/- mDir
                files <- getDirectoryFiles fullDir ["*"]
                let noBoot   = filter (not . (isSuffixOf "-boot")) files
                    cmp fe f = compare (dropExtension fe) f
                    found    = intersectOrd cmp noBoot mFiles
                return (map (fullDir -/-) found, mDir)

        return $ sort [ (encodeModule d f, f) | (fs, d) <- result, f <- fs ]

    _ <- addOracle $ \(ModuleFilesKey query) -> answer query
    return ()
