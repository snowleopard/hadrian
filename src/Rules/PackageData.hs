module Rules.PackageData (buildPackageData) where

import Base
import Context
import Expression
import Oracles.Setting
import Rules.Generate
import Settings.Packages.Rts
import Target
import Utilities

import Hadrian.Haskell.Cabal.Parse (configurePackage)

-- | Build @package-data.mk@ by using ghc-cabal utility to process .cabal files.
buildPackageData :: Context -> Rules ()
buildPackageData context@Context {..} = do
    let dir       = "//" ++ contextDir context
    -- TODO: Get rid of hardcoded file paths.
    dir -/- "setup-config" %> \_ -> do
        configurePackage context

    -- TODO: Get rid of hardcoded file paths.
    dir -/- "inplace-pkg-config" %> \conf -> do
        dataFile <- pkgDataFile context
        need [dataFile] -- ghc-cabal builds inplace package configuration file
        when (package == rts) $ do
            genPath <- buildRoot <&> (-/- generatedDir)
            rtsPath <- rtsBuildPath
            need [rtsConfIn]
            build $ target context HsCpp [rtsConfIn] [conf]
            fixFile conf $ unlines
                         . map
                         ( replace "\"\"" ""
                         . replace "rts/dist/build" rtsPath
                         . replace "includes/dist-derivedconstants/header" genPath )
                         . lines

    priority 2.0 $ when (nonCabalContext context) $ dir -/- "package-data.mk" %>
        generatePackageData context

generatePackageData :: Context -> FilePath -> Action ()
generatePackageData context@Context {..} file = do
    orderOnly =<< interpretInContext context generatedDependencies
    asmSrcs <- packageAsmSources package
    cSrcs   <- packageCSources   package
    cmmSrcs <- packageCmmSources package
    genPath <- buildRoot <&> (-/- generatedDir)
    writeFileChanged file . unlines $
        [ "S_SRCS = "   ++ unwords asmSrcs                                  ] ++
        [ "C_SRCS = "   ++ unwords cSrcs                                    ] ++
        [ "CMM_SRCS = " ++ unwords cmmSrcs                                  ] ++
        [ "DEP_EXTRA_LIBS = m"                 | package == hp2ps           ] ++
        [ "CC_OPTS = -I" ++ genPath            | package `elem` [hp2ps, rts]] ++
        [ "MODULES = Main"                     | package == ghcCabal        ] ++
        [ "HS_SRC_DIRS = ."                    | package == ghcCabal        ]
    putSuccess $ "| Successfully generated " ++ file

packageCSources :: Package -> Action [FilePath]
packageCSources pkg
    | pkg /= rts = getDirectoryFiles (pkgPath pkg) ["*.c"]
    | otherwise  = do
        windows <- windowsHost
        sources <- fmap (map unifyPath) . getDirectoryFiles (pkgPath pkg) .
            map (-/- "*.c") $ [ ".", "hooks", "sm", "eventlog", "linker" ] ++
                              [ if windows then "win32" else "posix"     ]
        return sources

packageAsmSources :: Package -> Action [FilePath]
packageAsmSources pkg
    | pkg == rts = do
        buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
        buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
        return $ [ "AdjustorAsm.S" | buildAdjustor   ]
              ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]
    | otherwise  = return []

packageCmmSources :: Package -> Action [FilePath]
packageCmmSources pkg
    | pkg == rts  = do
        rtsPath <- rtsBuildPath
        sources <- getDirectoryFiles (pkgPath pkg) ["*.cmm"]
        return $ sources ++ [ rtsPath -/- "cmm/AutoApply.cmm" ]
    | pkg == base = do
        sources <- getDirectoryFiles (pkgPath pkg) ["cbits/*.cmm"]
        return sources
    | otherwise   = return []
