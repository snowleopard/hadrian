module Rules.Documentation (buildPackageDocumentation, documentationRules, haddockDependencies) where

import Base
import Context
import Flavour
import GHC
import Oracles.ModuleFiles
import Oracles.PackageData
import Settings
import Target
import Utilities

docPackage :: Package
docPackage = hsLibrary "Documentation" "docs"

-- | Build all documentation
documentationRules :: Rules ()
documentationRules = do
    buildHtmlDocumentation
    buildPdfDocumentation
    buildDocumentationArchives
    "docs" ~> do
        root <- buildRoot
        -- Archives `need` the associated html
        let archives = map pathArchive docPaths
            pdfs = map pathPdf $ drop 1 docPaths
        need $ map (root -/-) $ archives ++ pdfs

docPaths :: [FilePath]
docPaths = [ "libraries", "users_guide", "Haddock" ]

docRoot :: FilePath
docRoot = "docs"

htmlRoot :: FilePath
htmlRoot = docRoot -/- "html"

pdfRoot :: FilePath
pdfRoot = docRoot -/- "pdfs"

archiveRoot :: FilePath
archiveRoot = docRoot -/- "archives"

pathPdf :: FilePath -> FilePath
pathPdf path = pdfRoot -/- path <.> ".pdf"

pathIndex :: FilePath -> FilePath
pathIndex path = htmlRoot -/- path -/- "index.html"

pathLibIndex :: FilePath -> FilePath
pathLibIndex path = htmlRoot -/- "libraries" -/- path -/- "index.html"

pathArchive :: FilePath -> FilePath
pathArchive path = archiveRoot -/- path <.> "html.tar.xz"

pathPath :: FilePath -> FilePath
pathPath "users_guide" = "docs/users_guide"
pathPath "Haddock" = "utils/haddock/doc"
pathPath _ = "lol"

----------------------------------------------------------------------
-- HTML

-- | Build all HTML documentation
buildHtmlDocumentation :: Rules ()
buildHtmlDocumentation = do
    mapM_ buildSphinxHtml $ drop 1 docPaths
    buildLibraryDocumentation
    "//html/index.html" %> \file -> do
        root <- buildRoot
        need $ map ((root -/-) . pathIndex) docPaths
        copyFileUntracked "docs/index.html" file

-----------------------------
-- Sphinx

-- | Compile a Sphinx ReStructured Text package to HTML
buildSphinxHtml :: FilePath -> Rules ()
buildSphinxHtml path = do
    "//" ++ htmlRoot -/- path -/- "index.html" %> \file -> do
        let dest = takeDirectory file
            context = vanillaContext Stage0 docPackage
        build $ target context (Sphinx Html) [pathPath path] [dest]

-----------------------------
-- Haddock

haddockHtmlLib :: FilePath
haddockHtmlLib = "inplace/lib/html/haddock-util.js"

haddockDependencies :: Context -> Action [FilePath]
haddockDependencies context = do
    path     <- buildPath context
    depNames <- pkgDataList $ DepNames path
    sequence [ pkgHaddockFile $ vanillaContext Stage1 depPkg
             | Just depPkg <- map findPackageByName depNames, depPkg /= rts ]

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'doc' subdirectory. Pedantically tracking all built
-- files in the Shake database seems fragile and unnecessary.
buildPackageDocumentation :: Context -> Rules ()
buildPackageDocumentation context@Context {..} = when (stage == Stage1) $ do

    -- Js and Css files for haddock output
    when (package == haddock) $ haddockHtmlLib %> \_ -> do
        let dir = takeDirectory haddockHtmlLib
        liftIO $ removeFiles dir ["//*"]
        copyDirectory "utils/haddock/haddock-api/resources/html" dir

    -- Per-package haddocks
    "//" ++ pkgName package <.> "haddock" %> \file -> do
        haddocks <- haddockDependencies context
        srcs <- hsSources context
        need $ srcs ++ haddocks ++ [haddockHtmlLib]

        -- Build Haddock documentation
        -- TODO: pass the correct way from Rules via Context
        dynamicPrograms <- dynamicGhcPrograms <$> flavour
        let haddockWay = if dynamicPrograms then dynamic else vanilla
        build $ target (context {way = haddockWay}) Haddock srcs [file]

-- | Build the haddocks for GHC's libraries
buildLibraryDocumentation :: Rules ()
buildLibraryDocumentation = do
    "//" ++ htmlRoot -/- "libraries/index.html" %> \file -> do
        haddocks <- allHaddocks
        need haddocks
        let libDocs = filter (\x -> takeFileName x /= "ghc.haddock") haddocks
        let iface haddock = "--read-interface="
                            ++ (takeFileName . takeDirectory) haddock ++ ","
                            ++ haddock
        runBuilder Haddock ([ "--gen-index"
                            , "--gen-contents"
                            , "-o", takeDirectory file
                            , "-t", "Haskell Hierarchical Libraries"
                            , "-p", "libraries/prologue.txt" ]
                            ++ map iface libDocs)
                           [] [file]
  where
    allHaddocks = do
        pkgs <- stagePackages Stage1
        sequence [ pkgHaddockFile $ vanillaContext Stage1 pkg
                 | pkg <- pkgs, isLibrary pkg, isHsPackage pkg
                                             , pkgName pkg /= "Win32" ]

----------------------------------------------------------------------
-- PDF

-- | Build all PDF documentation
buildPdfDocumentation :: Rules ()
buildPdfDocumentation = mapM_ buildSphinxPdf docPaths

-- | Compile a Sphinx ReStructured Text package to LaTeX
buildSphinxPdf :: FilePath -> Rules ()
buildSphinxPdf path = do
    "//" ++ path <.> "pdf" %> \file -> do
        let context = vanillaContext Stage0 docPackage
        withTempDir $ \dir -> do
            -- TODO: Figure out a crutch
            build $ target context (Sphinx Latex) [pathPath path] [dir]
            --runBuilderWithCmdOptions [Cwd dir] Xelatex ["-halt-on-error"
            --                                           , name <.> "tex" ] [] []
            unit $ cmd Shell [Cwd dir] ["xelatex", "-halt-on-error"
                                                 , path <.> "tex" ]
            unit $ cmd Shell [Cwd dir] ["xelatex", "-halt-on-error"
                                                 , path <.> "tex" ]
            unit $ cmd Shell [Cwd dir] ["xelatex", "-halt-on-error"
                                                 , path <.> "tex" ]
            unit $ cmd Shell [Cwd dir] ["makeindex", path <.> "idx"]
            unit $ cmd Shell [Cwd dir] ["xelatex", "-halt-on-error"
                                                 , path <.> "tex" ]
            unit $ cmd Shell [Cwd dir] ["xelatex", "-halt-on-error"
                                                 , path <.> "tex" ]
            copyFileUntracked (dir -/- path <.> "pdf") file

----------------------------------------------------------------------
-- Archive

-- | Build archives of documentation
buildDocumentationArchives :: Rules ()
buildDocumentationArchives = mapM_ buildArchive docPaths

buildArchive :: FilePath -> Rules ()
buildArchive path = do
    "//" ++ pathArchive path %> \file -> do
        root <- buildRoot
        let context = vanillaContext Stage0 docPackage
            src = root -/- pathIndex path
        need [src]
        build $ target context (Tar Create) [takeDirectory src] [file]
