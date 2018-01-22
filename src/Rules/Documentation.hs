module Rules.Documentation (
    -- * Rules
    buildPackageDocumentation, documentationRules,

    -- * Utilities
    haddockDependencies
    ) where

import Base
import Context
import Expression (getConfiguredCabalData, interpretInContext)
import GHC
import Oracles.ModuleFiles
import Settings
import Target
import qualified Types.ConfiguredCabal as ConfCabal
import Types.Flavour
import Utilities

-- | Build all documentation
documentationRules :: Rules ()
documentationRules = do
    buildHtmlDocumentation
    buildPdfDocumentation
    buildDocumentationArchives
    buildManPage
    "//docs//gen_contents_index" %> copyFile "libraries/gen_contents_index"
    "//docs//prologue.txt" %> copyFile "libraries/prologue.txt"
    "docs" ~> do
        root <- buildRoot
        let html = htmlRoot -/- "index.html"
            archives = map pathArchive docPaths
            pdfs = map pathPdf $ docPaths \\ [ "libraries" ]
        need $ map (root -/-) $ [html] ++ archives ++ pdfs
        need [ root -/- htmlRoot -/- "libraries" -/- "gen_contents_index" ]
        need [ root -/- htmlRoot -/- "libraries" -/- "prologue.txt" ]
        need [manPagePath]

manPagePath :: FilePath
manPagePath = "_build/docs/users_guide/build-man/ghc.1"

-- TODO: Add support for Documentation Packages so we can
-- run the builders without this hack.
docPackage :: Package
docPackage = hsLibrary "Documentation" "docs"

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

pathArchive :: FilePath -> FilePath
pathArchive path = archiveRoot -/- path <.> "html.tar.xz"

-- TODO: Replace this with pkgPath when support is added
-- for Documentation Packages.
pathPath :: FilePath -> FilePath
pathPath "users_guide" = "docs/users_guide"
pathPath "Haddock" = "utils/haddock/doc"
pathPath _ = ""

----------------------------------------------------------------------
-- HTML

-- | Build all HTML documentation
buildHtmlDocumentation :: Rules ()
buildHtmlDocumentation = do
    mapM_ buildSphinxHtml $ docPaths \\ [ "libraries" ]
    buildLibraryDocumentation
    root <- buildRootRules
    root -/- htmlRoot -/- "index.html" %> \file -> do
        root <- buildRoot
        need $ map ((root -/-) . pathIndex) docPaths
        copyFileUntracked "docs/index.html" file

-----------------------------
-- Sphinx

-- | Compile a Sphinx ReStructured Text package to HTML
buildSphinxHtml :: FilePath -> Rules ()
buildSphinxHtml path = do
    root <- buildRootRules
    root -/- htmlRoot -/- path -/- "index.html" %> \file -> do
        let dest = takeDirectory file
            context = vanillaContext Stage0 docPackage
        build $ target context (Sphinx Html) [pathPath path] [dest]

-----------------------------
-- Haddock

-- | Build the haddocks for GHC's libraries
buildLibraryDocumentation :: Rules ()
buildLibraryDocumentation = do
    root <- buildRootRules
    root -/- htmlRoot -/- "libraries/index.html" %> \file -> do
        -- If we omit this, the 'docs' rule fails with an error
        -- complaining about this file not being there:
        -- https://gist.github.com/alpmestan/f0709cdf001efe1dd35ed1c7a216fc71
        -- when running e.g:
        -- hadrian/build.nix.sh --flavour=quick-with-ng -j3 docs
        need [ root -/- "stage1/lib/llvm-targets" ]
        haddocks <- allHaddocks
        let libDocs = filter (\x -> takeFileName x `notElem` [ "ghc.haddock"
                                                             , "rts.haddock"
                                                             ]
                             ) haddocks
            context = vanillaContext Stage2 docPackage
        need libDocs
        build $ target context (Haddock BuildIndex) libDocs [file]

allHaddocks :: Action [FilePath]
allHaddocks = do
    pkgs <- stagePackages Stage1
    sequence [ pkgHaddockFile $ vanillaContext Stage1 pkg
             | pkg <- pkgs, isLibrary pkg, isHsPackage pkg ]

haddockHtmlLib :: FilePath -> FilePath
haddockHtmlLib root = root -/- "lib/html/haddock-bundle.min.js"

-- | Find the haddock files for the dependencies of the current library
haddockDependencies :: Context -> Action [FilePath]
haddockDependencies context = do
    depNames <- interpretInContext context (getConfiguredCabalData ConfCabal.depNames)
    sequence [ pkgHaddockFile $ vanillaContext Stage1 depPkg
             | Just depPkg <- map findPackageByName depNames, depPkg /= rts ]

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'doc' subdirectory. Pedantically tracking all built
-- files in the Shake database seems fragile and unnecessary.
buildPackageDocumentation :: Context -> Rules ()
buildPackageDocumentation context@Context {..} = when (stage == Stage1 && package /= rts) $ do
    root <- buildRootRules

    -- Js and Css files for haddock output
    when (package == haddock) $ haddockHtmlLib root %> \_ -> do
        let dir = takeDirectory (haddockHtmlLib root)
        liftIO $ removeFiles dir ["//*"]
        copyDirectory "utils/haddock/haddock-api/resources/html" dir

    -- Per-package haddocks

    root -/- htmlRoot -/- "libraries" -/- pkgName package -/- "haddock-prologue.txt" %> \file -> do
      -- this is how ghc-cabal produces "haddock-prologue.txt" files
      (syn, desc) <- interpretInContext context . getConfiguredCabalData $ \p ->
        (ConfCabal.synopsis p, ConfCabal.description p)
      let prologue = if null desc
                     then syn
                     else desc
      liftIO (writeFile file prologue)

    root -/- htmlRoot -/- "libraries" -/- pkgName package -/- pkgName package <.> "haddock" %> \file -> do
      need [ root -/- htmlRoot -/- "libraries" -/- pkgName package -/- "haddock-prologue.txt" ]
      haddocks <- haddockDependencies context
      srcs <- hsSources context
      need $ srcs ++ haddocks ++ [haddockHtmlLib root]

      -- Build Haddock documentation
      -- TODO: pass the correct way from Rules via Context
      dynamicPrograms <- dynamicGhcPrograms <$> flavour
      let haddockWay = if dynamicPrograms then dynamic else vanilla
      build $ target (context {way = haddockWay}) (Haddock BuildPackage)
                     srcs [file]

----------------------------------------------------------------------
-- PDF

-- | Build all PDF documentation
buildPdfDocumentation :: Rules ()
buildPdfDocumentation = mapM_ buildSphinxPdf docPaths

-- | Compile a Sphinx ReStructured Text package to LaTeX
buildSphinxPdf :: FilePath -> Rules ()
buildSphinxPdf path = do
    root <- buildRootRules
    root -/- pdfRoot -/- path <.> "pdf" %> \file -> do
        let context = vanillaContext Stage0 docPackage
        withTempDir $ \dir -> do
            build $ target context (Sphinx Latex) [pathPath path] [dir]
            build $ target context Xelatex [path <.> "tex"] [dir]
            copyFileUntracked (dir -/- path <.> "pdf") file

----------------------------------------------------------------------
-- Archive

-- | Build archives of documentation
buildDocumentationArchives :: Rules ()
buildDocumentationArchives = mapM_ buildArchive docPaths

buildArchive :: FilePath -> Rules ()
buildArchive path = do
    root <- buildRootRules
    root -/- pathArchive path %> \file -> do
        root <- buildRoot
        let context = vanillaContext Stage0 docPackage
            src = root -/- pathIndex path
        need [src]
        build $ target context (Tar Create) [takeDirectory src] [file]

-- | build man page
buildManPage :: Rules ()
buildManPage = do
    manPagePath %> \file -> do
        need ["docs/users_guide/ghc.rst"]
        let context = vanillaContext Stage0 docPackage
        withTempDir $ \dir -> do
            build $ target context (Sphinx Man) ["docs/users_guide"] [dir]
            copyFileUntracked (dir -/- "ghc.1") file
