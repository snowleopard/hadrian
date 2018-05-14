module Rules.Nofib where

import Base
import Expression
import GHC
import Oracles.Setting
import Target
import Utilities

import System.Environment

nofibRules :: Rules ()
nofibRules = do
  root <- buildRootRules

  "nofib" ~> need [root -/- nofibLogFile]

  root -/- nofibLogFile %> \fp -> do
    needNofibBuilders
    makePath <- builderPath (Make "nofib")
    top <- topDirectory
    ghcPath <- builderPath (Ghc CompileHs Stage2)
    perlPath <- builderPath Perl
    liftIO (setEnv "MAKE" makePath)

    build $
      target (vanillaContext Stage2 compiler)
             RunNofib
             [top -/- ghcPath, perlPath]
             [fp]

nofibLogFile :: FilePath
nofibLogFile = "nofib-log"

needNofibBuilders :: Action ()
needNofibBuilders = do
  unlitPath <- programPath (Context Stage1 unlit vanilla)
  mtlPath <- pkgConfFile (Context Stage1 mtl vanilla)
  liftIO $ print (unlitPath, mtlPath)
  need [ unlitPath, mtlPath ]
  needBuilder (Ghc CompileHs Stage2)
