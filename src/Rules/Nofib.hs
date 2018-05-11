module Rules.Nofib where

import Base
import Expression
import GHC.Packages
import Oracles.Setting
import Target
import Utilities

import System.Environment

nofibRules :: Rules ()
nofibRules = do
  "nofib" ~> do
    needNofibBuilders
    makePath <- builderPath (Make "")
    top <- topDirectory
    root <- buildRoot
    liftIO (setEnv "MAKE" makePath)

    let out = top -/- root -/- "nofib.stdout"
        err = top -/- root -/- "nofib.stderr"

    buildWithCmdOptions [FileStdout out, FileStderr err] $
      target (vanillaContext Stage2 compiler)
             (Make "nofib")
             []
             [out, err]

nofibHC :: FilePath -> String
nofibHC ghcPath = "WithNofibHc=" ++ ghcPath

perlProg :: FilePath -> String
perlProg perlPath = "PERL=" ++ perlPath

needNofibBuilders :: Action ()
needNofibBuilders = needBuilder $ Ghc CompileHs Stage2
