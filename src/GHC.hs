{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module GHC (
    -- * GHC packages
    defaultPackages, testsuitePackages,
    ) where

import Base
import Flavour
import GHC.Packages
import Oracles.Flag
import Oracles.Setting
import Settings

-- | Packages that are built by default. You can change this in "UserSettings".
defaultPackages :: Stage -> Action [Package]
defaultPackages Stage0 = stage0Packages
defaultPackages Stage1 = stage1Packages
defaultPackages Stage2 = stage2Packages
defaultPackages Stage3 = return []

-- | Packages built in 'Stage0' by default. You can change this in "UserSettings".
stage0Packages :: Action [Package]
stage0Packages = do
    win <- windowsHost
    cross <- flag CrossCompiling
    return $ [ binary
             , cabal
             , compareSizes
             , compiler
             , deriveConstants
             , genapply
             , genprimopcode
             , ghc
             , ghcBoot
             , ghcBootTh
             , ghcHeap
             , ghci
             , ghcPkg
             , hsc2hs
             , hpc
             , mtl
             , parsec
             , templateHaskell
             , text
             , transformers
             , unlit                         ]
          ++ [ terminfo | not win, not cross ]
          ++ [ touchy   | win                ]

-- | Packages built in 'Stage1' by default. You can change this in "UserSettings".
stage1Packages :: Action [Package]
stage1Packages = do
    win        <- windowsHost
    intLib     <- integerLibrary =<< flavour
    libraries0 <- filter isLibrary <$> stage0Packages
    cross      <- flag CrossCompiling
    return $ libraries0 -- Build all Stage0 libraries in Stage1
          ++ [ array
             , base
             , bytestring
             , containers
             , deepseq
             , directory
             , filepath
             , ghc
             , ghcCompact
             , ghcPkg
             , ghcPrim
             , haskeline
             , hsc2hs
             , intLib
             , pretty
             , process
             , rts
             , stm
             , time
             , unlit
             , xhtml                         ]
          ++ [ hpcBin   | not cross          ]
          ++ [ iserv    | not win, not cross ]
          ++ [ libiserv | not win, not cross ]
          ++ [ runGhc   | not cross          ]
          ++ [ touchy   | win                ]
          ++ [ unix     | not win            ]
          ++ [ win32    | win                ]

-- | Packages built in 'Stage2' by default. You can change this in "UserSettings".
stage2Packages :: Action [Package]
stage2Packages = do
    cross <- flag CrossCompiling
    return $ [ ghcTags             ]
          ++ [ haddock | not cross ]

-- | Packages that are built only for the testsuite.
testsuitePackages :: Action [Package]
testsuitePackages = do
    win <- windowsHost
    return $ [ checkApiAnnotations
             , checkPpr
             , ghci
             , ghcPkg
             , hp2ps
             , iserv
             , parallel
             , runGhc        ] ++
             [ timeout | win ]
