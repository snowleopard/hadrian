-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Builder.Sphinx
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Support for invoking the documentation utility Sphinx.
-----------------------------------------------------------------------------
module Hadrian.Builder.Sphinx (SphinxMode (..), args) where

import Development.Shake
import Hadrian.Builder.Type
import Hadrian.Expression
import Hadrian.Utilities

-- | Default command line arguments for invoking the archiving utility @tar@.
args :: (ShakeValue c, ShakeValue b) => SphinxMode -> Args c b
args mode = do
    outPath <- getOutput
    mconcat [ arg "-b", arg modeString
            , arg "-d", arg $ outPath -/- (".doctrees-" ++ modeString)
            , arg =<< getInput
            , arg outPath ]
  where
    modeString = case mode of
        Html  -> "html"
        Latex -> "latex"
        Man   -> "man"
