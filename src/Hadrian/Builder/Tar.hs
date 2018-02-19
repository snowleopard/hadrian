-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Builder.Tar
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Support for invoking the archiving utility @tar@.
-----------------------------------------------------------------------------
module Hadrian.Builder.Tar (TarMode (..), args) where

import Development.Shake
import Hadrian.Builder.Mode
import Hadrian.Expression

-- | Default command line arguments for invoking the archiving utility @tar@.
args :: (ShakeValue c, ShakeValue b) => TarMode -> Args c b
args Create = mconcat
    [ arg "-c"
    , output "//*.gz"  ? arg "--gzip"
    , output "//*.bz2" ? arg "--bzip2"
    , output "//*.xz"  ? arg "--xz"
    , arg "-f", arg =<< getOutput
    , getInputs ]
args Extract = mconcat
    [ arg "-x"
    , input "*.gz"  ? arg "--gzip"
    , input "*.bz2" ? arg "--bzip2"
    , input "*.xz"  ? arg "--xz"
    , arg "-f", arg =<< getInput
    , arg "-C", arg =<< getOutput ]
