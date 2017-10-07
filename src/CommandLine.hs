module CommandLine (
    optDescrs, cmdLineArgsMap, cmdBuildHaddock, cmdFlavour, cmdIntegerSimple,
    cmdProgressColour, cmdProgressInfo, cmdSkipConfigure, cmdSplitObjects,
    cmdInstallDestDir
    ) where

import Data.Either
import qualified Data.HashMap.Strict as Map
import Data.List.Extra
import Development.Shake hiding (Normal)
import Hadrian.Utilities
import System.Console.GetOpt
import System.Environment

-- | All arguments that can be passed to Hadrian via the command line.
data CommandLineArgs = CommandLineArgs
    { buildHaddock   :: Bool
    , flavour        :: Maybe String
    , installDestDir :: Maybe String
    , integerSimple  :: Bool
    , progressColour :: UseColour
    , progressInfo   :: ProgressInfo
    , skipConfigure  :: Bool
    , splitObjects   :: Bool }
    deriving (Eq, Show)

-- | Default values for 'CommandLineArgs'.
defaultCommandLineArgs :: CommandLineArgs
defaultCommandLineArgs = CommandLineArgs
    { buildHaddock   = False
    , flavour        = Nothing
    , installDestDir = Nothing
    , integerSimple  = False
    , progressColour = Auto
    , progressInfo   = Normal
    , skipConfigure  = False
    , splitObjects   = False }

readBuildHaddock :: Either String (CommandLineArgs -> CommandLineArgs)
readBuildHaddock = Right $ \flags -> flags { buildHaddock = True }

readFlavour :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readFlavour ms = Right $ \flags -> flags { flavour = lower <$> ms }

readInstallDestDir :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readInstallDestDir ms = Right $ \flags -> flags { installDestDir = ms }

readIntegerSimple :: Either String (CommandLineArgs -> CommandLineArgs)
readIntegerSimple = Right $ \flags -> flags { integerSimple = True }

readProgressColour :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readProgressColour ms =
    maybe (Left "Cannot parse progress-colour") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe UseColour
    go "never"   = Just Never
    go "auto"    = Just Auto
    go "always"  = Just Always
    go _         = Nothing
    set :: UseColour -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { progressColour = flag }

readProgressInfo :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readProgressInfo ms =
    maybe (Left "Cannot parse progress-info") (Right . set) (go =<< lower <$> ms)
  where
    go :: String -> Maybe ProgressInfo
    go "none"    = Just None
    go "brief"   = Just Brief
    go "normal"  = Just Normal
    go "unicorn" = Just Unicorn
    go _         = Nothing
    set :: ProgressInfo -> CommandLineArgs -> CommandLineArgs
    set flag flags = flags { progressInfo = flag }

readSkipConfigure :: Either String (CommandLineArgs -> CommandLineArgs)
readSkipConfigure = Right $ \flags -> flags { skipConfigure = True }

readSplitObjects :: Either String (CommandLineArgs -> CommandLineArgs)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

-- | Standard 'OptDescr' descriptions of Hadrian's command line arguments.
optDescrs :: [OptDescr (Either String (CommandLineArgs -> CommandLineArgs))]
optDescrs =
    [ Option [] ["flavour"] (OptArg readFlavour "FLAVOUR") $
      "Build flavour (default, devel1, devel2, perf, prof, " ++
      "quick, quickest, quick-cross or validate)."
    , Option [] ["haddock"] (NoArg readBuildHaddock)
      "Generate Haddock documentation."
    , Option [] ["install-destdir"] (OptArg readInstallDestDir "DESTDIR")
      "Installation destination directory."
    , Option [] ["integer-simple"] (NoArg readIntegerSimple)
      "Build GHC with integer-simple library."
    , Option [] ["progress-colour"] (OptArg readProgressColour "MODE")
      "Use colours in progress info (Never, Auto or Always)."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal or Unicorn)."
    , Option [] ["skip-configure"] (NoArg readSkipConfigure)
      "Skip the boot and configure scripts (if you want to run them manually)."
    , Option [] ["split-objects"] (NoArg readSplitObjects)
      "Generate split objects (requires a full clean rebuild)." ]

-- | A type-indexed map containing Hadrian command line arguments to be passed
-- to Shake via 'shakeExtra'.
cmdLineArgsMap :: IO (Map.HashMap TypeRep Dynamic)
cmdLineArgsMap = do
    (opts, _, _) <- getOpt Permute optDescrs <$> getArgs
    let args = foldl (flip id) defaultCommandLineArgs (rights opts)
    return $ insertExtra (progressColour args) -- Accessed by Hadrian.Utilities
           $ insertExtra (progressInfo   args) -- Accessed by Hadrian.Utilities
           $ insertExtra args Map.empty

cmdLineArgs :: Action CommandLineArgs
cmdLineArgs = userSetting defaultCommandLineArgs

cmdBuildHaddock :: Action Bool
cmdBuildHaddock = buildHaddock <$> cmdLineArgs

cmdFlavour :: Action (Maybe String)
cmdFlavour = flavour <$> cmdLineArgs

cmdInstallDestDir :: Action (Maybe String)
cmdInstallDestDir = installDestDir <$> cmdLineArgs

cmdIntegerSimple :: Action Bool
cmdIntegerSimple = integerSimple <$> cmdLineArgs

cmdProgressColour :: Action UseColour
cmdProgressColour = progressColour <$> cmdLineArgs

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> cmdLineArgs

cmdSkipConfigure :: Action Bool
cmdSkipConfigure = skipConfigure <$> cmdLineArgs

cmdSplitObjects :: Action Bool
cmdSplitObjects = splitObjects <$> cmdLineArgs
