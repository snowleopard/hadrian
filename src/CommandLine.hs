module CommandLine (
    optDescrs, cmdLineArgsMap, cmdFlavour, lookupFreeze1, cmdIntegerSimple,
    cmdProgressColour, cmdProgressInfo, cmdConfigure, cmdSplitObjects,
    cmdInstallDestDir, cmdTestArgs
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
    { configure      :: Bool
    , flavour        :: Maybe String
    , freeze1        :: Bool
    , installDestDir :: Maybe String
    , integerSimple  :: Bool
    , progressColour :: UseColour
    , progressInfo   :: ProgressInfo
    , splitObjects   :: Bool
    , testArgs       :: TestArgs }
    deriving (Eq, Show)

-- | Default values for 'CommandLineArgs'.
defaultCommandLineArgs :: CommandLineArgs
defaultCommandLineArgs = CommandLineArgs
    { configure      = False
    , flavour        = Nothing
    , freeze1        = False
    , installDestDir = Nothing
    , integerSimple  = False
    , progressColour = Auto
    , progressInfo   = Brief
    , splitObjects   = False
    , testArgs       = defaultTestArgs }

readConfigure :: Either String (CommandLineArgs -> CommandLineArgs)
readConfigure = Right $ \flags -> flags { configure = True }

readFlavour :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readFlavour ms = Right $ \flags -> flags { flavour = lower <$> ms }

readFreeze1 :: Either String (CommandLineArgs -> CommandLineArgs)
readFreeze1 = Right $ \flags -> flags { freeze1 = True }

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

readSplitObjects :: Either String (CommandLineArgs -> CommandLineArgs)
readSplitObjects = Right $ \flags -> flags { splitObjects = True }

readTestOnly :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestOnly tests = Right $ \flags -> flags { testArgs = (testArgs flags) { testOnly = tests } }

readTestSkipPerf :: Either String (CommandLineArgs -> CommandLineArgs)
readTestSkipPerf = Right $ \flags -> flags { testArgs = (testArgs flags) { testSkipPerf = True } }

readTestSummary :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestSummary filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testJUnit = filepath } }

readTestJUnit :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestJUnit filepath = Right $ \flags -> flags { testArgs = (testArgs flags) { testJUnit = filepath } }

readTestConfig :: Maybe String -> Either String (CommandLineArgs -> CommandLineArgs)
readTestConfig config =
    case config of
         Nothing -> Right id
         Just conf -> Right $ \flags ->
                        let configs = conf : testConfigs (testArgs flags)
                         in flags { testArgs = (testArgs flags) { testConfigs = configs } }

-- | Standard 'OptDescr' descriptions of Hadrian's command line arguments.
optDescrs :: [OptDescr (Either String (CommandLineArgs -> CommandLineArgs))]
optDescrs =
    [ Option ['c'] ["configure"] (NoArg readConfigure)
      "Run the boot and configure scripts (if you do not want to run them manually)."
    , Option [] ["flavour"] (OptArg readFlavour "FLAVOUR")
      "Build flavour (Default, Devel1, Devel2, Perf, Prof, Quick or Quickest)."
    , Option [] ["freeze1"] (NoArg readFreeze1)
      "Freeze Stage1 GHC."
    , Option [] ["install-destdir"] (OptArg readInstallDestDir "DESTDIR")
      "Installation destination directory."
    , Option [] ["integer-simple"] (NoArg readIntegerSimple)
      "Build GHC with integer-simple library."
    , Option [] ["progress-colour"] (OptArg readProgressColour "MODE")
      "Use colours in progress info (Never, Auto or Always)."
    , Option [] ["progress-info"] (OptArg readProgressInfo "STYLE")
      "Progress info style (None, Brief, Normal or Unicorn)."
    , Option [] ["split-objects"] (NoArg readSplitObjects)
      "Generate split objects (requires a full clean rebuild)."
    , Option [] ["test-only"] (OptArg readTestOnly "TEST_CASE")
      "Test cases to run."
    , Option [] ["test-skip-perf"] (NoArg readTestSkipPerf)
      "Skip performance tests."
    , Option [] ["test-summary"] (OptArg readTestSummary "TEST_SUMMARY")
      "Where to output the test summary file."
    , Option [] ["test-junit"] (OptArg readTestJUnit "TEST_JUNIT")
      "Output testsuite summary in JUnit format."
    , Option [] ["test-config"] (OptArg readTestConfig "EXTRA_TEST_CONFIG")
      "Configurations to run test, in key=value format." ]

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

cmdConfigure :: Action Bool
cmdConfigure = configure <$> cmdLineArgs

cmdFlavour :: Action (Maybe String)
cmdFlavour = flavour <$> cmdLineArgs

lookupFreeze1 :: Map.HashMap TypeRep Dynamic -> Bool
lookupFreeze1 = freeze1 . lookupExtra defaultCommandLineArgs

cmdInstallDestDir :: Action (Maybe String)
cmdInstallDestDir = installDestDir <$> cmdLineArgs

cmdIntegerSimple :: Action Bool
cmdIntegerSimple = integerSimple <$> cmdLineArgs

cmdProgressColour :: Action UseColour
cmdProgressColour = progressColour <$> cmdLineArgs

cmdProgressInfo :: Action ProgressInfo
cmdProgressInfo = progressInfo <$> cmdLineArgs

cmdSplitObjects :: Action Bool
cmdSplitObjects = splitObjects <$> cmdLineArgs

cmdTestArgs :: Action TestArgs
cmdTestArgs = testArgs <$> cmdLineArgs
