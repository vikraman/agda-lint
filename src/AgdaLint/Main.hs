-- command line
module AgdaLint.Main (defaultMain) where

import           AgdaLint.Config
import           AgdaLint.Report
import           AgdaLint.Rule
import           AgdaLint.Run
import           AgdaLint.Types
import           Control.Exception   (IOException, handle)
import           Control.Monad       (forM_, unless, when)
import           Data.List           (find)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           GHC.Generics        (Generic)
import           Options.Applicative
import           System.Directory    (canonicalizePath, getCurrentDirectory)
import           System.Environment  (getArgs, getProgName, lookupEnv)
import           System.Exit         (ExitCode (..), exitSuccess, exitWith)
import           System.FilePath     (makeRelative)
import           Data.Version        (showVersion)
import           GHC.IO.Encoding     (setLocaleEncoding)
import           Paths_agda_lint     (version)
import           System.IO           (hPutStrLn, hSetEncoding, stderr, stdout,
                                      utf8)

data FailOn = OnError | OnWarning | Never
  deriving (Eq, Show, Enum, Bounded, Generic)

data Options = Options
  { optFix       :: Bool
  , optNoWarn    :: Bool
  , optListRules :: Bool
  , optConfig    :: Maybe FilePath
  , optFormat    :: Output
  , optFailOn    :: FailOn
  , optPaths     :: [FilePath]
  }

options :: Parser Options
options =
  Options
    <$> switch (long "fix" <> help "rewrite fixable findings in place")
    <*> switch (long "no-warn" <> help "only report errors")
    <*> switch (long "list-rules" <> help "list rules with their configured level")
    <*> optional (strOption (long "config" <> metavar "FILE" <> help "config file (default: agda-lint.toml)"))
    <*> option (enum camelName) (long "format" <> metavar "FORMAT" <> value Text <> help "output format: text or github (default: text)")
    <*> option (enum failName) (long "fail-on" <> metavar "LEVEL" <> value OnError <> help "exit with 1 on: error, warning or never (default: error)")
    <*> many (strArgument (metavar "PATH..." <> help "only report on these files or directories"))
  where
    failName = T.toLower . T.replace "On" "" . T.pack . show

-- option value by name
enum :: forall a. (Bounded a, Enum a) => (a -> T.Text) -> ReadM a
enum name = eitherReader $ \s ->
  maybe (Left ("expected one of " <> T.unpack (T.intercalate ", " (map name values)))) Right (find ((== T.pack s) . name) values)
  where
    values = [minBound .. maxBound] :: [a]

-- usage errors exit with 2
parseOptions :: IO Options
parseOptions = do
  args <- getArgs
  prog <- getProgName
  case execParserPure defaultPrefs parser args of
    Failure f | (msg, ExitFailure _) <- renderFailure f prog -> hPutStrLn stderr msg >> exitWith (ExitFailure 2)
    result -> handleParseResult result
  where
    parser = info (options <**> helper <**> simpleVersioner (showVersion version)) (fullDesc <> progDesc "Lint agda sources, configured by agda-lint.toml in the current directory")

-- lint with the given rules
defaultMain :: [Rule] -> IO ()
defaultMain rules = handle unreadable . handle failure $ do
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  opts <- parseOptions
  root <- getCurrentDirectory
  proj <- loadProject rules root (optConfig opts) (optPaths opts)
  when (optListRules opts) $ do
    forM_ (projRules proj) $ \(Active r lvl _) ->
      TIO.putStrLn (T.justifyLeft 24 ' ' (ruleName r) <> T.justifyLeft 9 ' ' (camelName lvl) <> ruleSummary r)
    exitSuccess
  when (optFix opts) $ do
    settled <- fixProject proj
    unless settled $ hPutStrLn stderr "agda-lint: fixes did not settle, some keep changing the sources"
  files <- loadSources proj
  when (null files) $ hPutStrLn stderr "agda-lint: no sources found"
  forM_ (unmatchedPaths proj files) $ \p -> hPutStrLn stderr ("agda-lint: " <> p <> ": no sources here")
  let diags = lintFiles proj files
  workspace <- traverse canonicalizePath =<< lookupEnv "GITHUB_WORKSPACE"
  here <- canonicalizePath root
  let shown = if optNoWarn opts then filter ((== Error) . diagLevel) diags else diags
      errors = length (filter ((== Error) . diagLevel) diags)
      prefix = maybe "" (`makeRelative` here) workspace
      failed = case optFailOn opts of
        OnError   -> errors > 0
        OnWarning -> not (null shown)
        Never     -> False
  mapM_ (TIO.putStrLn . report (optFormat opts) prefix) shown
  hPutStrLn stderr (show errors <> " error(s), " <> show (length shown - errors) <> " warning(s)")
  exitWith (if failed then ExitFailure 1 else ExitSuccess)
  where
    failure (LintError msg) = hPutStrLn stderr ("agda-lint: " <> msg) >> exitWith (ExitFailure 2)
    unreadable e = hPutStrLn stderr ("agda-lint: " <> show (e :: IOException)) >> exitWith (ExitFailure 2)
