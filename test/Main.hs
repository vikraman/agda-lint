module Main (main) where

import           AgdaLint           hiding (defaultMain)
import           AgdaLint.Config
import           AgdaLint.Fix
import           AgdaLint.Report
import           AgdaLint.Run
import           AgdaLint.Source    (analyse)
import           AgdaLint.Types     (Diagnostic (..))
import           Control.Exception  (try)
import           Control.Monad      (filterM, forM, forM_, unless, when)
import qualified Data.ByteString    as BS
import           Data.List          (isInfixOf, isPrefixOf, isSuffixOf, sort)
import           Data.Maybe         (isJust)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           GHC.Generics       (Generic)
import           System.Directory
import           System.Environment (lookupEnv)
import           System.Exit        (ExitCode (..))
import           System.FilePath    (dropExtension, makeRelative, takeDirectory,
                                     (</>))
import           System.Process     (CreateProcess (..), proc,
                                     readCreateProcessWithExitCode)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Toml.Schema        (FromValue (..), genericFromTable)

fixtures :: FilePath
fixtures = "test/fixtures"

main :: IO ()
main = do
  cases <- sort <$> listDirectory fixtures
  accept <- isJust <$> lookupEnv "AGDA_LINT_ACCEPT"
  defaultMain $
    testGroup
      "agda-lint"
      [ testGroup "fixtures" (map (golden accept) cases)
      , testGroup "config" configTests
      , testGroup "custom" customTests
      , testGroup "report" reportTests
      , testGroup "fix" fixTests
      , testGroup "cli" cliTests
      ]

-- files below a directory, relative to it
filesIn :: FilePath -> IO [FilePath]
filesIn dir = do
  entries <- map (dir </>) <$> listDirectory dir
  files <- filterM doesFileExist entries
  dirs <- filterM doesDirectoryExist entries
  (files ++) . concat <$> mapM filesIn dirs

-- copy of a fixture to work on
scratch :: FilePath -> IO FilePath
scratch fixture = scratchAs fixture fixture

scratchAs :: String -> FilePath -> IO FilePath
scratchAs label fixture = do
  tmp <- getTemporaryDirectory
  let dst = tmp </> "agda-lint-test" </> label
  exists <- doesDirectoryExist dst
  when exists (removeDirectoryRecursive dst)
  files <- filesIn (fixtures </> fixture)
  forM_ files $ \f -> do
    let to = dst </> makeRelative (fixtures </> fixture) f
    createDirectoryIfMissing True (takeDirectory to)
    copyFile f to
  pure dst

-- expected.txt is the report, x.fixed is x after --fix
golden :: Bool -> FilePath -> TestTree
golden accept fixture = testCase fixture $ do
  dir <- scratch fixture
  proj <- loadProject builtinRules dir Nothing []
  out <- T.unlines . map (report Text "") <$> lintProject proj
  settled <- fixProject proj
  assertBool "fixes settle" settled
  sources <- filter (not . (".fixed" `isSuffixOf`)) <$> filesIn dir
  changed <- fmap concat . forM sources $ \f -> do
    before <- TIO.readFile (fixtures </> fixture </> makeRelative dir f)
    now <- TIO.readFile f
    pure [(makeRelative dir f <> ".fixed", now) | before /= now]
  let outputs = ("expected.txt", out) : changed
  if accept
    then do
      old <- filter (".fixed" `isSuffixOf`) <$> filesIn (fixtures </> fixture)
      mapM_ removeFile old
      forM_ outputs $ \(f, t) -> TIO.writeFile (fixtures </> fixture </> f) t
    else do
      forM_ outputs $ \(f, t) -> do
        exists <- doesFileExist (fixtures </> fixture </> f)
        assertBool (f <> " is missing") exists
        want <- TIO.readFile (fixtures </> fixture </> f)
        assertEqual f want t
      stale <- filter (".fixed" `isSuffixOf`) <$> filesIn (fixtures </> fixture)
      forM_ stale $ \f ->
        assertBool (f <> " is stale") (makeRelative (fixtures </> fixture) f `elem` map fst changed)
  -- fixing again changes nothing
  _ <- fixProject proj
  forM_ changed $ \(f, t) -> do
    again <- TIO.readFile (dir </> dropExtension f)
    assertEqual (f <> " is not a fixpoint") t again

levels :: [Rule] -> Text -> Either String [(Text, Level)]
levels rs src = do
  (_, active) <- parseSettings src >>= resolve rs
  pure [(ruleName (activeRule a), activeLevel a) | a <- active]

fails :: String -> Text -> Assertion
fails want src = case levels builtinRules src of
  Left e   -> unless (want `isInfixOf` e) (assertFailure ("expected " <> want <> ", got " <> e))
  Right _ -> assertFailure ("expected " <> want <> ", but the config was accepted")

configTests :: [TestTree]
configTests =
  [ testCase "core preset" $ do
      ls <- either assertFailure pure (levels builtinRules "")
      lookup "asciiArrow" ls @?= Just Error
      lookup "trailingWhitespace" ls @?= Just Warning
      lookup "letBinding" ls @?= Just Off
  , testCase "all preset" $ do
      ls <- either assertFailure pure (levels builtinRules "preset = \"all\"\n[rules]\ntab = \"off\"")
      lookup "letBinding" ls @?= Just Warning
      lookup "tab" ls @?= Just Off
  , testCase "rule level" $ do
      ls <- either assertFailure pure (levels builtinRules "[rules]\nletBinding = \"error\"")
      lookup "letBinding" ls @?= Just Error
  , testCase "unknown key" $ fails "unknown keys in config: nope" "nope = 1"
  , testCase "unknown rule" $ fails "unknown rules in config: nope" "[rules]\nnope = \"off\""
  , testCase "unknown level" $ fails "expected one of error, warning, off" "[rules]\ntab = \"loud\""
  , testCase "unknown preset" $ fails "expected one of core, all" "preset = \"some\""
  , testCase "bad pair" $ fails "pairs must be two letters" "pairs = [\"MNP\"]"
  , testCase "bad sort" $ fails "exactly one of heads or exact" "[[sorts]]\nletters = \"V\""
  , testCase "bad style" $ fails "sectionTitle" "[style]\nsectionTitle = \"upper\""
  , testCase "unknown style key" $ fails "width" "[style]\nwidth = 3"
  , testCase "pattern without matcher" $ fails "pattern p: needs exactly one of token or regex" "[[pattern]]\nname = \"p\""
  , testCase "pattern with bad regex" $ fails "pattern p: bad regex" "[[pattern]]\nname = \"p\"\nregex = \"(\""
  , testCase "pattern clashing with a rule" $ fails "duplicate rules: tab" "[[pattern]]\nname = \"tab\"\ntoken = \"x\""
  , testCase "pattern without a name" $ fails "needs a name" "[[pattern]]\nname = \"\"\ntoken = \"x\""
  , testCase "pattern replacing with itself" $ fails "pattern p: replace matches the pattern itself" "[[pattern]]\nname = \"p\"\nregex = \"a+\"\nreplace = \"ba\""
  , testCase "bad line length" $ fails "must be positive" "[style]\nlineLength = 0"
  , testCase "sort without letters" $ fails "a sort needs letters" "[[sorts]]\nexact = [\"Ty\"]\nletters = \"\""
  , testCase "bad allow entries" $
      forM_ ["A.agda:", ":3", "A.agda:x", ""] $ \e ->
        fails "expected path or path:line" ("[allow]\ntab = [\"" <> e <> "\"]")
  , testCase "unsupported literate format" $
      either (assertBool "message" . ("unsupported literate format" `isInfixOf`)) (const (assertFailure "accepted")) (analyse "A.lagda.rst" "")
  , testCase "format by file name" $
      either assertFailure (const (pure ())) (analyse "x.lagda.d/A.agda" "f : Set → Set")
  , testCase "syntax error" $ fails "" "sources = ["
  , testCase "unknown rule in allow" $ fails "unknown rules in config: nope" "[allow]\nnope = [\"A.agda\"]"
  ]

-- rule with an option and a two-edit fix
newtype Opts = Opts {maxPostulates :: Maybe Int}
  deriving (Generic)

instance FromValue Opts where fromValue = genericFromTable

fewPostulates :: Rule
fewPostulates = ruleWith "fewPostulates" Warning "at most maxPostulates postulates" ["maxPostulates"] $ \Opts {maxPostulates} _ f ->
  [ Finding (tokPos t) "too many postulates" [replaceTok t "module _", InsertLineAfter (lineNo l) "  -- todo"]
  | (i, (l, t)) <- zip [1 ..] [(l, t) | l <- codeLines f, t <- codeTokens l, tokText t == "postulate"]
  , i > maybe 0 id maxPostulates
  ]

customTests :: [TestTree]
customTests =
  [ testCase "off unless enabled" $ do
      ls <- either assertFailure pure (levels (builtinRules ++ [fewPostulates]) "maxPostulates = 1")
      lookup "fewPostulates" ls @?= Just Off
  , testCase "option is validated" $
      case levels (builtinRules ++ [fewPostulates]) "maxPostulates = \"one\"" of
        Left e  -> assertBool e ("fewPostulates" `isInfixOf` e)
        Right _ -> assertFailure "config was accepted"
  , testCase "key needs the rule" $ fails "unknown keys in config: maxPostulates" "maxPostulates = 1"
  , testCase "finds and fixes" $ do
      dir <- scratchAs "t1" "pattern"
      TIO.writeFile (dir </> "agda-lint.toml") "maxPostulates = 0\n[rules]\nfewPostulates = \"error\"\n"
      proj <- loadProject [fewPostulates] dir Nothing []
      ds <- lintProject proj
      map (report Text "") ds @?= ["Bad.agda:3:1: error [fewPostulates] too many postulates"]
      _ <- fixProject proj
      fixedSrc <- TIO.readFile (dir </> "Bad.agda")
      take 3 (drop 2 (T.lines fixedSrc)) @?= ["module _", "  -- todo", "  Nat : Set"]
  , testCase "paths restrict the report" $ do
      dir <- scratchAs "t2" "allow"
      whole <- lintProject =<< loadProject builtinRules dir Nothing []
      sub <- lintProject =<< loadProject builtinRules dir Nothing ["Sub"]
      one <- lintProject =<< loadProject builtinRules dir Nothing ["./A.agda"]
      assertBool "sub" (not (null sub) && all ((== "Sub/B.agda") . diagFile) sub)
      assertBool "one" (not (null one) && all ((== "A.agda") . diagFile) one)
      length whole @?= length sub + length one
  , testCase "paths are resolved" $ do
      dir <- scratchAs "t3" "allow"
      one <- lintProject =<< loadProject builtinRules dir Nothing ["A.agda"]
      forM_ [dir </> "A.agda", "Sub/../A.agda", "./Sub/..//A.agda"] $ \p -> do
        ds <- lintProject =<< loadProject builtinRules dir Nothing [p]
        assertEqual p one ds
      whole <- lintProject =<< loadProject builtinRules dir Nothing []
      forM_ [".", "./", dir] $ \p -> do
        ds <- lintProject =<< loadProject builtinRules dir Nothing [p]
        assertEqual p whole ds
  , testCase "bad paths are errors" $ do
      dir <- scratchAs "t4" "allow"
      forM_ [("nope.agda", "no such file"), ("..", "outside the project")] $ \(p, want) -> do
        r <- try (loadProject builtinRules dir Nothing [p])
        case r of
          Left (LintError e) -> assertBool e (want `isInfixOf` e)
          Right _            -> assertFailure (p <> " was accepted")
  ]

fixTests :: [TestTree]
fixTests =
  [ testCase "inserts keep their order" $
      fixLines [[InsertLineAfter 1 "a"], [InsertLineAfter 1 "b"]] ["x", "y"] @?= ["x", "a", "b", "y"]
  , testCase "insert at the top and after a deleted line" $
      fixLines [[InsertLineAfter 0 "top"], [DeleteLine 1, InsertLineAfter 1 "new"]] ["x", "y"] @?= ["top", "new", "y"]
  , testCase "overlapping spans, the first wins" $
      fixLines [[ReplaceSpan (Pos 1 0) 3 "A"], [ReplaceSpan (Pos 1 2) 3 "B"], [ReplaceSpan (Pos 1 5) 1 "C"]] ["abcdef"] @?= ["AdeC"]
  , testCase "a group is all or nothing" $
      fixLines [[ReplaceLine 1 "X"], [ReplaceSpan (Pos 1 0) 1 "y", InsertLineAfter 2 "z"]] ["a", "b"] @?= ["X", "b"]
  , testCase "the same edit twice is one edit" $
      fixLines [[DeleteLine 1, InsertLineAfter 2 "z"], [DeleteLine 1]] ["a", "b"] @?= ["b", "z"]
  , testCase "out of range edits are dropped" $
      fixLines [[ReplaceSpan (Pos 1 2) 5 "x"], [DeleteLine 3], [ReplaceLine 0 "x"], [InsertLineAfter 9 "x"]] ["abc", "d"] @?= ["abc", "d"]
  , testCase "crlf is kept" $
      fst (fixText [[ReplaceLine 1 "x"], [InsertLineAfter 1 "y"]] "a\r\nb\r\n") @?= "x\r\ny\r\nb\r\n"
  , testCase "line origins" $
      lineOrigins [InsertLineAfter 0 "t", InsertLineAfter 1 "a", DeleteLine 3] 4 @?= [Nothing, Just 1, Nothing, Just 2, Just 4]
  , testCase "allowed lines survive earlier fixes" $ do
      dir <- scratchAs "t5" "allow"
      TIO.writeFile (dir </> "agda-lint.toml") "[rules]\nmoduleBlankLine = \"warning\"\n[allow]\nasciiArrow = [\"A.agda:2\"]\n"
      TIO.writeFile (dir </> "A.agda") "module A where\nf : Set -> Set\ng : Set -> Set\n"
      _ <- fixProject =<< loadProject builtinRules dir Nothing ["A.agda"]
      result <- TIO.readFile (dir </> "A.agda")
      result @?= "module A where\n\nf : Set -> Set\ng : Set → Set\n"
  ]

fixLines :: [[Fix]] -> [Text] -> [Text]
fixLines groups ls = applyFixes (acceptFixes ls groups) ls

-- the built executable
run :: FilePath -> [String] -> IO (ExitCode, String, String)
run dir args = readCreateProcessWithExitCode (proc "agda-lint" args) {cwd = Just dir} ""

cliTests :: [TestTree]
cliTests =
  [ testCase "findings exit with 1" $ do
      dir <- scratchAs "t6" "core"
      (code, out, err) <- run dir []
      code @?= ExitFailure 1
      assertBool out ("Bad.agda:6:9: error [asciiArrow]" `isInfixOf` out)
      assertBool err ("7 error(s), 2 warning(s)" `isInfixOf` err)
  , testCase "warnings alone exit with 0" $ do
      dir <- scratchAs "t7" "core"
      writeFile (dir </> "Bad.agda") "x = 1 \n"
      (code, _, _) <- run dir []
      code @?= ExitSuccess
      (strict, _, _) <- run dir ["--fail-on", "warning"]
      strict @?= ExitFailure 1
      (quiet, out, _) <- run dir ["--fail-on", "warning", "--no-warn"]
      quiet @?= ExitSuccess
      out @?= ""
  , testCase "never fails" $ do
      dir <- scratchAs "t8" "core"
      (code, _, _) <- run dir ["--fail-on", "never"]
      code @?= ExitSuccess
  , testCase "bad usage, config, path and source exit with 2" $ do
      dir <- scratchAs "t9" "core"
      forM_ [["--bogus"], ["--config", "nope.toml"], ["nope.agda"]] $ \args -> do
        (code, _, _) <- run dir args
        assertEqual (unwords args) (ExitFailure 2) code
      BS.writeFile (dir </> "Bytes.agda") (BS.pack [45, 45, 32, 255, 254, 10])
      (code, _, err) <- run dir []
      code @?= ExitFailure 2
      assertBool err ("agda-lint: " `isPrefixOf` err)
  , testCase "annotations from the workspace root" $ do
      dir <- scratchAs "t10" "core"
      (_, out, _) <- readCreateProcessWithExitCode (proc "agda-lint" ["--format", "github"]) {cwd = Just dir, env = Just [("GITHUB_WORKSPACE", dir)]} ""
      assertBool out ("::warning file=Bad.agda,line=4," `isInfixOf` out)
  , testCase "version" $ do
      (code, out, _) <- run "." ["--version"]
      code @?= ExitSuccess
      assertBool out (all (`elem` ("0123456789.\n" :: String)) out && not (null out))
  ]

reportTests :: [TestTree]
reportTests =
  [ testCase "text" $ report Text "" d @?= "A/B.agda:3:5: error [asciiArrow] use → instead of ->"
  , testCase "github" $ report Github "sub" d @?= "::error file=sub/A/B.agda,line=3,col=5,title=agda-lint%3A asciiArrow::use → instead of ->"
  , testCase "github at the root" $ report Github "." d @?= "::error file=A/B.agda,line=3,col=5,title=agda-lint%3A asciiArrow::use → instead of ->"
  , testCase "github escapes" $
      report Github "" (Diagnostic "a,b.agda" (finding (Pos 1 0) "100%\nsure") "r" Warning)
        @?= "::warning file=a%2Cb.agda,line=1,col=1,title=agda-lint%3A r::100%25%0Asure"
  ]
  where
    d = Diagnostic "A/B.agda" (finding (Pos 3 4) "use → instead of ->") "asciiArrow" Error
