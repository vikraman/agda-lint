-- rules declared in the config
module AgdaLint.Pattern (patternRules) where

import           AgdaLint.Lexer
import           AgdaLint.Rule
import           AgdaLint.Source      hiding (name)
import           AgdaLint.Types
import           Control.Monad        (when)
import           Data.Bifunctor       (first)
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Text.Regex.TDFA      (defaultCompOpt, defaultExecOpt,
                                       getAllMatches, match)
import           Text.Regex.TDFA.Text (compile)
import           Toml.Schema

data RawPattern = RawPattern
  { name    :: Text
  , level   :: Maybe Level
  , token   :: Maybe Text
  , regex   :: Maybe Text
  , scope   :: Maybe Text
  , message :: Maybe Text
  , replace :: Maybe Text
  }
  deriving (Generic)

instance FromValue RawPattern where fromValue = genericFromTable

newtype Patterns = Patterns [RawPattern]

instance FromValue Patterns where
  fromValue = parseTableFromValue (Patterns . fromMaybe [] <$> optKey "pattern")

patternRules :: Settings -> Either String [Rule]
patternRules settings = do
  Patterns raws <- decodeSettings (restrictSettings ["pattern"] settings)
  traverse patternRule raws

patternRule :: RawPattern -> Either String Rule
patternRule RawPattern {name, level, token, regex, scope, message, replace} = inRule $ do
  byLine <- case scope of
    Nothing      -> Right False
    Just "token" -> Right False
    Just "line"  -> Right True
    Just s       -> Left ("unknown scope " <> show s <> ", expected one of token, line")
  when (byLine && isJust token) $ Left "scope = \"line\" needs a regex"
  spans <- case (token, regex) of
    -- offset and length of each match
    (Just t, Nothing) -> Right (\x -> [(0, T.length x) | x == t])
    (Nothing, Just r) -> do
      re <- first (("bad regex: " <>)) (compile defaultCompOpt defaultExecOpt r)
      Right (\x -> [(o, n) | (o, n) <- getAllMatches (match re x), n > 0])
    _ -> Left "needs exactly one of token or regex"
  when (T.null name) $ Left "needs a name"
  -- the fix would apply forever
  when (any (not . null . spans) replace) $ Left "replace matches the pattern itself"
  let source = fromMaybe "" (token <|> regex)
  pure
    Rule
      { ruleName = name
      , ruleLevel = fromMaybe Warning level
      , ruleCore = True
      , ruleSummary = fromMaybe ("pattern " <> source) message
      , ruleKeys = []
      , ruleSetup = const (Right (check byLine spans))
      }
  where
    inRule = first (("pattern " <> T.unpack name <> ": ") <>)
    a <|> b = maybe b Just a
    check byLine spans _ f
      | byLine = [found (Pos (lineNo l) o) (T.take n (T.drop o (lineText l))) | l <- blockLines f, (o, n) <- spans (lineText l)]
      | otherwise =
          [ found (tokPos t) {posCol = posCol (tokPos t) + o} (T.take n (T.drop o (tokText t)))
          | l <- codeLines f
          , t <- codeTokens l
          , posLine (tokPos t) == tokEndLine t
          , (o, n) <- spans (tokText t)
          ]
    found pos matched = Finding pos (describe matched) [ReplaceSpan pos (T.length matched) r | Just r <- [replace]]
    describe matched = case (message, replace) of
      (Just m, _)        -> m
      (Nothing, Just r)  -> "use " <> r <> " instead of " <> matched
      (Nothing, Nothing) -> matched <> " is not allowed"
