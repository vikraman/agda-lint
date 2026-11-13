-- config
module AgdaLint.Config
  ( Config (..)
  , Preset (..)
  , Active (..)
  , configFile
  , parseSettings
  , resolve
  ) where

import           AgdaLint.Allow
import           AgdaLint.Pattern
import           AgdaLint.Rule
import           AgdaLint.Types
import           Control.Monad   (unless)
import           Data.Bifunctor  (first)
import           Data.List       (nub, (\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import qualified Toml
import           Toml.Schema

data Preset = Core | All
  deriving (Eq, Show, Enum, Bounded, Generic)

instance FromValue Preset where fromValue = enumFromValue

data Config = Config
  { cfgSources :: [String]
  , cfgExclude :: [String]
  , cfgPreset  :: Preset
  , cfgAllow   :: [Allow]
  }

-- toml config, keys as fields
data RawConfig = RawConfig
  { sources   :: Maybe [Text]
  , exclude   :: Maybe [Text]
  , preset    :: Maybe Preset
  , rules     :: Maybe (Map Text Level)
  , allow     :: Maybe (Map Text [Text])
  }
  deriving (Generic)

instance FromValue RawConfig where fromValue = genericFromTable

-- rule with its level and check
data Active = Active {activeRule :: Rule, activeLevel :: Level, activeCheck :: Check}

-- config in current directory
configFile :: FilePath
configFile = "agda-lint.toml"

coreKeys :: [Text]
coreKeys = ["sources", "exclude", "preset", "rules", "allow"]

parseSettings :: Text -> Either String Settings
parseSettings = Toml.parse

resolve :: [Rule] -> Settings -> Either String (Config, [Active])
resolve given settings = do
  patterns <- patternRules settings
  let allRules = given ++ patterns
      names = map ruleName allRules
      dupes = nub (names \\ nub names)
      unknownKeys = settingsKeys settings \\ ("pattern" : coreKeys ++ concatMap ruleKeys allRules)
  unless (null dupes) $ Left ("duplicate rules: " <> list dupes)
  unless (null unknownKeys) $ Left ("unknown keys in config: " <> list unknownKeys)
  RawConfig {sources, exclude, preset, rules, allow} <- decodeSettings (restrictSettings coreKeys settings)
  let levels = fromMaybe Map.empty rules
      allows = fromMaybe Map.empty allow
      unknownRules = nub (Map.keys levels ++ Map.keys allows) \\ names
  unless (null unknownRules) $ Left ("unknown rules in config: " <> list unknownRules)
  allowing <- sequence [parseAllow r e | (r, es) <- Map.toList allows, e <- es]
  let cfg =
        Config
          { cfgSources = maybe ["**/*.agda", "**/*.lagda", "**/*.lagda.tex", "**/*.lagda.md"] (map T.unpack) sources
          , cfgExclude = maybe ["_build/**"] (map T.unpack) exclude
          , cfgPreset = fromMaybe Core preset
          , cfgAllow = allowing
          }
      levelOf r
        | Just l <- Map.lookup (ruleName r) levels = l
        | cfgPreset cfg == All || ruleCore r = ruleLevel r
        | otherwise = Off
      activate r = do
        check <- first ((T.unpack (ruleName r) <> ": ") <>) (ruleSetup r (restrictSettings (ruleKeys r) settings))
        pure (Active r (levelOf r) check)
  active <- traverse activate allRules
  pure (cfg, active)
  where
    list = T.unpack . T.intercalate ", "
