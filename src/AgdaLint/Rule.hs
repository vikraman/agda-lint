-- rules
module AgdaLint.Rule
  ( Rule (..)
  , Check
  , Settings
  , rule
  , ruleWith
  , core
  , decodeSettings
  , restrictSettings
  , settingsKeys
  , enumFromValue
  ) where

import           AgdaLint.Project
import           AgdaLint.Source
import           AgdaLint.Types
import           Data.Bifunctor  (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Toml
import           Toml.Schema

type Check = Context -> SourceFile -> [Finding]

-- config table
type Settings = Toml.Table' Toml.Position

data Rule = Rule
  { ruleName    :: Text
  -- recommended level
  , ruleLevel   :: Level
  -- on in the core preset
  , ruleCore    :: Bool
  , ruleSummary :: Text
  -- top-level config keys it reads
  , ruleKeys    :: [Text]
  -- runs once, on the config restricted to its keys
  , ruleSetup   :: Settings -> Either String Check
  }

-- rule without options
rule :: Text -> Level -> Text -> Check -> Rule
rule n l s c = Rule n l False s [] (const (Right c))

-- rule with options decoded from its keys
ruleWith :: FromValue o => Text -> Level -> Text -> [Text] -> (o -> Check) -> Rule
ruleWith n l s ks c = Rule n l False s ks (fmap c . decodeSettings)

-- on by default
core :: Rule -> Rule
core r = r {ruleCore = True}

decodeSettings :: FromValue a => Settings -> Either String a
decodeSettings t = first (unlines . map Toml.prettyMatchMessage) (runMatcherFatalWarn (fromValue (Toml.Table' (Toml.Position 0 1 1) t)))

restrictSettings :: [Text] -> Settings -> Settings
restrictSettings ks (Toml.MkTable m) = Toml.MkTable (Map.restrictKeys m (Set.fromList ks))

settingsKeys :: Settings -> [Text]
settingsKeys (Toml.MkTable m) = Map.keys m
