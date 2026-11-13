-- agda-lint with project rules

module Main (main) where

import           AgdaLint
import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Toml.Schema  (FromValue (..), genericFromTable)

main :: IO ()
main = defaultMain (builtinRules ++ [noPostulate, lemmaPrefix])

-- no options, no fix
noPostulate :: Rule
noPostulate = rule "noPostulate" Warning "no postulates" $ \_ f ->
  [ finding (tokPos t) "prove it or move it to an assumptions module"
  | l <- codeLines f
  , t <- codeTokens l
  , tokKind t == Keyword
  , tokText t == "postulate"
  ]

-- options from the lemmaPrefixes key, fixable
newtype LemmaOpts = LemmaOpts {lemmaPrefixes :: Maybe [Text]}
  deriving (Generic)

instance FromValue LemmaOpts where fromValue = genericFromTable

lemmaPrefix :: Rule
lemmaPrefix = core . ruleWith "lemmaPrefix" Warning "lemma names do not start with one of lemmaPrefixes" ["lemmaPrefixes"] $ \opts _ f ->
  [ fixed (tokPos t) ("drop the " <> p <> " prefix") (replaceTok t rest)
  | l <- codeLines f
  , t <- wordsOf l
  , p <- fromMaybe ["lemma-"] (lemmaPrefixes opts)
  , Just rest <- [T.stripPrefix p (tokText t)]
  , not (T.null rest)
  ]
