-- output formats
module AgdaLint.Report (Output (..), report) where

import           AgdaLint.Types
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           System.FilePath (normalise, (</>))

data Output = Text | Github
  deriving (Eq, Show, Enum, Bounded, Generic)

-- prefix is the path from the repository root, for annotations
report :: Output -> FilePath -> Diagnostic -> Text
report out prefix d = case out of
  Text ->
    T.pack (diagFile d) <> ":" <> T.show (posLine pos) <> ":" <> T.show (posCol pos + 1) <> ": "
      <> camelName (diagLevel d) <> " [" <> diagRule d <> "] " <> findingMessage (diagFinding d)
  Github ->
    "::" <> camelName (diagLevel d)
      <> " file=" <> property (T.pack (normalise (prefix </> diagFile d)))
      <> ",line=" <> T.show (posLine pos)
      <> ",col=" <> T.show (posCol pos + 1)
      <> ",title=" <> property ("agda-lint: " <> diagRule d)
      <> "::" <> escape (findingMessage (diagFinding d))
  where
    pos = findingPos (diagFinding d)

-- workflow command escapes
escape :: Text -> Text
escape = T.replace "\n" "%0A" . T.replace "\r" "%0D" . T.replace "%" "%25"

property :: Text -> Text
property = T.replace "," "%2C" . T.replace ":" "%3A" . escape
