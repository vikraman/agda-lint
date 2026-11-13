-- findings and fixes
module AgdaLint.Types
  ( Level (..)
  , Pos (..)
  , Fix (..)
  , Finding (..)
  , Diagnostic (..)
  , finding
  , fixed
  , camelName
  , enumFromValue
  ) where

import           Data.Char    (toLower)
import           Data.List    (find)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Generic.Data (Constructors, gconName)
import           GHC.Generics (Generic)
import qualified Toml
import           Toml.Schema  (FromValue (..), Matcher, failAt)

data Level = Error | Warning | Off
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- line from 1, column from 0
data Pos = Pos {posLine :: Int, posCol :: Int}
  deriving (Eq, Ord, Show)

data Fix
  = ReplaceSpan Pos Int Text
  | ReplaceLine Int Text
  | InsertLineAfter Int Text
  | DeleteLine Int
  deriving (Eq, Ord, Show)

data Finding = Finding
  { findingPos     :: Pos
  , findingMessage :: Text
  , findingFixes   :: [Fix]
  }
  deriving (Eq, Ord, Show)

data Diagnostic = Diagnostic
  { diagFile    :: FilePath
  , diagFinding :: Finding
  , diagRule    :: Text
  , diagLevel   :: Level
  }
  deriving (Eq, Ord, Show)

finding :: Pos -> Text -> Finding
finding p m = Finding p m []

fixed :: Pos -> Text -> Fix -> Finding
fixed p m f = Finding p m [f]

-- camelCase constructor name
camelName :: Constructors a => a -> Text
camelName x = case gconName x of
  c : cs -> T.pack (toLower c : cs)
  []     -> ""

-- enum by camelCase constructor name
enumFromValue :: forall a l. (Bounded a, Enum a, Constructors a) => Toml.Value' l -> Matcher l a
enumFromValue v = do
  t <- fromValue v
  maybe (failAt (Toml.valueAnn v) (unknown t)) pure (find ((== t) . camelName) values)
  where
    values = [minBound .. maxBound] :: [a]
    unknown t = "unknown value " <> show (t :: Text) <> ", expected one of " <> T.unpack (T.intercalate ", " (map camelName values))

instance FromValue Level where fromValue = enumFromValue
