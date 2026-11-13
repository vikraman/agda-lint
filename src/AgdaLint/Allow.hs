-- allowed findings
module AgdaLint.Allow (Allow (..), parseAllow, allowed) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Read       as TR
import qualified System.FilePath.Glob as Glob

-- path glob, line (all if Nothing), rule
data Allow = Allow {allowPath :: Glob.Pattern, allowLine :: Maybe Int, allowRule :: Text}

-- path glob, or path glob:line
parseAllow :: Text -> Text -> Either String Allow
parseAllow r entry = case T.breakOnEnd ":" entry of
  ("", p) | not (T.null p) -> Right (Allow (glob p) Nothing r)
  (p, n)
    | T.length p > 1, Right (k, "") <- TR.decimal n -> Right (Allow (glob (T.dropEnd 1 p)) (Just k) r)
  _ -> Left ("expected path or path:line for " <> T.unpack r <> ", got " <> show entry)
  where
    glob = Glob.compile . T.unpack

-- line is Nothing for a line added by a fix
allowed :: [Allow] -> FilePath -> Text -> Maybe Int -> Bool
allowed allow file r line =
  any (\a -> allowRule a == r && Glob.match (allowPath a) file && maybe True ((== line) . Just) (allowLine a)) allow
