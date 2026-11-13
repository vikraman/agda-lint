-- applying fixes
module AgdaLint.Fix (acceptFixes, applyFixes, fixText, lineOrigins) where

import           AgdaLint.Types
import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T

-- fixed text and the edits made, crlf line ends are kept
fixText :: [[Fix]] -> Text -> (Text, [Fix])
fixText groups src = (T.intercalate end (applyFixes accepted ls), accepted)
  where
    crlf = "\r\n" `T.isInfixOf` src
    end = if crlf then "\r\n" else "\n"
    ls = map dropCR (T.splitOn "\n" src)
    dropCR t = if crlf then fromMaybe t (T.stripSuffix "\r" t) else t
    accepted = acceptFixes ls groups

-- part of a line an edit rewrites
data Extent = Whole | Cols Int Int
  deriving (Eq)

-- each group is taken whole or not at all, earlier groups first
acceptFixes :: [Text] -> [[Fix]] -> [Fix]
acceptFixes ls = Set.toList . snd . foldl' take' (Map.empty, Set.empty)
  where
    count = length ls
    widths = Map.fromList (zip [1 ..] (map T.length ls))
    take' (used, done) group
      | all valid new, Just used' <- foldl' claim (Just used) new = (used', done <> Set.fromList new)
      | otherwise = (used, done)
      where
        new = filter (`Set.notMember` done) (Set.toList (Set.fromList group))
    valid = \case
      ReplaceSpan (Pos n c) w _ -> c >= 0 && w >= 0 && maybe False (c + w <=) (Map.lookup n widths)
      ReplaceLine n _ -> n >= 1 && n <= count
      DeleteLine n -> n >= 1 && n <= count
      InsertLineAfter n _ -> n >= 0 && n <= count
    extent = \case
      ReplaceSpan (Pos n c) w _ -> Just (n, Cols c (c + w))
      ReplaceLine n _ -> Just (n, Whole)
      DeleteLine n -> Just (n, Whole)
      InsertLineAfter _ _ -> Nothing
    claim used fx = case (used, extent fx) of
      (Just u, Just (n, e))
        | any (clash e) (Map.findWithDefault [] n u) -> Nothing
        | otherwise -> Just (Map.insertWith (++) n [e] u)
      _ -> used
    clash (Cols a b) (Cols c d) = a < d && c < b || (a == c && (a == b || c == d))
    clash _ _                   = True

-- edits that do not clash, as chosen by acceptFixes
applyFixes :: [Fix] -> [Text] -> [Text]
applyFixes fixes ls = Map.findWithDefault [] 0 inserts ++ concat (zipWith edit [1 ..] ls)
  where
    whole = Map.fromList [(n, t) | ReplaceLine n t <- fixes]
    spans = Map.fromListWith (++) [(posLine p, [(posCol p, w, new)]) | ReplaceSpan p w new <- fixes]
    deletes = Set.fromList [n | DeleteLine n <- fixes]
    inserts = Map.fromListWith (flip (++)) [(n, [t]) | InsertLineAfter n t <- fixes]
    -- rightmost first, so columns stay valid
    perLine n t = case Map.lookup n whole of
      Just t' -> t'
      Nothing -> foldl' (\acc (c, w, new) -> T.take c acc <> new <> T.drop (c + w) acc) t (sortOn (\(c, _, _) -> negate c) (Map.findWithDefault [] n spans))
    edit n t = [perLine n t | Set.notMember n deletes] ++ Map.findWithDefault [] n inserts

-- for each line after the fixes, its line before them
lineOrigins :: [Fix] -> Int -> [Maybe Int]
lineOrigins fixes count = concatMap origin [0 .. count]
  where
    deletes = Set.fromList [n | DeleteLine n <- fixes]
    inserts = Map.fromListWith (+) [(n, 1 :: Int) | InsertLineAfter n _ <- fixes]
    origin n = [Just n | n >= 1, Set.notMember n deletes] ++ replicate (Map.findWithDefault 0 n inserts) Nothing
