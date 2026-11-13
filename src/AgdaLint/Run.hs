{-# LANGUAGE MultiWayIf #-}
-- running
module AgdaLint.Run
  ( Project (..)
  , LintError (..)
  , loadProject
  , loadSources
  , unmatchedPaths
  , lintFiles
  , lintProject
  , fixProject
  ) where

import           AgdaLint.Allow
import           AgdaLint.Config
import           AgdaLint.Fix
import           AgdaLint.Project
import           AgdaLint.Rule
import           AgdaLint.Source
import           AgdaLint.Types
import           Control.Exception         (Exception, throwIO)
import           Control.Monad             (foldM, forM, join, unless, when)
import           Data.Containers.ListUtils (nubOrd)
import           Data.List                 (isPrefixOf, (!?))
import qualified Data.List                 as List
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, isJust)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.Directory          (canonicalizePath, doesFileExist,
                                            doesPathExist)
import           System.FilePath           (addTrailingPathSeparator,
                                            isRelative, makeRelative,
                                            splitDirectories, takeFileName,
                                            (</>))
import qualified System.FilePath.Glob      as Glob

newtype LintError = LintError String
  deriving (Show)

instance Exception LintError

data Project = Project
  { projRoot   :: FilePath
  , projConfig :: Config
  , projRules  :: [Active]
  -- files to report on, relative to the root, all if empty
  , projPaths  :: [FilePath]
  }

failWith :: String -> IO a
failWith = throwIO . LintError

-- config file defaults to agda-lint.toml in the root, which may be missing
loadProject :: [Rule] -> FilePath -> Maybe FilePath -> [FilePath] -> IO Project
loadProject rules root explicit paths = do
  let p = fromMaybe (root </> configFile) explicit
  exists <- doesFileExist p
  when (not exists && isJust explicit) $ failWith (p <> ": no such config file")
  src <- if exists then TIO.readFile p else pure ""
  (cfg, active) <- either (failWith . ((p <> ": ") <>)) pure (parseSettings src >>= resolve rules)
  Project root cfg active <$> mapM (underRoot root) paths

-- path below the root, relative to it
underRoot :: FilePath -> FilePath -> IO FilePath
underRoot root p = do
  exists <- doesPathExist (root </> p)
  unless exists $ failWith (p <> ": no such file or directory")
  rel <- makeRelative <$> canonicalizePath root <*> canonicalizePath (root </> p)
  unless (isRelative rel && take 1 (splitDirectories rel) /= [".."]) $ failWith (p <> ": outside the project")
  pure rel

loadSources :: Project -> IO [SourceFile]
loadSources proj = do
  let root = projRoot proj
      cfg = projConfig proj
  found <- concat <$> Glob.globDir (map Glob.compile (cfgSources cfg)) root
  let excludes = map Glob.compile (cfgExclude cfg)
      rel = makeRelative root
      keep p = not (any (`Glob.match` rel p) excludes) && not (".#" `isPrefixOf` takeFileName p)
      paths = nubOrd (filter keep found)
  forM (List.sort paths) $ \p -> either failWith pure . analyse (rel p) =<< TIO.readFile p

covers :: FilePath -> FilePath -> Bool
covers q p = q == p || q == "." || addTrailingPathSeparator q `isPrefixOf` p

selected :: Project -> FilePath -> Bool
selected proj p = null (projPaths proj) || any (`covers` p) (projPaths proj)

-- paths that select no source
unmatchedPaths :: Project -> [SourceFile] -> [FilePath]
unmatchedPaths proj files = [q | q <- projPaths proj, not (any (covers q . srcPath) files)]

-- line of each current line before fixing, by file
type Origins = Map FilePath [Maybe Int]

lintFiles :: Project -> [SourceFile] -> [Diagnostic]
lintFiles = lintWith Map.empty

lintWith :: Origins -> Project -> [SourceFile] -> [Diagnostic]
lintWith origins proj files =
  List.sort
    [ Diagnostic (srcPath f) fd (ruleName r) lvl
    | f <- files
    , selected proj (srcPath f)
    , Active r lvl check <- projRules proj
    , lvl /= Off
    , fd <- check ctx f
    , not (allowed (cfgAllow (projConfig proj)) (srcPath f) (ruleName r) (origin (srcPath f) (posLine (findingPos fd))))
    ]
  where
    ctx = mkContext files
    origin p n = maybe (Just n) (\os -> join (os !? (n - 1))) (Map.lookup p origins)

lintProject :: Project -> IO [Diagnostic]
lintProject proj = lintFiles proj <$> loadSources proj

-- rewrite fixable findings in place, until nothing changes; False if it did not settle
fixProject :: Project -> IO Bool
fixProject proj = loop (5 :: Int) Map.empty
  where
    loop n origins = do
      diags <- lintWith origins proj <$> loadSources proj
      let byFile = Map.fromListWith (flip (++)) [(diagFile d, [findingFixes (diagFinding d)]) | d <- diags, not (null (findingFixes (diagFinding d)))]
      if
        | Map.null byFile -> pure True
        | n == 0 -> pure False
        | otherwise -> loop (n - 1) =<< foldM rewrite origins (Map.toList byFile)
    rewrite origins (p, groups) = do
      src <- TIO.readFile (projRoot proj </> p)
      let (out, fixes) = fixText groups src
          moved = lineOrigins fixes (length (T.splitOn "\n" src))
          before o = maybe (Just o) (\os -> join (os !? (o - 1))) (Map.lookup p origins)
      when (out /= src) $ TIO.writeFile (projRoot proj </> p) out
      pure (Map.insert p (map (>>= before) moved) origins)
