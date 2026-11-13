-- cross-file context
module AgdaLint.Project
  ( Context (..)
  , mkContext
  , importClosures
  , visibleNotations
  , importers
  , notationOperators
  ) where

import           AgdaLint.Source
import qualified Data.Graph      as Graph
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)

data Context = Context
  { ctxFiles     :: [SourceFile]
  , ctxNotations :: Map FilePath (Map Text Notation)
  , ctxOperators :: Set Text
  , ctxImporters :: Map FilePath [SourceFile]
  }

mkContext :: [SourceFile] -> Context
mkContext files =
  Context
    { ctxFiles = files
    , ctxNotations = visibleNotations closures
    , ctxOperators = notationOperators files
    , ctxImporters = importers closures
    }
  where
    closures = importClosures files

-- each file with the files it imports, transitively, itself included
importClosures :: [SourceFile] -> Map FilePath [SourceFile]
importClosures files = Map.fromList [(srcPath f, closure f) | f <- files]
  where
    byModule = Map.fromList [(m, srcPath f) | f <- files, Just m <- [srcModule f]]
    imports f = mapMaybe ((`Map.lookup` byModule) . openModule) (filter openIsImport (srcOpens f))
    (graph, fromVertex, toVertex) = Graph.graphFromEdges [(f, srcPath f, imports f) | f <- files]
    closure f = [g | Just v <- [toVertex (srcPath f)], w <- Graph.reachable graph v, let (g, _, _) = fromVertex w]

visibleNotations :: Map FilePath [SourceFile] -> Map FilePath (Map Text Notation)
visibleNotations = Map.map visible
  where
    visible closure =
      let merged = Map.unionsWith pick (map (Map.map Just . srcSyntax) closure)
          pick a b = if a == b then a else Nothing
       in Map.mapMaybe id merged

-- each file with the files that import it, transitively, itself included
importers :: Map FilePath [SourceFile] -> Map FilePath [SourceFile]
importers closures = Map.fromListWith (++) [(srcPath d, [g]) | gs <- Map.elems closures, g <- take 1 gs, d <- gs]

-- notation operators
notationOperators :: [SourceFile] -> Set Text
notationOperators files =
  Set.fromList ["→", "->", ":", "=", "|"]
    <> Set.fromList [x | f <- files, n <- Map.elems (srcSyntax f), x <- notationRhs n, x `notElem` notationParams n]
