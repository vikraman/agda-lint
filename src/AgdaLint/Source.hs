-- structure
module AgdaLint.Source where

import           AgdaLint.Lexer
import           AgdaLint.Types
import           Control.Monad   (guard)
import           Data.List       (unsnoc)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, fromMaybe, isJust, listToMaybe,
                                  mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Void       (Void)
import           Text.Megaparsec hiding (Pos, State, Token, match)

data Clause = Clause {clauseHead :: Line, clauseBody :: [Line]}

clauseLines :: Clause -> [Line]
clauseLines c = clauseHead c : clauseBody c

data Binder = Binder {binderNames :: [Token], binderType :: [Token]}

data VariableDecl = VariableDecl {varNames :: [Token], varType :: [Text]}

data Notation = Notation {notationParams :: [Text], notationRhs :: [Text]}
  deriving (Eq)

data ImportModifier = ModPublic | ModUsing | ModHiding | ModRenaming
  deriving (Eq, Ord, Show)

data OpenDecl = OpenDecl
  { openModule    :: Text
  , openIsImport  :: Bool
  , openIsOpen    :: Bool
  , openModifiers :: [ImportModifier]
  , openLine      :: Line
  }

data SourceFile = SourceFile
  { srcPath         :: FilePath
  , srcLines        :: [Line]
  , srcModule       :: Maybe Text
  , srcOpens        :: [OpenDecl]
  , srcClauses      :: [Clause]
  , srcVariables    :: [VariableDecl]
  , srcSyntax       :: Map Text Notation
  , srcFixities     :: Set Text
  , srcNamedKeys    :: Set Text
  , srcModuleParams :: Set Text
  , srcTokenCounts  :: Map Text Int
  }

codeLines :: SourceFile -> [Line]
codeLines = filter ((== Code) . lineRole) . srcLines

blockLines :: SourceFile -> [Line]
blockLines = filter ((/= Prose) . lineRole) . srcLines

texts :: [Token] -> [Text]
texts = map tokText

codeTokens :: Line -> [Token]
codeTokens = filter ((`notElem` [Comment, Pragma]) . tokKind) . lineTokens

indentOf :: Line -> Int
indentOf = T.length . T.takeWhile (== ' ') . lineText

firstText :: Line -> Maybe Text
firstText l = tokText <$> listToMaybe (codeTokens l)

wordsOf :: Line -> [Token]
wordsOf = filter ((== Word) . tokKind) . codeTokens

lineStart :: Line -> Pos
lineStart l = Pos (lineNo l) (indentOf l)

replaceTok :: Token -> Text -> Fix
replaceTok t = ReplaceSpan (tokPos t) (T.length (tokText t))

analyse :: FilePath -> Text -> Either String SourceFile
analyse path src = do
  ls <- lexFile path src
  let code = filter ((`elem` [Code, Blank]) . lineRole) ls
  pure SourceFile
    { srcPath = path
    , srcLines = ls
    , srcModule = listToMaybe (mapMaybe (fmap tokText . runTokens (word "module" *> name) . codeTokens) code)
    , srcOpens = mapMaybe openDecl code
    , srcClauses = clauses code
    , srcVariables = variables code
    , srcSyntax = Map.fromList (mapMaybe notation code)
    , srcFixities = Set.fromList (concatMap fixity code)
    , srcNamedKeys = Set.fromList (concatMap (namedKeys . codeTokens) code)
    , srcModuleParams =
        Set.fromList [tokText n | l <- code, firstText l == Just "module", b <- binders (codeTokens l), n <- binderNames b]
    , srcTokenCounts = Map.fromListWith (+) [(tokText t, 1) | l <- code, t <- wordsOf l]
    }

-- line token parsers
type TokParser = Parsec Void [Token]

runTokens :: TokParser a -> [Token] -> Maybe a
runTokens p = parseMaybe (p <* takeRest)

-- all matches of p
scan :: TokParser a -> TokParser [a]
scan p = catMaybes <$> many (Just <$> try p <|> Nothing <$ anySingle)

word :: Text -> TokParser Token
word t = satisfy (\x -> tokText x == t && tokKind x `elem` [Word, Keyword])

name :: TokParser Token
name = satisfy ((== Word) . tokKind)

delim :: Text -> TokParser Token
delim t = satisfy (\x -> tokKind x == Delim && tokText x == t)

openDecl :: Line -> Maybe OpenDecl
openDecl l = runTokens p (codeTokens l)
  where
    p = do
      isOpen <- isJust <$> optional (word "open")
      isImport <- isJust <$> optional (word "import")
      guard (isOpen || isImport)
      m <- name
      mods <- mapMaybe (modifier . tokText) <$> many anySingle
      pure (OpenDecl (tokText m) isImport isOpen mods l)
    modifier = \case
      "public"   -> Just ModPublic
      "using"    -> Just ModUsing
      "hiding"   -> Just ModHiding
      "renaming" -> Just ModRenaming
      _          -> Nothing

notation :: Line -> Maybe (Text, Notation)
notation l = runTokens p (codeTokens l)
  where
    p = do
      _ <- word "syntax"
      n <- name
      params <- many (satisfy ((/= "=") . tokText))
      _ <- word "="
      rhs <- many anySingle
      pure (tokText n, Notation {notationParams = texts params, notationRhs = texts rhs})

fixity :: Line -> [Text]
fixity l = fromMaybe [] (runTokens p (codeTokens l))
  where
    p = choice (map word ["infix", "infixl", "infixr"]) *> anySingle *> (texts <$> many anySingle)

-- named argument keys
namedKeys :: [Token] -> [Text]
namedKeys = maybe [] (map tokText) . runTokens (scan (delim "{" *> name <* word "="))

-- line plus indented continuations
clauses :: [Line] -> [Clause]
clauses = finish . foldl' step (Nothing, [], False)
  where
    finish (cur, done, _) = reverse (maybe done (: done) cur)
    extend (Clause h b) l = Clause h (b ++ [l])
    step (cur, done, inVar) l
      | inVar && indentOf l >= 2 && ":" `elem` toks = (cur, done, True)
      | lineRole l == Blank = (cur, done, False)
      | isVariableHeader = (Nothing, flush, True)
      | Just c <- cur, firstText l == Just "..." = (Just (extend c l), done, False)
      | Just c <- cur, isSeparator, indentOf l > indentOf (clauseHead c) = (Just (extend c l), done, False)
      | resets = (Nothing, flush, False)
      | Just c <- cur, indentOf l > indentOf (clauseHead c) = (Just (extend c l), done, False)
      | otherwise = (Just (Clause l []), flush, False)
      where
        flush = maybe done (: done) cur
        toks = texts (codeTokens l)
        isVariableHeader = toks `elem` [["variable"], ["private", "variable"]]
        stripped = T.strip (lineText l)
        isSeparator = not (T.null stripped) && T.all (== '-') stripped
        resets =
          toks == ["mutual"]
            || (take 1 toks `elem` [["module"], ["private"], ["data"], ["record"]] && fmap snd (unsnoc toks) == Just "where")

variables :: [Line] -> [VariableDecl]
variables = go False
  where
    go _ [] = []
    go inVar (l : rest)
      | toks `elem` [["variable"], ["private", "variable"]] = go True rest
      | "private" : "variable" : entry@(_ : _) <- toks, ":" `elem` entry = decl (drop 2 (codeTokens l)) : go False rest
      | inVar && indentOf l >= 2 && ":" `elem` toks = decl (codeTokens l) : go True rest
      | lineRole l == Blank = go inVar rest
      | otherwise = go False rest
      where
        toks = texts (codeTokens l)
    decl ts = let (ns, ty) = break ((== ":") . tokText) ts in VariableDecl ns (drop 1 (texts ty))

-- binders on one line
binders :: [Token] -> [Binder]
binders = fromMaybe [] . runTokens (scan binder)
  where
    binder = do
      close <- (")" <$ delim "(") <|> ("}" <$ delim "{")
      names <- some (satisfy (\t -> tokKind t == Word && tokText t /= ":"))
      _ <- word ":"
      ty <- concat <$> many (bracketed <|> (: []) <$> satisfy (not . isBracket))
      _ <- delim close
      pure (Binder names ty)
    bracketed = do
      o <- delim "(" <|> delim "{"
      inner <- concat <$> many (bracketed <|> (: []) <$> satisfy (not . isBracket))
      c <- delim ")" <|> delim "}"
      pure (o : inner ++ [c])
    isBracket t = tokKind t == Delim && tokText t `elem` ["(", ")", "{", "}"]

codomain :: [Token] -> [Text]
codomain = codomainOf . texts

codomainOf :: [Text] -> [Text]
codomainOf = reverse . takeWhile (/= "→") . reverse
