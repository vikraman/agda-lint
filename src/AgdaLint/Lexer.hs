-- lexer
module AgdaLint.Lexer
  ( TokenKind (..)
  , Token (..)
  , LineRole (..)
  , Line (..)
  , Format (..)
  , keywords
  , delims
  , formatOf
  , codeMask
  , lexFile
  ) where

import           AgdaLint.Types
import           Control.Monad              (void)
import           Data.Bifunctor             (first)
import           Data.Char                  (isAlphaNum, isSpace)
import           Data.List                  (mapAccumL)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           System.FilePath            (takeFileName)
import           Text.Megaparsec            hiding (Pos, State, Token, match)
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec.Char       (char, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

data TokenKind = Word | Delim | Keyword | Literal | Comment | Pragma
  deriving (Eq, Ord, Show)

data Token = Token
  { tokKind    :: TokenKind
  , tokText    :: Text
  , tokPos     :: Pos
  , tokEndLine :: Int
  }
  deriving (Eq, Ord, Show)

data LineRole = Code | CommentOnly | Blank | Prose
  deriving (Eq, Show)

data Line = Line
  { lineNo     :: Int
  , lineText   :: Text
  , lineTokens :: [Token]
  , lineRole   :: LineRole
  }

keywords :: Set Text
keywords =
  Set.fromList
    [ "module", "where", "let", "in", "data", "record", "field", "constructor", "open", "import"
    , "using", "hiding", "renaming", "to", "public", "private", "variable", "mutual", "infix"
    , "infixl", "infixr", "syntax", "pattern", "postulate", "instance", "abstract", "with"
    , "rewrite", "do", "λ", "∀", "forall"
    ]

delims :: [Char]
delims = "(){};@"

type Lexer = Parsec Void Text

-- whole file
tokensP :: Lexer [Token]
tokensP = skipSpace *> (concat <$> many (tokenP <* skipSpace)) <* eof
  where
    skipSpace = skipMany (satisfy isSpace)

tokenP :: Lexer [Token]
tokenP =
  choice
    [ one Pragma (string "{-#" *> skipUntil "#-}")
    , one Comment (string "{-" *> blockBody)
    , one Comment lineComment
    , one Literal stringLiteral
    , one Delim (void (oneOf delims))
    , wordTokens
    ]
  where
    one kind p = do
      start <- getSourcePos
      (txt, _) <- MP.match p
      end <- getSourcePos
      pure [Token kind txt (toPos start) (unPos (sourceLine end))]
    skipUntil close = skipManyTill anySingle (void (string close) <|> eof)
    blockBody = skipManyTill (string "{-" *> blockBody <|> void anySingle) (void (string "-}") <|> eof)
    lineComment = try $ do
      _ <- string "--" *> takeWhileP Nothing (== '-')
      notFollowedBy (satisfy (\c -> not (isSpace c || isAlphaNum c)))
      void (takeWhileP Nothing (/= '\n'))
    stringLiteral = char '"' *> void (manyTill L.charLiteral (void (char '"') <|> lookAhead (void newline) <|> eof))

-- words
wordTokens :: Lexer [Token]
wordTokens = do
  start <- getSourcePos
  w <- takeWhile1P (Just "word") (\c -> not (isSpace c || c `elem` delims || c == '"'))
  let pos = toPos start
      line = posLine pos
      kindOf x
        | "'" `T.isPrefixOf` x = Literal
        | Set.member x keywords = Keyword
        | otherwise = Word
  pure $ case T.uncons w of
    Just ('.', rest)
      | not (T.null rest), not ("." `T.isPrefixOf` rest) ->
          [Token Delim "." pos line, Token (kindOf rest) rest pos {posCol = posCol pos + 1} line]
    _ -> [Token (kindOf w) w pos line]

toPos :: SourcePos -> Pos
toPos sp = Pos (unPos (sourceLine sp)) (unPos (sourceColumn sp) - 1)

-- literate formats
data Format = PlainAgda | LaTeXAgda | MarkdownAgda

formatOf :: FilePath -> Either String Format
formatOf p
  | ".lagda.md" `T.isSuffixOf` f = Right MarkdownAgda
  | any (`T.isSuffixOf` f) [".lagda", ".lagda.tex"] = Right LaTeXAgda
  | ".lagda." `T.isInfixOf` f = Left (p <> ": unsupported literate format")
  | otherwise = Right PlainAgda
  where
    f = T.pack (takeFileName p)

-- agda code lines
codeMask :: Format -> [Text] -> [Bool]
codeMask fmt = case fmt of
  PlainAgda    -> map (const True)
  LaTeXAgda    -> snd . mapAccumL latex False
  MarkdownAgda -> snd . mapAccumL markdown Nothing
  where
    latex inside t
      | inside = let stay = not ("\\end{code}" `T.isPrefixOf` t) in (stay, stay)
      | otherwise = ("\\begin{code}" `T.isPrefixOf` t, False)
    -- inside a fence, and whether it is agda: unlabelled fences are
    markdown fence t = case (fence, T.stripPrefix "```" (T.strip t)) of
      (Just _, Just "")      -> (Nothing, False)
      (Just agda, _)         -> (fence, agda)
      (Nothing, Just lang)   -> (Just (lang `elem` ["", "agda"]), False)
      (Nothing, Nothing)     -> (Nothing, False)

lexFile :: FilePath -> Text -> Either String [Line]
lexFile path src = do
  mask <- (`codeMask` rawLines) <$> formatOf path
  let masked = T.intercalate "\n" [if m then t else "" | (t, m) <- zip rawLines mask]
  lexed <- first errorBundlePretty (runParser (oneColumnTabs *> tokensP) path masked)
  let byLine = Map.fromListWith (flip (++)) [(posLine (tokPos t), [t]) | t <- lexed]
      insideComment = Set.fromList [n | t <- lexed, tokKind t `elem` [Comment, Pragma], n <- [posLine (tokPos t) + 1 .. tokEndLine t]]
      mkLine n t isCode
        | not isCode = Line n t [] Prose
        | otherwise = Line n t toks role
        where
          toks = Map.findWithDefault [] n byLine
          role
            | any ((`notElem` [Comment, Pragma]) . tokKind) toks = Code
            | not (null toks) || Set.member n insideComment = CommentOnly
            | otherwise = Blank
  pure (zipWith3 mkLine [1 ..] rawLines mask)
  where
    -- without the cr of a crlf line end
    rawLines = map (\t -> fromMaybe t (T.stripSuffix "\r" t)) (T.splitOn "\n" src)
    -- tab is one column
    oneColumnTabs = updateParserState (\st -> st {statePosState = (statePosState st) {pstateTabWidth = pos1}})
