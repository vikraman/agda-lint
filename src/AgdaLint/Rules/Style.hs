-- stdlib style guide rules
module AgdaLint.Rules.Style (styleRules) where

import           AgdaLint.Lexer
import           AgdaLint.Rule
import           AgdaLint.Rules.Naming (isSingleLetterVar)
import           AgdaLint.Source
import           AgdaLint.Types
import           Data.Char             (isLetter, isLower, isSpace, isUpper)
import           Data.List             (find, unsnoc)
import qualified Data.List             as List
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import qualified Toml
import           Toml.Schema

styleRules :: [Rule]
styleRules =
  [ ruleWith "lineLength" Warning "lines longer than [style] lineLength" ["style"] checkLineLength
  , core (rule "trailingWhitespace" Warning "trailing whitespace" checkTrailingWhitespace)
  , core (rule "tab" Warning "tab characters" checkTab)
  , rule "leadingArrow" Warning "arrows at line breaks go at the end of the line" checkLeadingArrow
  , rule "moduleBlankLine" Warning "a single blank line after the module header" checkModuleBlankLine
  , core (rule "importOrder" Warning "a block of imports is in alphabetical order" checkImportOrder)
  , rule "importPublic" Warning "no public re-exports in the import list" checkImportPublic
  , rule "importModifierOrder" Warning "import modifiers in the order public, using, renaming" checkImportModifierOrder
  , rule "mutualBlock" Warning "mutual blocks are obsolete; put signatures before definitions" checkMutualBlock
  , rule "letBinding" Warning "prefer where blocks to let" checkLetBinding
  , rule "trailingComment" Warning "comments go above a term, not at the end of a line" checkTrailingComment
  , ruleWith "sectionHeader" Warning "dividers are [style] sectionWidth dashes with a title in [style] sectionTitle case" ["style"] checkSectionHeader
  , rule "withAlignment" Warning "the | of a with-clause is not aligned (... | p)" checkWithAlignment
  , rule "unicodeInstance" Warning "use {{ }} instead of ⦃ ⦄" checkUnicodeInstance
  , rule "asciiPrime" Warning "names use ′ instead of the ascii '" checkAsciiPrime
  , rule "missingFixity" Warning "operators (names starting or ending in _) have a fixity declaration" checkMissingFixity
  , rule "beginSameLine" Warning "begin goes on the line with the proof it starts" checkBeginSameLine
  , rule "whereLayout" Warning "where goes on its own line below the clause" checkWhereLayout
  , rule "stackedPrivate" Warning "private and variable go on separate lines" checkStackedPrivate
  , rule "botElim" Warning "prefer contradiction to ⊥-elim" checkBotElim
  , ruleWith "rewriteName" Warning "rewrite rules have a name ending in one of [rewriteSuffixes]" ["rewriteSuffixes"] checkRewriteName
  ]

------------------------------------------------------------------------
-- options

data SectionTitle = Sentence | Lower | Any
  deriving (Eq, Show, Enum, Bounded, Generic)

data RawStyle = RawStyle {lineLength :: Maybe Int, sectionWidth :: Maybe Int, sectionTitle :: Maybe SectionTitle}
  deriving (Generic)

newtype StyleOpts = StyleOpts {style :: Maybe RawStyle}
  deriving (Generic)

data Style = Style
  { styleLineLength   :: Int
  , styleSectionWidth :: Int
  , styleSectionTitle :: SectionTitle
  }

newtype RewriteOpts = RewriteOpts {rewriteSuffixes :: Maybe [Text]}
  deriving (Generic)

instance FromValue SectionTitle where fromValue = enumFromValue
instance FromValue RawStyle where
  fromValue v = do
    s@RawStyle {lineLength, sectionWidth} <- genericFromTable v
    if any (< 1) lineLength || any (< 1) sectionWidth
      then failAt (Toml.valueAnn v) "lineLength and sectionWidth must be positive"
      else pure s
instance FromValue StyleOpts where fromValue = genericFromTable
instance FromValue RewriteOpts where fromValue = genericFromTable

styleOf :: StyleOpts -> Style
styleOf StyleOpts {style} = case style of
  Nothing -> Style 72 72 Sentence
  Just RawStyle {lineLength, sectionWidth, sectionTitle} ->
    Style (fromMaybe 72 lineLength) (fromMaybe 72 sectionWidth) (fromMaybe Sentence sectionTitle)

------------------------------------------------------------------------
-- checks

checkLineLength :: StyleOpts -> Check
checkLineLength opts _ f =
  [ finding (Pos (lineNo l) limit) ("line is " <> T.show (T.length (lineText l)) <> " characters (limit " <> T.show limit <> ")")
  | let limit = styleLineLength (styleOf opts)
  , l <- blockLines f
  , T.length (lineText l) > limit
  ]

checkTrailingWhitespace :: Check
checkTrailingWhitespace _ f =
  [ fixed (Pos (lineNo l) (T.length s)) "trailing whitespace" (ReplaceLine (lineNo l) s)
  | l <- blockLines f
  , let s = T.stripEnd (lineText l)
  , s /= lineText l
  ]

checkTab :: Check
checkTab _ f = [finding (Pos (lineNo l) c) "tab character" | l <- blockLines f, Just c <- [T.findIndex (== '\t') (lineText l)]]

checkLeadingArrow :: Check
checkLeadingArrow _ f = [finding (lineStart l) "put → at the end of the previous line" | l <- codeLines f, indentOf l > 0, firstText l == Just "→"]

checkModuleBlankLine :: Check
checkModuleBlankLine _ f = case dropWhile (not . isHeader) (codeLines f) of
  [] -> []
  (h : _) ->
    let headerEnd = fromMaybe h (find ((== Just "where") . fmap snd . unsnoc . texts . codeTokens) (dropWhile ((< lineNo h) . lineNo) (codeLines f)))
        after = [l | l <- srcLines f, lineNo l > lineNo headerEnd, not (isFence l)]
        pos = Pos (lineNo headerEnd) 0
        msg = "put a single blank line after the module header"
     in case after of
          (a : b : _) | lineRole a == Blank, lineRole b /= Blank -> []
          (a : _) | lineRole a /= Blank -> [fixed pos msg (InsertLineAfter (lineNo headerEnd) "")]
          (_ : rest) ->
            let extra = takeWhile ((== Blank) . lineRole) rest
             in [Finding pos msg (map (DeleteLine . lineNo) extra) | not (null extra)]
          [] -> [finding pos msg]
  where
    isHeader l = indentOf l == 0 && firstText l == Just "module"
    isFence l = lineRole l == Prose

checkImportOrder :: Check
checkImportOrder _ f = concatMap check (blocks (srcLines f))
  where
    -- a block is a run of imports, broken by blank lines, comments and other code
    blocks ls = case dropWhile (not . isImport) ls of
      [] -> []
      xs@(x : _) ->
        let continues l = isImport l || (lineRole l == Code && indentOf l > indentOf x)
            (b, rest) = span continues xs
         in filter isImport b : blocks rest
    isImport l = maybe False openIsImport (openDecl l)
    check b =
      [ finding (lineStart l) (m <> " should come before " <> p)
      | (prev, l) <- zip b (drop 1 b)
      , Just p <- [openModule <$> openDecl prev]
      , Just m <- [openModule <$> openDecl l]
      , m < p
      ]

checkImportPublic :: Check
checkImportPublic _ f =
  [ finding (lineStart (openLine o)) "import without public and open it publicly later"
  | o <- srcOpens f
  , openIsImport o && openIsOpen o
  , ModPublic `elem` openModifiers o
  ]

checkImportModifierOrder :: Check
checkImportModifierOrder _ f =
  [ finding (lineStart (openLine o)) "modifiers go in the order public, using, renaming"
  | o <- srcOpens f
  , let ranks = map rank (openModifiers o)
  , ranks /= List.sort ranks
  ]
  where
    rank = \case ModPublic -> 0 :: Int; ModUsing -> 1; ModHiding -> 1; ModRenaming -> 2

checkMutualBlock :: Check
checkMutualBlock _ f = [finding (lineStart l) "mutual blocks are obsolete; put signatures before definitions" | l <- codeLines f, texts (codeTokens l) == ["mutual"]]

checkLetBinding :: Check
checkLetBinding _ f = [finding (tokPos t) "prefer a where block to let" | l <- codeLines f, t <- codeTokens l, tokKind t == Keyword, tokText t == "let"]

checkTrailingComment :: Check
checkTrailingComment _ f =
  [ finding (tokPos c) "put the comment above the term"
  | l <- codeLines f
  , c@Token {tokKind = Comment, tokText = body} <- lineTokens l
  , any ((< posCol (tokPos c)) . posCol . tokPos) (codeTokens l)
  , not (T.all (== '-') body)
  ]

checkSectionHeader :: StyleOpts -> Check
checkSectionHeader opts _ f = concat [check l next | (l, next) <- zip ls (map Just (drop 1 ls) ++ [Nothing]), isDivider l]
  where
    style = styleOf opts
    ls = blockLines f
    isDivider l = T.length (lineText l) >= 4 && T.all (== '-') (lineText l)
    check l next =
      [ finding (Pos (lineNo l) 0) ("divider is " <> T.show (T.length (lineText l)) <> " dashes (expected " <> T.show (styleSectionWidth style) <> ")")
      | T.length (lineText l) /= styleSectionWidth style
      ]
        ++ case next of
          Just n
            | Just title <- T.stripStart <$> T.stripPrefix "--" (lineText n)
            , not (T.null title)
            , not (T.all (== '-') title)
            , Just problem <- titleProblem title ->
                [finding (Pos (lineNo n) 0) problem]
          _ -> []
    titleProblem t = case styleSectionTitle style of
      Sentence | not (maybe False (isUpper . fst) (T.uncons t)) || not (T.any isLower t) -> Just ("title should be in sentence case: " <> t)
      Lower | not (all allowedWord (T.words t)) -> Just ("title should be lowercase: " <> t)
      _ -> Nothing
      where
        -- acronyms are fine in an otherwise lowercase title
        allowedWord w = T.toLower w == w || (T.any isLower t && T.all (\c -> not (isLetter c) || isUpper c) w)

checkWithAlignment :: Check
checkWithAlignment _ f =
  [ finding (lineStart l) "write ... | without aligning the |"
  | l <- codeLines f
  , Just (d, b) <- [runTokens ((,) <$> word "..." <*> word "|") (codeTokens l)]
  , posCol (tokPos b) - posCol (tokPos d) > 4
  ]

checkUnicodeInstance :: Check
checkUnicodeInstance _ f =
  [ fixed (tokPos t) "use {{ }} instead of ⦃ ⦄" (replaceTok t (T.replace "⦃" "{{" (T.replace "⦄" "}}" (tokText t))))
  | l <- codeLines f
  , t <- wordsOf l
  , T.any (`elem` ("⦃⦄" :: String)) (tokText t)
  ]

checkAsciiPrime :: Check
checkAsciiPrime _ f =
  [ fixed (tokPos t) ("use ′ instead of ' in " <> tokText t) (replaceTok t (T.replace "'" "′" (tokText t)))
  | l <- codeLines f
  , t <- wordsOf l
  , "'" `T.isInfixOf` tokText t
  , not (isSingleLetterVar (T.takeWhile (/= '\'') (tokText t)))
  ]

checkMissingFixity :: Check
checkMissingFixity _ f =
  [ finding (lineStart l) (n <> " has no fixity declaration")
  | c <- srcClauses f
  , let l = clauseHead c
  , Just n <- [tokText <$> runTokens (name <* word ":") (codeTokens l)]
  , n /= "_"
  , "_" `T.isPrefixOf` n || "_" `T.isSuffixOf` n
  , Set.notMember n (srcFixities f)
  ]

checkBeginSameLine :: Check
checkBeginSameLine _ f = [finding (lineStart l) "put begin on the line with the proof it starts" | l <- codeLines f, firstText l == Just "begin"]

checkWhereLayout :: Check
checkWhereLayout _ f =
  [ finding (tokPos w) "put where on its own line below the clause"
  | l <- codeLines f
  , let ts = codeTokens l
  , length ts > 1
  , Just (_, w) <- [unsnoc ts]
  , tokText w == "where"
  , "=" `elem` texts ts
  , firstText l `notElem` map Just ["module", "data", "record", "instance"]
  ]

checkStackedPrivate :: Check
checkStackedPrivate _ f = [finding (lineStart l) "put private and variable on separate lines" | l <- codeLines f, take 2 (texts (codeTokens l)) == ["private", "variable"]]

checkBotElim :: Check
checkBotElim _ f = [finding (tokPos t) "prefer contradiction to ⊥-elim" | l <- codeLines f, t <- wordsOf l, tokText t == "⊥-elim"]

checkRewriteName :: RewriteOpts -> Check
checkRewriteName RewriteOpts {rewriteSuffixes} _ f =
  [ finding (advancePos (tokPos t) (T.take off body)) (n <> " is a rewrite rule; end its name in one of " <> T.intercalate ", " sfx)
  | l <- blockLines f
  , t@Token {tokKind = Pragma, tokText = body} <- lineTokens l
  , Just rest <- [T.stripPrefix "REWRITE" . T.stripStart =<< T.stripPrefix "{-#" body]
  , let base = T.length body - T.length rest
  , (i, n) <- wordOffsets rest
  , n /= "#-}"
  , let off = base + i
  , not (any (`T.isSuffixOf` n) sfx)
  ]
  where
    sfx = fromMaybe ["-β", "-η"] rewriteSuffixes

-- words with their offsets
wordOffsets :: Text -> [(Int, Text)]
wordOffsets = go 0
  where
    go i t =
      let (sp, rest) = T.span isSpace t
          (w, rest') = T.break isSpace rest
          j = i + T.length sp
       in if T.null w then [] else (j, w) : go (j + T.length w) rest'

-- position after the given text, across newlines
advancePos :: Pos -> Text -> Pos
advancePos p pre = case T.splitOn "\n" pre of
  [seg] -> p {posCol = posCol p + T.length seg}
  segs  -> Pos (posLine p + length segs - 1) (maybe 0 (T.length . snd) (unsnoc segs))
