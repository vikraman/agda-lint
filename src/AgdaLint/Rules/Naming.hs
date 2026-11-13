-- naming rules
module AgdaLint.Rules.Naming
  ( namingRules
  , isSubscript
  , isSingleLetterVar
  , subscripted
  , hasLetterFrom
  , clauseNames
  ) where

import           AgdaLint.Lexer
import           AgdaLint.Project
import           AgdaLint.Rule
import           AgdaLint.Source
import           AgdaLint.Types
import           Control.Monad   (void)
import           Data.Char       (chr, digitToInt, isDigit, isLetter, isSpace,
                                  ord)
import           Data.Foldable   (toList)
import           Data.Ix         (inRange)
import           Data.List       (find, isPrefixOf, unsnoc)
import qualified Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Text.Megaparsec hiding (Pos, State, Token, match)
import qualified Toml
import           Toml.Schema     hiding (table)

namingRules :: [Rule]
namingRules =
  [ core (rule "asciiArrow" Error "use → instead of ->" checkAsciiArrow)
  , core (rule "asciiLambda" Error "use λ instead of \\" checkAsciiLambda)
  , rule "patternLambda" Error "lambdas that match on patterns use λ { (…) → … }" checkPatternLambda
  , ruleWith "asciiSubscript" Error "use unicode subscripts (V₁) instead of ascii digits (V1)" ["asciiSubscriptIgnore"] checkAsciiSubscript
  , rule "prime" Error "no primes on single-letter variables (Γ', σ'')" checkPrime
  , rule "prefixType" Error "write types that have a syntax declaration in their notation" checkPrefixType
  , core (ruleWith "bannedName" Error "names listed in [bannedNames]" ["bannedNames", "fixBannedNames"] checkBannedName)
  , core (ruleWith "sortLetter" Warning "typed binders and variables use the letters configured in [[sorts]]" ["sorts"] checkSortLetter)
  , core (ruleWith "reservedLetter" Error "letters of a reserved sort are not used for binders of other types" ["sorts"] checkReservedLetter)
  , ruleWith "unneededSubscript" Warning "a lone M₁ in a clause should be M" ["subscriptExempt"] checkUnneededSubscript
  , core (ruleWith "pairSubscript" Warning "two of a kind use the letter pair (M N), not M M₁" ["pairs", "pairSkip", "subscriptExempt"] checkPairSubscript)
  , rule "unusedVariable" Warning "generalizable variables that are never used" checkUnusedVariable
  ]

------------------------------------------------------------------------
-- options

newtype SubscriptOpts = SubscriptOpts {asciiSubscriptIgnore :: Maybe [Text]}
  deriving (Generic)

data BannedOpts = BannedOpts {bannedNames :: Maybe (Map Text Text), fixBannedNames :: Maybe [Text]}
  deriving (Generic)

data Banned = Banned {bannedReplacement :: Maybe Text, bannedAutoFix :: Bool}

newtype SortOpts = SortOpts {sorts :: Maybe [SortSpec]}
  deriving (Generic)

data SortMatch = HeadsAnyOf [Text] | ExactlyOneOf [Text]

data SortSpec = SortSpec {sortMatch :: SortMatch, sortLetters :: [Text], sortReserved :: Bool}

data RawSort = RawSort {heads :: Maybe [Text], exact :: Maybe [Text], letters :: Text, reserved :: Maybe Bool}
  deriving (Generic)

newtype ExemptOpts = ExemptOpts {subscriptExempt :: Maybe [Text]}
  deriving (Generic)

data PairOpts = PairOpts {pairs :: Maybe [Pair], pairSkip :: Maybe [PairSkip], subscriptExempt :: Maybe [Text]}
  deriving (Generic)

data Pair = Pair Text Text
  deriving (Eq)

data PairSkip = PairSkip {skipPath :: FilePath, skipPairs :: [Pair]}

instance FromValue SubscriptOpts where fromValue = genericFromTable
instance FromValue BannedOpts where fromValue = genericFromTable
instance FromValue SortOpts where fromValue = genericFromTable
instance FromValue RawSort where fromValue = genericFromTable
instance FromValue ExemptOpts where fromValue = genericFromTable
instance FromValue PairOpts where fromValue = genericFromTable

instance FromValue SortSpec where
  fromValue v = do
    RawSort {heads, exact, letters, reserved} <- fromValue v
    let spec m = pure (SortSpec m (map T.singleton (T.unpack letters)) (fromMaybe False reserved))
    case (heads, exact) of
      _ | T.null letters -> failAt (Toml.valueAnn v) "a sort needs letters"
      (Just hs, Nothing) -> spec (HeadsAnyOf hs)
      (Nothing, Just es) -> spec (ExactlyOneOf es)
      _                  -> failAt (Toml.valueAnn v) "a sort needs exactly one of heads or exact"

instance FromValue Pair where
  fromValue v = do
    t <- fromValue v
    case T.unpack t of
      [a, b] -> pure (Pair (T.singleton a) (T.singleton b))
      _      -> failAt (Toml.valueAnn v) ("pairs must be two letters: " <> T.unpack t)

instance FromValue PairSkip where
  fromValue = parseTableFromValue (PairSkip <$> (T.unpack <$> reqKey "path") <*> reqKey "pairs")

------------------------------------------------------------------------
-- checks

subscriptDigit :: Char -> Char
subscriptDigit c = chr (ord '₀' + digitToInt c)

isSubscript :: Char -> Bool
isSubscript = inRange ('₀', '₉')

checkAsciiArrow :: Check
checkAsciiArrow _ f =
  [fixed (tokPos t) "use → instead of ->" (replaceTok t "→") | l <- codeLines f, t <- wordsOf l, tokText t == "->"]

checkAsciiLambda :: Check
checkAsciiLambda _ f =
  [ fixed (tokPos t) "use λ instead of \\" (ReplaceSpan (tokPos t) 1 (if spaced then "λ" else "λ "))
  | l <- codeLines f
  , t <- wordsOf l
  , Just rest <- [T.stripPrefix "\\" (tokText t)]
  -- not an operator like \\ or \/
  , maybe True (\(c, _) -> isLetter c || c == '_') (T.uncons rest)
  , let spaced = maybe True (isSpace . fst) (T.uncons (T.drop (posCol (tokPos t) + 1) (lineText l)))
  ]

checkPatternLambda :: Check
checkPatternLambda _ f =
  [ finding (tokPos p) "use a pattern-matching lambda: λ { (…) → … }"
  | l <- codeLines f
  , lam : rest <- List.tails (codeTokens l)
  , tokText lam == "λ"
  , take 1 (map tokText rest) /= ["{"]
  , p <- patterns (takeWhile ((/= "→") . tokText) rest)
  ]
  where
    -- parenthesised binders without a type are patterns; () is the absurd lambda
    patterns (t : ts)
      | tokKind t == Delim, tokText t == "(" =
          let (inside, after) = group (1 :: Int) [] ts
           in [t | not (null inside), ":" `notElem` map tokText inside] ++ patterns after
      -- an unmatched closer ends the lambda
      | tokText t `elem` [")", "}", ";"] = []
      | otherwise = patterns ts
    patterns [] = []
    group _ acc [] = (reverse acc, [])
    group d acc (t : ts)
      | tokText t == ")" && d == 1 = (reverse acc, ts)
      | tokText t == ")" = group (d - 1) (t : acc) ts
      | tokText t == "(" = group (d + 1) (t : acc) ts
      | otherwise = group d (t : acc) ts

checkAsciiSubscript :: SubscriptOpts -> Check
checkAsciiSubscript SubscriptOpts {asciiSubscriptIgnore} _ f =
  [ fixed (tokPos t) ("use " <> u <> " instead of " <> tokText t) (replaceTok t u)
  | l <- codeLines f
  , t <- wordsOf l
  , let (letters, rest) = T.span isLetter (tokText t)
        (digits, primes) = T.span isDigit rest
  , T.length letters `elem` [1, 2]
  , not (T.null digits)
  , T.all (== '\'') primes
  , not (any (`T.isPrefixOf` tokText t) (fromMaybe [] asciiSubscriptIgnore))
  , let u = letters <> T.map subscriptDigit digits <> primes
  ]

isSingleLetterVar :: Text -> Bool
isSingleLetterVar t = case T.unpack t of
  c : rest -> isLetter c && all isSubscript rest
  []       -> False

checkPrime :: Check
checkPrime _ f =
  [ finding (tokPos t) ("no primes on variables: " <> tokText t)
  | l <- codeLines f
  , t <- wordsOf l
  , let (base, primes) = T.break (== '\'') (tokText t)
  , not (T.null primes)
  , T.all (== '\'') primes
  , isSingleLetterVar base
  ]

-- application arguments
isAtom :: Set Text -> Token -> Bool
isAtom operators t =
  tokKind t == Word
    && Set.notMember (tokText t) operators
    && not ("]" `T.isSuffixOf` tokText t)

-- argument span
data Arg = Arg {argFirst :: Token, argLast :: Token}

argument :: Set Text -> TokParser Arg
argument operators = parenthesised <|> (\t -> Arg t t) <$> satisfy (isAtom operators)
  where
    parenthesised = do
      o <- delim "("
      Arg o <$> inside
    inside = skipMany (void (delim "(" *> inside) <|> void (satisfy (not . isParen))) *> delim ")"
    isParen t = tokKind t == Delim && tokText t `elem` ["(", ")"]

-- notation application
application :: Set Text -> Map Text Notation -> TokParser (Token, Notation, [Arg])
application operators table = do
  t <- name
  n <- maybe empty pure (Map.lookup (tokText t) table)
  args <- count (length (notationParams n)) (argument operators)
  notFollowedBy (argument operators)
  pure (t, n, args)

spanText :: Line -> Token -> Token -> Text
spanText l a b = T.take (posCol (tokPos b) + T.length (tokText b) - posCol (tokPos a)) (T.drop (posCol (tokPos a)) (lineText l))

-- prefix uses
prefixUses :: Set Text -> Map Text Notation -> Line -> [(Token, Int, Text)]
prefixUses operators table l
  | firstText l `elem` [Just "syntax", Just "data"] = []
  | otherwise = mapMaybe render (fromMaybe [] (runTokens (scan (lookAhead (application operators table) <* anySingle)) (codeTokens l)))
  where
    render (t, n, args) = do
      (_, end) <- fmap argLast <$> unsnoc args
      let width = posCol (tokPos end) + T.length (tokText end) - posCol (tokPos t)
          sub = Map.fromList (zip (notationParams n) [spanText l (argFirst a) (argLast a) | a <- args])
      pure (t, width, T.unwords [Map.findWithDefault x x sub | x <- notationRhs n])

checkPrefixType :: Check
checkPrefixType ctx f =
  [ fixed (tokPos t) ("use the syntax form: " <> out) (ReplaceSpan (tokPos t) w out)
  | let table = Map.findWithDefault Map.empty (srcPath f) (ctxNotations ctx)
  , not (Map.null table)
  , l <- codeLines f
  , (t, w, out) <- prefixUses (ctxOperators ctx) table l
  ]

checkBannedName :: BannedOpts -> Check
checkBannedName BannedOpts {bannedNames, fixBannedNames} _ f =
  [ Finding (tokPos t) msg [replaceTok t r | bannedAutoFix b, Just r <- [bannedReplacement b]]
  | l <- codeLines f
  , t <- wordsOf l
  , Just b <- [Map.lookup (tokText t) table]
  , let msg = maybe (tokText t <> " is not allowed") (\r -> "use " <> r <> " instead of " <> tokText t) (bannedReplacement b)
  ]
  where
    table = Map.mapWithKey banned (fromMaybe Map.empty bannedNames)
    banned k v = Banned (if T.null v then Nothing else Just v) (k `elem` fromMaybe [] fixBannedNames)

hasLetterFrom :: [Text] -> Text -> Bool
hasLetterFrom letters v = any (maybe False (T.all isSubscript) . (`T.stripPrefix` v)) letters

checkSortLetter :: SortOpts -> Check
checkSortLetter SortOpts {sorts} _ f =
  [ finding (tokPos n) (tokText n <> " : " <> T.unwords cod <> " should be one of " <> T.unwords (sortLetters s))
  | (ns, cod) <- [(binderNames b, codomain (binderType b)) | l <- codeLines f, b <- binders (codeTokens l)]
      ++ [(varNames v, codomainOf (varType v)) | v <- srcVariables f]
  , Just s <- [find (sortMatches cod) (fromMaybe [] sorts)]
  , n <- ns
  , tokText n /= "_"
  , not (hasLetterFrom (sortLetters s) (tokText n))
  ]

checkReservedLetter :: SortOpts -> Check
checkReservedLetter SortOpts {sorts} _ f =
  [ finding (tokPos n) (tokText n <> " is reserved for " <> T.unwords (sortNames s) <> ", but has type " <> T.unwords (texts (binderType b)))
  | l <- codeLines f
  , b <- binders (codeTokens l)
  , let cod = codomain (binderType b)
  , not (null cod), cod /= ["_"]
  -- universes are types too
  , take 1 cod /= ["Set"]
  , n <- binderNames b
  , Just s <- [find (\o -> sortReserved o && hasLetterFrom (sortLetters o) (tokText n)) specs]
  , not (sortMatches cod s)
  ]
  where
    specs = fromMaybe [] sorts
    sortNames s = case sortMatch s of
      HeadsAnyOf hs   -> hs
      ExactlyOneOf es -> es

sortMatches :: [Text] -> SortSpec -> Bool
sortMatches cod s = case sortMatch s of
  HeadsAnyOf hs   -> any (`elem` hs) cod
  ExactlyOneOf es -> case cod of [c] -> c `elem` es; _ -> False

-- clause names without named-argument keys
clauseNames :: Clause -> [Text]
clauseNames = concatMap (dropKeys . codeTokens) . clauseLines
  where
    dropKeys (Token {tokKind = Delim, tokText = "{"} : _ : e@(Token {tokText = "="} : _)) = dropKeys e
    dropKeys (t : ts) = [tokText t | tokKind t == Word] ++ dropKeys ts
    dropKeys [] = []

-- clauses for subscript rules
relevantClauses :: Maybe [Text] -> SourceFile -> [Clause]
relevantClauses exempt f =
  [ c
  | c <- srcClauses f
  , firstText (clauseHead c) /= Just "syntax"
  , not (any (`elem` clauseNames c) (fromMaybe [] exempt))
  ]

clausePos :: Clause -> Pos
clausePos c = Pos (lineNo (clauseHead c)) 0

subscripted :: Text -> Maybe (Text, Text)
subscripted t = case T.unpack t of
  c : rest@(_ : _) | isLetter c, all isSubscript rest -> Just (T.singleton c, T.pack rest)
  _ -> Nothing

checkUnneededSubscript :: ExemptOpts -> Check
checkUnneededSubscript ExemptOpts {subscriptExempt} _ f =
  [ finding (clausePos c) (v <> " is the only " <> letter <> " here; use " <> letter)
  | c <- relevantClauses subscriptExempt f
  , let names = Set.fromList (clauseNames c)
        families = Map.fromListWith Set.union [(letter, Set.singleton t) | t <- toList names, Just (letter, _) <- [subscripted t]]
  , (letter, vs) <- Map.toList families
  , Set.notMember letter names
  , [v] <- [Set.toList vs]
  , Set.notMember v (srcNamedKeys f)
  , Set.notMember v (srcModuleParams f)
  ]

checkPairSubscript :: PairOpts -> Check
checkPairSubscript PairOpts {pairs, pairSkip, subscriptExempt} _ f =
  [ finding (clausePos c) (T.unwords (Set.toList used) <> ": use " <> a <> " and " <> b)
  | c <- relevantClauses subscriptExempt f
  , let names = Set.fromList (clauseNames c)
  , Pair a b <- active
  , let used = Set.filter (hasLetterFrom [a, b]) names
  , Set.size used == 2
  , used /= Set.fromList [a, b]
  , Set.null (Set.intersection used (srcNamedKeys f))
  , Set.size (Set.map (T.take 1) used) == 1
  , Set.notMember b names
  ]
  where
    skipped = concat [skipPairs s | s <- fromMaybe [] pairSkip, skipPath s `isPrefixOf` srcPath f]
    active = filter (`notElem` skipped) (fromMaybe [] pairs)

checkUnusedVariable :: Check
checkUnusedVariable ctx f =
  [ finding (tokPos n) (tokText n <> " is declared but never used")
  | v <- srcVariables f
  , n <- varNames v
  , sum [Map.findWithDefault 0 (tokText n) (srcTokenCounts g) | g <- Map.findWithDefault [f] (srcPath f) (ctxImporters ctx)] <= 1
  ]
