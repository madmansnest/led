module LedRegularExpressions
  ( BRE(..)
  , AnchorType(..)
  , ClassItem(..)
  , RepeatKind(..)
  , Match(..)
  , parseBRE
  , matchBRE
  , matchBREInsensitive
  , matchesBRE
  , matchesBREInsensitive
  , matchAllBRE
  , matchAllBREInsensitive
  , buildReplacement
  ) where

import qualified Control.Monad.Trans.State.Strict as S
import Data.Char (isDigit, isAlpha, isAlphaNum, isUpper, isLower, isSpace, isPrint, isControl, isAscii, toLower, toUpper)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char

-- ---------------------------------------------------------------------------
-- AST
-- ---------------------------------------------------------------------------

data BRE
  = Literal Char
  | AnyChar                        -- .
  | Anchor AnchorType              -- ^ $
  | CharClass Bool [ClassItem]     -- [chars] / [^chars]
  | Group Int BRE                  -- \(...\) with group number
  | BackRef Int                    -- \1 - \9
  | Repeat BRE RepeatKind          -- atom* or atom\{n,m\}
  | Concat [BRE]
  deriving stock (Eq, Show)

data AnchorType = StartAnchor | EndAnchor
  deriving stock (Eq, Show)

data ClassItem
  = SingleChar Char
  | CharRange Char Char
  | PosixClass Text
  deriving stock (Eq, Show)

data RepeatKind = Star | Interval Int (Maybe Int)
  deriving stock (Eq, Show)

data Match = Match
  { matchText   :: Text
  , matchStart  :: Int
  , matchEnd    :: Int
  , matchGroups :: IM.IntMap Text
  } deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parser (Megaparsec-based)
-- ---------------------------------------------------------------------------

-- | Parser type with state for tracking group numbers
type BREParser = StateT Int (Parsec Void Text)

-- | Parse a BRE pattern string into an AST
parseBRE :: Text -> Either Text BRE
parseBRE input
  | T.null input = Left "Empty regular expression"
  | otherwise = case runParser (evalStateT pBRE 1) "" input of
      Left err -> Left $ toText $ errorBundlePretty err
      Right bre -> Right bre

-- | Top-level BRE parser
pBRE :: BREParser BRE
pBRE = do
  items <- many pAtomWithQuantifier
  case items of
    [x] -> pure x
    _   -> pure (Concat items)

-- | Parse atoms inside a group (stops at \))
pGroupContent :: BREParser BRE
pGroupContent = do
  items <- many (notFollowedBy (string "\\)") *> pAtomWithQuantifier)
  case items of
    [x] -> pure x
    _   -> pure (Concat items)

-- | Parse an atom with optional quantifier
pAtomWithQuantifier :: BREParser BRE
pAtomWithQuantifier = do
  atom <- pAtom
  case atom of
    Anchor _ -> pure atom  -- anchors can't be quantified
    _        -> pQuantifier atom

-- | Parse optional quantifier (* or \{n,m\})
pQuantifier :: BRE -> BREParser BRE
pQuantifier atom = choice
  [ Repeat atom Star <$ char '*'
  , pInterval atom
  , pure atom
  ]

-- | Parse interval expression \{n,m\}
-- Uses try only for the opening \{ to allow backtracking if not an interval,
-- but once committed, errors propagate without backtracking.
pInterval :: BRE -> BREParser BRE
pInterval atom = do
  _ <- try (char '\\' >> char '{')  -- backtrack only if not \{
  lo <- pNat <?> "number in interval"
  mhi <- optional $ do
    _ <- char ','
    optional pNat
  _ <- char '\\' >> char '}' <?> "\\} to close interval"
  case mhi of
    Nothing -> pure $ Repeat atom (Interval lo (Just lo))
    Just Nothing -> pure $ Repeat atom (Interval lo Nothing)
    Just (Just hi)
      | hi < lo   -> fail "Invalid interval: max < min"
      | otherwise -> pure $ Repeat atom (Interval lo (Just hi))

-- | Parse a natural number
pNat :: BREParser Int
pNat = do
  ds <- some digitChar
  case readMaybe ds of
    Just n  -> pure n
    Nothing -> fail "Invalid number"

-- | Parse a single atom
pAtom :: BREParser BRE
pAtom = choice
  [ AnyChar <$ char '.'
  , Anchor StartAnchor <$ char '^'
  , Anchor EndAnchor <$ char '$'
  , pBracket
  , pGroup           -- has internal try for \(
  , pBackRef         -- has internal try for \digit
  , pUnmatchedCloseGroup  -- has internal try for \)
  , pEscape          -- handles all other \X
  , Literal '*' <$ char '*'  -- leading/unquantified * is literal
  , pLiteral
  ]

-- | Fail on unmatched \)
pUnmatchedCloseGroup :: BREParser a
pUnmatchedCloseGroup = do
  _ <- try (char '\\' >> char ')')
  fail "Unmatched \\)"

-- | Parse a literal character (not special)
pLiteral :: BREParser BRE
pLiteral = Literal <$> satisfy (`notElem` specialChars)
  where specialChars = ".^$[\\*" :: [Char]

-- | Parse escape sequence
pEscape :: BREParser BRE
pEscape = do
  _ <- char '\\'
  c <- anySingle <?> "character after backslash"
  pure $ Literal c

-- | Parse backreference \1 through \9
-- Uses try to allow backtracking if not a valid backreference.
pBackRef :: BREParser BRE
pBackRef = do
  (_, d) <- try $ (,) <$> char '\\' <*> satisfy (\c -> c >= '1' && c <= '9')
  pure $ BackRef (fromEnum d - fromEnum '0')

-- | Parse group \(...\)
-- Uses try only for the opening \( to allow backtracking if not a group.
pGroup :: BREParser BRE
pGroup = do
  _ <- try (char '\\' >> char '(')  -- backtrack only if not \(
  gn <- S.get
  when (gn > 9) $ fail "Too many groups (max 9)"
  S.put (gn + 1)
  inner <- pGroupContent
  _ <- char '\\' >> char ')' <?> "\\) to close group"
  pure $ Group gn inner

-- | Parse bracket expression [...]
pBracket :: BREParser BRE
pBracket = do
  _ <- char '['
  negated <- isJust <$> optional (char '^')
  items <- pBracketItems True
  when (null items) $ fail "Empty bracket expression"
  _ <- char ']' <?> "closing ]"
  pure $ CharClass negated items

-- | Parse items inside bracket expression
pBracketItems :: Bool -> BREParser [ClassItem]
pBracketItems isFirst = do
  mc <- optional $ lookAhead anySingle
  case mc of
    Nothing -> fail "Unclosed bracket expression"
    Just ']'
      | isFirst -> do  -- ] as first char is literal
          _ <- anySingle
          rest <- pBracketItems False
          pure (SingleChar ']' : rest)
      | otherwise -> pure []
    Just '[' -> do
      -- Check for POSIX class [:name:]
      mPosix <- optional $ try pPosixClass
      case mPosix of
        Just pc -> do
          rest <- pBracketItems False
          pure (pc : rest)
        Nothing -> pRegularBracketItem
    Just _ -> pRegularBracketItem
  where
    pRegularBracketItem = do
      item <- pBracketItem
      rest <- pBracketItems False
      pure (item : rest)

-- | Parse a single bracket item (char or range)
pBracketItem :: BREParser ClassItem
pBracketItem = do
  c <- satisfy (`notElem` [']', '['])
  -- Check for range: c-hi where hi is not ]
  mRange <- optional $ try $ do
    _ <- char '-'
    hi <- satisfy (/= ']') <?> "range end character"
    pure hi
  case mRange of
    Just hi -> pure $ CharRange c hi
    Nothing -> pure $ SingleChar c

-- | Parse POSIX character class [:name:]
pPosixClass :: BREParser ClassItem
pPosixClass = do
  _ <- string "[:"
  name <- takeWhileP (Just "POSIX class name") isAlpha
  _ <- string ":]" <?> ":] to close POSIX class"
  pure $ PosixClass name

-- ---------------------------------------------------------------------------
-- Matcher
-- ---------------------------------------------------------------------------

type Groups = IM.IntMap (Int, Int)  -- group num -> (start, end) positions

data MState = MState
  { msPos       :: !Int
  , msGroups    :: !Groups
  , msInput     :: !Text      -- full input line (for backrefs and group extraction)
  , msRemaining :: !Text      -- remaining input from current position (for O(1) access)
  }

matchBRE :: BRE -> Text -> Maybe Match
matchBRE bre input = go 0 input
  where
    len = T.length input
    go pos remaining
      | pos > len = Nothing
      | otherwise =
          case tryMatch bre (MState pos IM.empty input remaining) of
            Just ms ->
              let s = pos
                  e = msPos ms
                  groups = IM.map (\(gs, ge) -> T.take (ge - gs) (T.drop gs input)) (msGroups ms)
              in Just (Match (T.take (e - s) (T.drop s input)) s e groups)
            Nothing ->
              -- If anchored at start, don't try other positions
              if startsWithStartAnchor bre then Nothing else go (pos + 1) (T.drop 1 remaining)

startsWithStartAnchor :: BRE -> Bool
startsWithStartAnchor (Anchor StartAnchor) = True
startsWithStartAnchor (Concat (Anchor StartAnchor : _)) = True
startsWithStartAnchor _ = False

matchesBRE :: BRE -> Text -> Bool
matchesBRE bre = isJust . matchBRE bre

-- | Case-insensitive version of matchBRE.
-- Literal characters are matched ignoring case.
matchBREInsensitive :: BRE -> Text -> Maybe Match
matchBREInsensitive bre input = go 0 input
  where
    len = T.length input
    go pos remaining
      | pos > len = Nothing
      | otherwise =
          case tryMatchInsensitive bre (MState pos IM.empty input remaining) of
            Just ms ->
              let s = pos
                  e = msPos ms
                  groups = IM.map (\(gs, ge) -> T.take (ge - gs) (T.drop gs input)) (msGroups ms)
              in Just (Match (T.take (e - s) (T.drop s input)) s e groups)
            Nothing ->
              if startsWithStartAnchor bre then Nothing else go (pos + 1) (T.drop 1 remaining)

-- | Case-insensitive version of matchesBRE.
matchesBREInsensitive :: BRE -> Text -> Bool
matchesBREInsensitive bre = isJust . matchBREInsensitive bre

-- | Try to match a BRE with a continuation (rest of the pattern).
-- The continuation receives the state after this node matches and
-- must return Just if the rest of the pattern succeeds.
tryMatchK :: BRE -> MState -> (MState -> Maybe MState) -> Maybe MState
tryMatchK bre ms k = case bre of
    Literal c -> do
      ch <- charAt ms
      guard (ch == c)
      k (advance ms)

    AnyChar -> do
      _ <- charAt ms
      k (advance ms)

    Anchor StartAnchor ->
      if msPos ms == 0 then k ms else Nothing

    Anchor EndAnchor ->
      if msPos ms == T.length (msInput ms) then k ms else Nothing

    CharClass negated items -> do
      ch <- charAt ms
      let member = any (classItemMatches ch) items
      guard (if negated then not member else member)
      k (advance ms)

    Group n inner ->
      let start = msPos ms
      in tryMatchK inner ms $ \ms' ->
           let end = msPos ms'
           in k (ms' { msGroups = IM.insert n (start, end) (msGroups ms') })

    BackRef n -> do
      (gs, ge) <- IM.lookup n (msGroups ms)
      let captured = T.take (ge - gs) (T.drop gs (msInput ms))
          clen = T.length captured
      guard (T.isPrefixOf captured (msRemaining ms))
      k (ms { msPos = msPos ms + clen, msRemaining = T.drop clen (msRemaining ms) })

    Repeat atom Star -> matchRepeatK atom 0 Nothing ms k

    Repeat atom (Interval lo mhi) -> matchRepeatK atom lo mhi ms k

    Concat [] -> k ms
    Concat (x:xs) -> tryMatchK x ms (\ms' -> tryMatchK (Concat xs) ms' k)

-- | Convenience wrapper without continuation.
tryMatch :: BRE -> MState -> Maybe MState
tryMatch bre ms = tryMatchK bre ms Just

-- | Case-insensitive version of tryMatchK.
-- Literal characters are matched using toLower comparison.
tryMatchKInsensitive :: BRE -> MState -> (MState -> Maybe MState) -> Maybe MState
tryMatchKInsensitive bre ms k = case bre of
    Literal c -> do
      ch <- charAt ms
      guard (toLower ch == toLower c)
      k (advance ms)

    AnyChar -> do
      _ <- charAt ms
      k (advance ms)

    Anchor StartAnchor ->
      if msPos ms == 0 then k ms else Nothing

    Anchor EndAnchor ->
      if msPos ms == T.length (msInput ms) then k ms else Nothing

    CharClass negated items -> do
      ch <- charAt ms
      let member = any (classItemMatchesInsensitive ch) items
      guard (if negated then not member else member)
      k (advance ms)

    Group n inner ->
      let start = msPos ms
      in tryMatchKInsensitive inner ms $ \ms' ->
           let end = msPos ms'
           in k (ms' { msGroups = IM.insert n (start, end) (msGroups ms') })

    BackRef n -> do
      (gs, ge) <- IM.lookup n (msGroups ms)
      let captured = T.take (ge - gs) (T.drop gs (msInput ms))
          clen = T.length captured
      -- Case-insensitive prefix check
      guard (T.length (msRemaining ms) >= clen &&
             T.map toLower (T.take clen (msRemaining ms)) == T.map toLower captured)
      k (ms { msPos = msPos ms + clen, msRemaining = T.drop clen (msRemaining ms) })

    Repeat atom Star -> matchRepeatKInsensitive atom 0 Nothing ms k

    Repeat atom (Interval lo mhi) -> matchRepeatKInsensitive atom lo mhi ms k

    Concat [] -> k ms
    Concat (x:xs) -> tryMatchKInsensitive x ms (\ms' -> tryMatchKInsensitive (Concat xs) ms' k)

-- | Convenience wrapper without continuation (case-insensitive).
tryMatchInsensitive :: BRE -> MState -> Maybe MState
tryMatchInsensitive bre ms = tryMatchKInsensitive bre ms Just

charAt :: MState -> Maybe Char
charAt ms = case T.uncons (msRemaining ms) of
  Just (c, _) -> Just c
  Nothing     -> Nothing

advance :: MState -> MState
advance ms = ms { msPos = msPos ms + 1, msRemaining = T.drop 1 (msRemaining ms) }

-- | Greedy repeat with backtracking via continuation.
-- Collects all possible states (0..max matches), tries longest first with k.
matchRepeatK :: BRE -> Int -> Maybe Int -> MState -> (MState -> Maybe MState) -> Maybe MState
matchRepeatK atom lo mhi ms k =
    let states = collectStates atom mhi ms
        -- states is in order [0 matches, 1 match, ...], reverse for greedy (longest first)
        candidates = reverse (drop lo states)
    in asum (map k candidates)

-- | Collect all possible states from matching atom 0, 1, 2, ... times.
-- Stops at max (if given) or when no more matches are possible.
collectStates :: BRE -> Maybe Int -> MState -> [MState]
collectStates atom mhi ms0 = go ms0 [ms0] 1
  where
    limit = fromMaybe maxBound mhi
    go cur acc n
      | n > limit = reverse acc
      | otherwise = case tryMatch atom cur of
          Just cur'
            | msPos cur' == msPos cur -> reverse acc  -- zero-width, stop
            | otherwise -> go cur' (cur' : acc) (n + 1)
          Nothing -> reverse acc

-- | Case-insensitive version of matchRepeatK.
matchRepeatKInsensitive :: BRE -> Int -> Maybe Int -> MState -> (MState -> Maybe MState) -> Maybe MState
matchRepeatKInsensitive atom lo mhi ms k =
    let states = collectStatesInsensitive atom mhi ms
        candidates = reverse (drop lo states)
    in asum (map k candidates)

-- | Case-insensitive version of collectStates.
collectStatesInsensitive :: BRE -> Maybe Int -> MState -> [MState]
collectStatesInsensitive atom mhi ms0 = go ms0 [ms0] 1
  where
    limit = fromMaybe maxBound mhi
    go cur acc n
      | n > limit = reverse acc
      | otherwise = case tryMatchInsensitive atom cur of
          Just cur'
            | msPos cur' == msPos cur -> reverse acc
            | otherwise -> go cur' (cur' : acc) (n + 1)
          Nothing -> reverse acc

-- ---------------------------------------------------------------------------
-- Character class matching
-- ---------------------------------------------------------------------------

classItemMatches :: Char -> ClassItem -> Bool
classItemMatches ch (SingleChar c) = ch == c
classItemMatches ch (CharRange lo hi) = ch >= lo && ch <= hi
classItemMatches ch (PosixClass name) = case name of
    "alpha"  -> isAlpha ch
    "digit"  -> isDigit ch
    "alnum"  -> isAlphaNum ch
    "upper"  -> isUpper ch
    "lower"  -> isLower ch
    "space"  -> isSpace ch
    "blank"  -> ch == ' ' || ch == '\t'
    "print"  -> isPrint ch
    "graph"  -> isPrint ch && ch /= ' '
    "cntrl"  -> isControl ch
    "punct"  -> isPrint ch && not (isAlphaNum ch) && ch /= ' '
    "xdigit" -> isDigit ch || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
    "ascii"  -> isAscii ch
    _        -> False

-- | Case-insensitive version of classItemMatches.
classItemMatchesInsensitive :: Char -> ClassItem -> Bool
classItemMatchesInsensitive ch (SingleChar c) = toLower ch == toLower c
classItemMatchesInsensitive ch (CharRange lo hi) =
    -- For case-insensitive range matching, check if either case is in range
    (ch >= lo && ch <= hi) || (toLower ch >= toLower lo && toLower ch <= toLower hi)
                           || (toUpper ch >= toUpper lo && toUpper ch <= toUpper hi)
classItemMatchesInsensitive ch cls@(PosixClass _) = classItemMatches ch cls

-- ---------------------------------------------------------------------------
-- Multi-match and replacement
-- ---------------------------------------------------------------------------

-- | Find all non-overlapping leftmost-longest matches.
matchAllBRE :: BRE -> Text -> [Match]
matchAllBRE bre input = go 0
  where
    len = T.length input
    go pos
      | pos > len = []
      | otherwise =
          case matchBRE bre (T.drop pos input) of
            Nothing -> []
            Just m ->
              let m' = m { matchStart = matchStart m + pos, matchEnd = matchEnd m + pos }
                  nextPos = matchEnd m' -- advance past match
                  -- If zero-width match, advance by 1 to avoid infinite loop
                  nextPos' = if nextPos == pos then pos + 1 else nextPos
              in m' : go nextPos'

-- | Case-insensitive version of matchAllBRE.
matchAllBREInsensitive :: BRE -> Text -> [Match]
matchAllBREInsensitive bre input = go 0
  where
    len = T.length input
    go pos
      | pos > len = []
      | otherwise =
          case matchBREInsensitive bre (T.drop pos input) of
            Nothing -> []
            Just m ->
              let m' = m { matchStart = matchStart m + pos, matchEnd = matchEnd m + pos }
                  nextPos = matchEnd m'
                  nextPos' = if nextPos == pos then pos + 1 else nextPos
              in m' : go nextPos'

-- | Build replacement text from a replacement pattern and a match.
-- & → whole match, \1-\9 → captured group, \\ → literal backslash.
buildReplacement :: Text -> Match -> Text
buildReplacement repl m = go repl
  where
    go t = case T.uncons t of
      Nothing -> T.empty
      Just ('&', r) -> matchText m <> go r
      Just ('\\', r) -> case T.uncons r of
        Just (c, r')
          | c >= '1' && c <= '9' ->
              let n = fromEnum c - fromEnum '0'
              in fromMaybe T.empty (IM.lookup n (matchGroups m)) <> go r'
          | c == '\\' -> "\\" <> go r'
          | c == '&'  -> "&" <> go r'
          | c == 'n'  -> "\n" <> go r'
          | otherwise -> T.singleton c <> go r'
        Nothing -> "\\"
      Just (c, r) -> T.singleton c <> go r
