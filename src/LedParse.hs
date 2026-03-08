{-# LANGUAGE StrictData #-}
module LedParse
  ( Addr(..), LineRange(..), DocRange(..), FullRange(..), Suffix(..)
  , Command(..), TargetAddr(..), SubstFlags(..), ParseResult(..)
  , Parser, parseCommand, feedLines, needsMoreInput
  , getCommandRange, setCommandRange
  , runParse, hasDocRange, isValidFunctionName, systemCommandNames
    -- * Partial parsing support (for visual mode)
  , pAddr, pDocRange, pLineRange, pFullRange
  ) where

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit, isLower)
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Addr
  = Current | LastLine | Mark Char | Number Int
  | Next Text | Prev Text | AddrOffset Addr Int
  deriving stock (Eq, Show)

data LineRange
  = LineDefault | LineSingle Addr | LineFree Addr Addr
  | LineBound Addr Addr | LinePrevious
  deriving stock (Eq, Show)

data DocRange
  = DocDefault | DocSingle Addr | DocFree Addr Addr | DocBound Addr Addr
  | DocAll | DocModified | DocManage | DocParam | DocPrevious
  deriving stock (Eq, Show)

data FullRange = FullRange { frDoc :: DocRange, frLines :: LineRange }
  deriving stock (Eq, Show)

data Suffix = NoSuffix | PrintSuffix | NumberSuffix | ListSuffix
  deriving stock (Eq, Show)

data Command
  = Append FullRange Text Suffix | Change FullRange Text Suffix | Insert FullRange Text Suffix
  | FnList | FnQuery Text | FnDefine Text [Text] Text
  | Delete FullRange Suffix | Join FullRange Suffix
  | Move FullRange TargetAddr Suffix | Transfer FullRange TargetAddr Suffix
  | MarkLine FullRange Char | PrintLines FullRange Suffix | PrintLineNumber FullRange
  | Substitute FullRange Text Text SubstFlags Suffix
  | Global FullRange Text Text | GlobalInteractive FullRange Text
  | GlobalReverse FullRange Text Text | GlobalReverseInteractive FullRange Text
  | Edit FullRange (Maybe FilePath) | EditAlways FullRange (Maybe FilePath)
  | EditShell FullRange Text | EditShellAlways FullRange Text
  | Filename FullRange (Maybe FilePath)
  | ReadFile FullRange (Maybe FilePath) | ReadShell FullRange Text
  | Write FullRange (Maybe FilePath) | WriteShell FullRange Text
  | WriteQuit FullRange (Maybe FilePath) | WriteQuitShell FullRange Text
  | ImportMacro FilePath | ImportDir
  | Undo FullRange | Redo FullRange | ShellCommand Text | ShellFilter FullRange Text | Comment
  | Quit | QuitAlways | TogglePrompt (Maybe Text) | Help | HelpMode
  | VisualMode  -- Toggle visual editing mode (vi command)
  | InvokeFunction Text FullRange [Text] Suffix
  deriving stock (Eq, Show)

data TargetAddr
  = LocalTarget Addr | CrossDocTarget DocRange Addr | ParamTarget Int Addr
  deriving stock (Eq, Show)

data SubstFlags = SubstFlags { sfGlobal :: Bool, sfCount :: Int, sfInsensitive :: Bool }
  deriving stock (Eq, Show)

data ParseResult = Complete Command | Incomplete | Failed Text
  deriving stock (Eq, Show)

type Parser = Parsec Void Text

pAddr :: Parser Addr
pAddr = do
  base <- choice
    [ Current <$ char '.', LastLine <$ char '$'
    , Mark <$> (char '\'' *> satisfy isLower), Number <$> L.decimal
    , pSearchAddr '/' Next, pSearchAddr '?' Prev, pBareOffset ]
  offsets <- many pOffsetSuffix
  pure (foldl' AddrOffset base offsets)

pSearchAddr :: Char -> (Text -> Addr) -> Parser Addr
pSearchAddr delim ctor = do
  _ <- char delim
  re <- takeWhileP Nothing (\c -> c /= delim && c /= '\n')
  _ <- optional (char delim)
  pure (ctor re)

pBareOffset :: Parser Addr
pBareOffset = do
  sign <- (1 <$ char '+') <|> ((-1) <$ char '-')
  n <- optional L.decimal
  pure (AddrOffset Current (sign * fromMaybe 1 n))

pOffsetSuffix :: Parser Int
pOffsetSuffix = do
  sign <- (1 <$ char '+') <|> ((-1) <$ char '-')
  n <- optional L.decimal
  pure (sign * fromMaybe 1 n)

pPairRange :: Char -> (Addr -> Addr -> a) -> Parser a
pPairRange sep ctor = try withFrom <|> bareSep
  where
    withFrom = ctor <$> pAddr <* char sep <*> option Current pAddr
    bareSep = char sep *> (ctor (Number 1) <$> option LastLine pAddr)

pLineRange :: Parser LineRange
pLineRange = choice
  [ try (pPairRange ';' LineBound), try (pPairRange ',' LineFree)
  , LinePrevious <$ char '%', LineSingle <$> try pAddr, pure LineDefault ]

pDocRangeWithColon :: Parser DocRange
pDocRangeWithColon = choice
  [ try (pPairRange ';' DocBound <* char ':')
  , try (pPairRange ',' DocFree <* char ':')
  , try (DocSingle <$> pAddr <* char ':') ]

pDocRange :: Parser DocRange
pDocRange = choice
  [ DocManage <$ (string "&&" <* optional (char ':'))
  , DocModified <$ (string "&*" <* optional (char ':'))
  , DocAll <$ (char '&' <* optional (char ':'))
  , DocParam <$ (char '@' <* optional (char ':'))
  , DocPrevious <$ string "%:"
  , try pDocRangeWithColon, pure DocDefault ]

pFullRange :: Parser FullRange
pFullRange = FullRange <$> pDocRange <*> pLineRange

pSuffix :: Parser Suffix
pSuffix = option NoSuffix $ choice
  [ PrintSuffix <$ char 'p', NumberSuffix <$ char 'n', ListSuffix <$ char 'l' ]

pTargetAddr :: Parser TargetAddr
pTargetAddr = choice
  [ try (CrossDocTarget <$> pDocRangeWithColon <*> option Current pAddr)
  , try (CrossDocTarget <$> pSpecialDocRange <*> option Current pAddr)
  , try (ParamTarget <$> (length <$> some (char '^') <* char '@') <*> option Current pAddr)
  , LocalTarget <$> pAddr ]
  where
    -- Special doc ranges for target addresses (with optional colon)
    pSpecialDocRange = choice
      [ DocManage <$ (string "&&" <* optional (char ':'))
      , DocModified <$ (string "&*" <* optional (char ':'))
      , DocAll <$ (char '&' <* optional (char ':'))
      , DocParam <$ (char '@' <* optional (char ':'))
      ]

pCommand :: Set Text -> Parser Command
pCommand userFns = do
  space
  choice [ ShellCommand <$> (char '!' *> takeRest), Comment <$ (char '#' *> takeRest)
         , try (pRangedCommand userFns), pUnrangedCommand ]

pUnrangedCommand :: Parser Command
pUnrangedCommand = choice
  [ Quit <$ string "q" <* eof, QuitAlways <$ string "Q" <* eof
  , Help <$ string "h" <* eof, HelpMode <$ string "H" <* eof
  , VisualMode <$ string "vi" <* eof  -- Toggle visual editing mode
  , char 'P' *> (TogglePrompt . justOrNothing . T.strip <$> takeRest) ]
  where justOrNothing t = if T.null t then Nothing else Just t

pRangedCommand :: Set Text -> Parser Command
pRangedCommand userFns = do
  range <- pFullRange
  space
  choice
    [ try (pTextCmd 'a' (Append range)), try (pTextCmd 'c' (Change range))
    , try (pInsertCmd range), try (pDeleteCmd range)
    , try (pSubstitute range)
    , try (pGlobalWithCmd 'g' (Global range)), try (pGlobalNoCmd 'G' (GlobalInteractive range))
    , try (pGlobalWithCmd 'v' (GlobalReverse range)), try (pGlobalNoCmd 'V' (GlobalReverseInteractive range))
    , try (pFileCmd 'E' (EditAlways range) (EditShellAlways range) False)
    , try (pFileCmd 'e' (Edit range) (EditShell range) True)
    , try pFnCommand
    , try (char 'f' *> notFollowedBy alphaNumChar *> (Filename range . toMaybePath <$> takeRest))
    , try (string "imd" *> space *> eof $> ImportDir)
    , try (string "im" *> space1 *> (ImportMacro . toString . T.strip <$> takeRest) >>= \c -> c <$ guard (c /= ImportMacro ""))
    , try (pSimpleCmd 'j' (Join range)), try (pMarkCmd range)
    , try (pMoveTransfer 'm' (Move range)), try (pMoveTransfer 't' (Transfer range))
    , PrintLineNumber range <$ (char '=' *> space *> eof)
    , try (pPrintCmd range), try (pFileCmd 'r' (ReadFile range) (ReadShell range) True)
    , try (ShellFilter range <$> (char '!' *> takeRest))
    , try (pNoSuffixCmd 'u' (Undo range)), try (pNoSuffixCmd 'U' (Redo range))
    , try (string "wq" *> notFollowedBy alphaNumChar *> space *> pFileOrShell (WriteQuit range) (WriteQuitShell range))
    , try (pFileCmd 'w' (Write range) (WriteShell range) True)
    , try (pUserFunction userFns range), pBareRange range ]

notSuffix :: Parser ()
notSuffix = notFollowedBy (satisfy (\c -> isAscii c && isAlpha c && c `notElem` ("pnl" :: String)))

pTextCmd :: Char -> (Text -> Suffix -> Command) -> Parser Command
pTextCmd c ctor = char c *> notSuffix *> (uncurry ctor <$> pTextBody)

pInsertCmd :: FullRange -> Parser Command
pInsertCmd range = do
  _ <- char 'i'
  notFollowedBy (satisfy (\ch -> isAscii ch && isAlpha ch && ch `notElem` ("pnl" :: String)))
  (txt, suf) <- pTextBody
  pure (Insert range txt suf)

pDeleteCmd :: FullRange -> Parser Command
pDeleteCmd range = char 'd' *> notSuffix *> (Delete range <$> pSuffix <* space <* eof)

pSimpleCmd :: Char -> (Suffix -> Command) -> Parser Command
pSimpleCmd c ctor = char c *> (ctor <$> pSuffix) <* space <* eof

pNoSuffixCmd :: Char -> Command -> Parser Command
pNoSuffixCmd c cmd = char c *> space *> eof $> cmd

pMarkCmd :: FullRange -> Parser Command
pMarkCmd range = char 'k' *> (MarkLine range <$> satisfy isLower) <* space <* eof

pMoveTransfer :: Char -> (TargetAddr -> Suffix -> Command) -> Parser Command
pMoveTransfer c ctor = char c *> space *> (ctor <$> pTargetAddr <*> pSuffix) <* space <* eof

pPrintCmd :: FullRange -> Parser Command
pPrintCmd range = do
  suf <- choice [PrintSuffix <$ char 'p', NumberSuffix <$ char 'n', ListSuffix <$ char 'l']
  space *> eof
  pure (PrintLines range suf)

pBareRange :: FullRange -> Parser Command
pBareRange range = eof $> PrintLines range PrintSuffix

pTextBody :: Parser (Text, Suffix)
pTextBody = do
  suf <- pSuffix
  rest <- takeWhileP Nothing (/= '\n')
  let stripped = T.stripStart rest
  if T.null stripped
    then optional (char '\n') *> takeRest >>= \remaining ->
           pure (maybe "" (T.intercalate "\n") (findDotTerminator (T.lines remaining)), suf)
    else pure (stripped, suf)

findDotTerminator :: [Text] -> Maybe [Text]
findDotTerminator = go []
  where
    go _ [] = Nothing
    go acc ("." : _) = Just (reverse acc)
    go acc (l : rest) = go (l : acc) rest

validDelim :: Char -> Bool
validDelim d = d /= '\\' && d /= ' ' && d /= '\n' && not (isAlphaNum d)

pDelimited :: Char -> Parser (Text, Bool)
pDelimited delim = do
  chunks <- many $ choice
    [ try (T.singleton '\\' <$ string "\\\\")
    , try (T.singleton '\n' <$ (char '\\' *> char '\n'))
    , T.singleton <$> satisfy (\c -> c /= delim && c /= '\n') ]
  closed <- isJust <$> optional (char delim)
  pure (T.concat chunks, closed)

pSubstitute :: FullRange -> Parser Command
pSubstitute range = do
  _ <- char 's'
  delim <- satisfy validDelim
  (pat, _) <- pDelimited delim
  (repl, closed) <- pDelimited delim
  flags <- pSubstFlags
  suf <- if closed then pSuffix else pure PrintSuffix
  space *> eof
  pure (Substitute range pat repl flags suf)

pSubstFlags :: Parser SubstFlags
pSubstFlags = go (SubstFlags False 0 False)
  where
    go f = choice
      [ char 'g' *> go (f { sfGlobal = True })
      , char 'i' *> go (f { sfInsensitive = True })
      , satisfy (\c -> isDigit c && c /= '0') >>= \d -> go (f { sfCount = fromEnum d - fromEnum '0' })
      , pure f ]

pGlobalWithCmd :: Char -> (Text -> Text -> Command) -> Parser Command
pGlobalWithCmd c ctor = do
  _ <- char c
  delim <- satisfy validDelim
  (pat, _) <- pDelimited delim
  cmdlist <- takeRest
  pure (ctor pat (if T.null cmdlist then "p" else cmdlist))

pGlobalNoCmd :: Char -> (Text -> Command) -> Parser Command
pGlobalNoCmd c ctor = do
  _ <- char c
  delim <- satisfy validDelim
  (pat, _) <- pDelimited delim
  pure (ctor pat)

toMaybePath :: Text -> Maybe FilePath
toMaybePath t = let s = T.strip t in if T.null s then Nothing else Just (toString s)

pFileOrShell :: (Maybe FilePath -> Command) -> (Text -> Command) -> Parser Command
pFileOrShell fileCtor shellCtor = choice
  [ shellCtor <$> (char '!' *> takeRest)
  , fileCtor . toMaybePath <$> takeRest ]

pFileCmd :: Char -> (Maybe FilePath -> Command) -> (Text -> Command) -> Bool -> Parser Command
pFileCmd c fileCtor shellCtor guardAlpha = do
  _ <- char c
  when guardAlpha $ notFollowedBy alphaNumChar
  space
  pFileOrShell fileCtor shellCtor

pFnCommand :: Parser Command
pFnCommand = do
  _ <- string "fn"
  choice [try pFnDelimited, pFnBrace]
  where
    pFnDelimited = do
      delim <- satisfy validDelim
      name <- takeWhile1P (Just "function name") (\c -> c /= delim && c /= '\n')
      mDelim2 <- optional (char delim)
      case mDelim2 of
        Nothing -> pure (FnQuery name)
        Just _ -> do
          body <- takeRest
          if T.null body
            then pure (FnQuery name)
            else pure (FnDefine name [] (normalizeFnBody body))

    pFnBrace = do
      rest <- T.strip <$> takeRest
      if T.null rest then pure FnList
      else let (beforeBrace, braceRest) = T.breakOn "{" rest
               ws = T.words beforeBrace
           in case ws of
                [] -> pure FnList
                (name:params) -> if T.null braceRest then pure (FnQuery name)
                                 else let body = T.strip $ stripBrace $ T.drop 1 braceRest
                                      in pure (FnDefine name params body)

    stripBrace t = let s = T.stripEnd t in if T.null s || T.last s /= '}' then s else T.init s

-- | Normalize function body: convert backslash-newline to newline
normalizeFnBody :: Text -> Text
normalizeFnBody = go mempty
  where
    go acc t = case T.uncons t of
      Nothing -> acc
      Just ('\\', r) -> case T.uncons r of
        Just ('\n', r') -> go (acc <> "\n") r'
        Just (c, r') -> go (acc <> "\\" <> T.singleton c) r'
        Nothing -> acc <> "\\"
      Just (c, r) -> go (acc <> T.singleton c) r

pUserFunction :: Set Text -> FullRange -> Parser Command
pUserFunction userFns range = do
  name <- takeWhile1P (Just "function name") (\c -> isAscii c && isAlpha c)
  let mkInvoke fn suf = do
        rest <- takeRest
        let args = T.words (T.strip rest)
        pure (InvokeFunction fn range args suf)
  if name `Set.member` userFns
    then do
      mDelim <- optional (satisfy validDelim)
      case mDelim of
        Just delim -> do
          (params, suf) <- pDelimitedParamsWithSuffix delim
          pure (InvokeFunction name range params suf)
        Nothing -> pSuffix >>= mkInvoke name
    else case matchWithSuffix userFns name of
      Just (fn, suf) -> mkInvoke fn suf
      Nothing -> fail "unknown function"

-- | Parse delimited parameters with suffix: /param1/param2/param3p
-- Last delimiter is optional, suffix (p/n/l) comes after last param or delimiter
pDelimitedParamsWithSuffix :: Char -> Parser ([Text], Suffix)
pDelimitedParamsWithSuffix delim = do
  params <- go []
  -- Check if last param ends with a suffix char
  let (params', suf) = extractSuffixFromParams params
  pure (params', suf)
  where
    go acc = do
      param <- takeWhileP Nothing (\c -> c /= delim && c /= '\n')
      let param' = T.strip param
      mNext <- optional (char delim)
      case mNext of
        Just _ -> go (if T.null param' then acc else acc ++ [param'])
        Nothing -> do
          space *> eof
          pure (if T.null param' then acc else acc ++ [param'])

    extractSuffixFromParams ps = case reverse ps of
      [] -> ([], NoSuffix)
      (lastP : restRev) ->
        let initPs = reverse restRev
        in if T.null lastP
           then (ps, NoSuffix)
           else case T.last lastP of
             'p' -> let rest = T.init lastP
                    in if T.null rest then (initPs, PrintSuffix) else (initPs ++ [rest], PrintSuffix)
             'n' -> let rest = T.init lastP
                    in if T.null rest then (initPs, NumberSuffix) else (initPs ++ [rest], NumberSuffix)
             'l' -> let rest = T.init lastP
                    in if T.null rest then (initPs, ListSuffix) else (initPs ++ [rest], ListSuffix)
             _ -> (ps, NoSuffix)

matchWithSuffix :: Set Text -> Text -> Maybe (Text, Suffix)
matchWithSuffix fns name
  | T.null name = Nothing
  | T.init name `Set.member` fns = case T.last name of
      'p' -> Just (T.init name, PrintSuffix)
      'n' -> Just (T.init name, NumberSuffix)
      'l' -> Just (T.init name, ListSuffix)
      _ -> Nothing
  | otherwise = Nothing

needsMoreInput :: [Text] -> ParseResult -> Bool
needsMoreInput _ Incomplete = True
needsMoreInput _ _ = False

feedLines :: Set Text -> [Text] -> ParseResult
feedLines userFns lns =
  let combined = T.intercalate "\n" lns
  in case runParse (pCommand userFns) combined of
       Left err -> if "unexpected end of input" `T.isInfixOf` toText (errorBundlePretty err)
                   then Incomplete else Failed (toText (errorBundlePretty err))
       Right cmd -> checkMultiline cmd lns

checkMultiline :: Command -> [Text] -> ParseResult
checkMultiline cmd lns = case cmd of
  Append _ "" _ -> Complete cmd
  Change _ "" _ -> Complete cmd
  Insert _ "" _ -> Complete cmd
  FnDefine n p b ->
    -- Check both braces (for brace syntax) and backslash continuation (for delimiter syntax)
    if countBraces b > 0 || endsWithBackslash b
      then Incomplete
      else Complete (FnDefine n p b)
  _ -> if endsWithBackslash (fromMaybe "" (viaNonEmpty last lns)) then Incomplete else Complete cmd

countBraces :: Text -> Int
countBraces = T.foldl' (\n c -> if c == '{' then n + 1 else if c == '}' then n - 1 else n) 0

endsWithBackslash :: Text -> Bool
endsWithBackslash t = odd (T.length (T.takeWhileEnd (== '\\') (T.stripEnd t)))

parseCommand :: Set Text -> Text -> ParseResult
parseCommand userFns input = feedLines userFns [input]

runParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runParse p = runParser p ""

hasDocRange :: Command -> Bool
hasDocRange = \case
  Quit -> False; QuitAlways -> False; Help -> False; HelpMode -> False
  TogglePrompt _ -> False; Comment -> False; FnList -> False; FnQuery _ -> False
  FnDefine {} -> False; ImportMacro _ -> False; ImportDir -> False; ShellCommand _ -> False
  _ -> True

getCommandRange :: Command -> Maybe FullRange
getCommandRange = \case
  Append r _ _ -> Just r; Change r _ _ -> Just r; Insert r _ _ -> Just r
  Delete r _ -> Just r; Join r _ -> Just r; Move r _ _ -> Just r; Transfer r _ _ -> Just r
  MarkLine r _ -> Just r; PrintLines r _ -> Just r; PrintLineNumber r -> Just r
  Substitute r _ _ _ _ -> Just r; Global r _ _ -> Just r; GlobalInteractive r _ -> Just r
  GlobalReverse r _ _ -> Just r; GlobalReverseInteractive r _ -> Just r
  Edit r _ -> Just r; EditAlways r _ -> Just r; EditShell r _ -> Just r; EditShellAlways r _ -> Just r
  Filename r _ -> Just r; ReadFile r _ -> Just r; ReadShell r _ -> Just r
  Write r _ -> Just r; WriteShell r _ -> Just r; WriteQuit r _ -> Just r; WriteQuitShell r _ -> Just r
  Undo r -> Just r; Redo r -> Just r; ShellFilter r _ -> Just r; InvokeFunction _ r _ _ -> Just r
  _ -> Nothing

setCommandRange :: FullRange -> Command -> Command
setCommandRange r = \case
  Append _ t s -> Append r t s; Change _ t s -> Change r t s; Insert _ t s -> Insert r t s
  Delete _ s -> Delete r s; Join _ s -> Join r s; Move _ t s -> Move r t s; Transfer _ t s -> Transfer r t s
  MarkLine _ c -> MarkLine r c; PrintLines _ s -> PrintLines r s; PrintLineNumber _ -> PrintLineNumber r
  Substitute _ p rep f s -> Substitute r p rep f s
  Global _ p cl -> Global r p cl; GlobalInteractive _ p -> GlobalInteractive r p
  GlobalReverse _ p cl -> GlobalReverse r p cl; GlobalReverseInteractive _ p -> GlobalReverseInteractive r p
  Edit _ fp -> Edit r fp; EditAlways _ fp -> EditAlways r fp
  EditShell _ c -> EditShell r c; EditShellAlways _ c -> EditShellAlways r c
  Filename _ fp -> Filename r fp; ReadFile _ fp -> ReadFile r fp; ReadShell _ c -> ReadShell r c
  Write _ fp -> Write r fp; WriteShell _ c -> WriteShell r c
  WriteQuit _ fp -> WriteQuit r fp; WriteQuitShell _ c -> WriteQuitShell r c
  Undo _ -> Undo r; Redo _ -> Redo r; ShellFilter _ c -> ShellFilter r c; InvokeFunction n _ a s -> InvokeFunction n r a s
  cmd -> cmd

isValidFunctionName :: Text -> Bool
isValidFunctionName name =
  T.length name >= 2 && T.all (\c -> isAscii c && isAlpha c) name && name /= "fn"

systemCommandNames :: Set.Set Text
systemCommandNames = Set.fromList
  ["a","c","d","e","E","f","fn","g","G","h","H","i","im","imd","j","k","l","m","n","p","q","Q","r","s","t","u","U","v","V","w","wq","P"]
