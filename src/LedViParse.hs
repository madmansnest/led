module LedViParse
  ( -- * Partial Parse Types
    PartialParse(..)
  , isParseError
  , isParseComplete

    -- * Parsing Functions
  , parsePartial
  , parsePartialWithFns
  ) where

import Data.Char (isAlphaNum)
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Char (isDigit)
import LedParse (DocRange(..), LineRange(..), Command(..), Parser, parseCommand, ParseResult(..), pDocRange, pLineRange, runParse, SubstFlags(..))

-- This allows us to provide real-time feedback as the user types,
-- even when the command is incomplete.
data PartialParse
  = PPEmpty
    -- ^ No input yet
  | PPAddress !DocRange !LineRange
    -- ^ Address parsed, waiting for command
  | PPCommand !Command
    -- ^ Complete command ready to execute
  | PPSubstPattern !DocRange !LineRange !Text
    -- ^ In @s/pattern/@ - have pattern, waiting for replacement
  | PPSubstReplace !DocRange !LineRange !Text !Text !SubstFlags
    -- ^ In @s/pattern/replace/flags@ - have pattern, replacement, and flags
  | PPGlobalPattern !DocRange !LineRange !Char !Text
    -- ^ In @g/pattern/@ - have pattern (and delimiter), waiting for command
  | PPGlobalCommand !DocRange !LineRange !Char !Text !Text
    -- ^ In @g/pattern/command@ - have pattern, delimiter, and command
  | PPAppendText !DocRange !LineRange ![Text]
    -- ^ Append command with text lines being entered
  | PPInsertText !DocRange !LineRange ![Text]
    -- ^ Insert command with text lines being entered
  | PPChangeText !DocRange !LineRange ![Text]
    -- ^ Change command with text lines being entered
  | PPCommandPrefix !DocRange !LineRange !Char
    -- ^ Command letter typed but not yet complete (e.g., "s" without delimiter)
    -- Used to show default range preview for the command
  | PPError !Text
    -- ^ Parse error with message
  | PPIncomplete
    -- ^ Need more input (different from PPEmpty - some input exists)
  deriving stock (Eq, Show)


isParseError :: PartialParse -> Bool
isParseError (PPError _) = True
isParseError _ = False


isParseComplete :: PartialParse -> Bool
isParseComplete (PPCommand _) = True
isParseComplete _ = False


-- Parsing Functions

-- This is the main entry point for visual mode parsing. It attempts
-- a full parse first, then falls back to extracting partial information.
parsePartial :: Text -> PartialParse
parsePartial = parsePartialWithFns Set.empty


parsePartialWithFns :: Set.Set Text -> Text -> PartialParse
parsePartialWithFns userFns input
  | T.null (T.strip input) = PPEmpty
  | otherwise = case parseCommand userFns input of
      Complete cmd -> PPCommand cmd
      -- For Incomplete or Failed, try partial parsers to get preview info
      -- This allows showing regex matches while typing s/pattern
      _ -> case tryPartialParsers userFns input of
        PPError _ -> PPIncomplete  -- If partial parsing also fails, show as incomplete
        pp -> pp


tryPartialParsers :: Set.Set Text -> Text -> PartialParse
tryPartialParsers _userFns =
  fromMaybe (PPError "Invalid command")
    . asum
    . sequenceA
        [ trySubstitutePartial
        , tryGlobalPartial
        , tryCommandPrefixPartial
        , tryAddressPartial
        ]


-- Partial Address Parser

tryAddressPartial :: Text -> Maybe PartialParse
tryAddressPartial input = case runParse pPartialAddress input of
  Right (docRange, lineRange, remaining) ->
    if T.null (T.strip remaining)
      then Just (PPAddress docRange lineRange)
      else Nothing  -- There's more after the address, let other parsers handle
  Left _ -> Nothing


pPartialAddress :: Parser (DocRange, LineRange, Text)
pPartialAddress = do
  space
  docRange <- pDocRange
  lineRange <- pLineRange
  remaining <- takeRest
  pure (docRange, lineRange, remaining)


-- Partial Command Prefix Parser

-- This handles cases like "s" (before delimiter) or "d" (incomplete delete).
tryCommandPrefixPartial :: Text -> Maybe PartialParse
tryCommandPrefixPartial input = case runParse pPartialCommandPrefix input of
  Right pp -> Just pp
  Left _ -> Nothing


-- Recognizes command letters that have a default range.
-- Only matches when the command letter is followed by nothing or whitespace.
pPartialCommandPrefix :: Parser PartialParse
pPartialCommandPrefix = do
  space
  docRange <- pDocRange
  lineRange <- pLineRange
  space
  cmdChar <- satisfy isCommandChar
  -- Only accept if this is the end (nothing after command letter)
  rest <- takeRest
  if T.null (T.strip rest)
    then pure (PPCommandPrefix docRange lineRange cmdChar)
    else fail "not a valid command prefix"
  where
    -- Command letters that can have ranges and benefit from preview
    -- Includes 's' and 'g' so that typing just the letter shows the default range
    -- (before delimiter is typed, the partial parsers for s/g don't match)
    isCommandChar c = c `elem` ("acdgijklmnpstuwxyvGV" :: String)


-- Partial Substitute Parser

trySubstitutePartial :: Text -> Maybe PartialParse
trySubstitutePartial input = case runParse pPartialSubstitute input of
  Right pp -> Just pp
  Left _ -> Nothing


pPartialSubstitute :: Parser PartialParse
pPartialSubstitute = do
  space
  docRange <- pDocRange
  lineRange <- pLineRange
  space
  _ <- char 's'
  delim <- satisfy validDelim
  (pat, patClosed) <- pDelimitedPartial delim
  if not patClosed
    then pure (PPSubstPattern docRange lineRange pat)
    else do
      (repl, replClosed) <- pDelimitedPartial delim
      if not replClosed
        then pure (PPSubstReplace docRange lineRange pat repl defaultSubstFlags)
        else do
          -- Have both pattern and replacement - parse any flags
          rest <- takeRest
          let flagStr = T.strip rest
          if T.null flagStr || T.all isValidFlagChar flagStr
            then pure (PPSubstReplace docRange lineRange pat repl (parseSubstFlags flagStr))
            else fail "extra text after substitute"


defaultSubstFlags :: SubstFlags
defaultSubstFlags = SubstFlags False 0 False


isValidFlagChar :: Char -> Bool
isValidFlagChar c = c `elem` ("gpnl123456789i" :: String)


parseSubstFlags :: Text -> SubstFlags
parseSubstFlags = T.foldl' addFlag defaultSubstFlags
  where
    addFlag f 'g' = f { sfGlobal = True }
    addFlag f 'i' = f { sfInsensitive = True }
    addFlag f c | isDigit c && c /= '0' = f { sfCount = fromEnum c - fromEnum '0' }
    addFlag f _ = f  -- Ignore p, n, l (suffix flags)


-- Partial Global Parser

tryGlobalPartial :: Text -> Maybe PartialParse
tryGlobalPartial input = case runParse pPartialGlobal input of
  Right pp -> Just pp
  Left _ -> Nothing


pPartialGlobal :: Parser PartialParse
pPartialGlobal = do
  space
  docRange <- pDocRange
  lineRange <- pLineRange
  space
  _ <- satisfy (`elem` ("gGvV" :: String))
  delim <- satisfy validDelim
  (pat, patClosed) <- pDelimitedPartial delim
  if not patClosed
    then pure (PPGlobalPattern docRange lineRange delim pat)
    else do
      cmdlist <- takeRest
      pure (PPGlobalCommand docRange lineRange delim pat cmdlist)


-- Helper Parsers

validDelim :: Char -> Bool
validDelim d = d /= '\\' && d /= ' ' && d /= '\n' && not (isAlphaNum d)


pDelimitedPartial :: Char -> Parser (Text, Bool)
pDelimitedPartial delim = do
  chunks <- many $ choice
    [ try (T.singleton '\\' <$ string "\\\\")
    , try (T.singleton delim <$ (char '\\' *> char delim))
    , try (T.singleton '\n' <$ (char '\\' *> char '\n'))
    , T.singleton <$> satisfy (\c -> c /= delim && c /= '\n')
    ]
  closed <- isJust <$> optional (char delim)
  pure (T.concat chunks, closed)
