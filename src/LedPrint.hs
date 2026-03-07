module LedPrint
  ( printByteCount, IODirection(..), printSuffix, printRange
  , formatLine, listFormat
  , LineFormat(..), formatDisplayLine
  ) where

import qualified Data.Text as T

import LedParse (Suffix(..))
import qualified LedDocument
import LedInput (Led, outputLine)
import LedState (getDocument, getCurrentLine, setCurrentLine)
import LedCore (LedState(..))

getMultiDocPrint :: Led (Maybe Int)
getMultiDocPrint = gets ledMultiDocPrint

-- | Direction of I/O operation for byte count display
data IODirection = ReadFrom | WriteTo

-- | Print byte count with direction arrow and source/target
-- Format: "123 <- filename" for read, "123 -> filename" for write
printByteCount :: Int -> IODirection -> Text -> Led ()
printByteCount n dir source = gets ledSilent >>= bool printIt (pure ())
  where
    arrow = case dir of
      ReadFrom -> " <- "
      WriteTo  -> " -> "
    printIt = outputLine . toString $ show n <> arrow <> source

printSuffix :: Suffix -> Led ()
printSuffix NoSuffix = pure ()
printSuffix suf = do
    cur <- getCurrentLine
    when (cur > 0) $ printRange suf cur cur

printRange :: Suffix -> Int -> Int -> Led ()
printRange suf start end = do
    doc <- getDocument
    mDocNum <- getMultiDocPrint
    let lns = LedDocument.getLines start end doc
    forM_ (zip [start..end] lns) $ \(lineNum, line) ->
      outputLine . toString $ formatLine suf mDocNum lineNum line
    setCurrentLine end

formatLine :: Suffix -> Maybe Int -> Int -> Text -> Text
formatLine suf mDocNum lineNum line = case suf of
    NoSuffix    -> line
    PrintSuffix -> line
    NumberSuffix -> case mDocNum of
      Just docNum -> show docNum <> ":" <> show lineNum <> "\t" <> line
      Nothing     -> show lineNum <> "\t" <> line
    ListSuffix  -> listFormat line

listFormat :: Text -> Text
listFormat = (<> "$") . T.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '$'  = "\\$"
    escapeChar '\a' = "\\a"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar '\v' = "\\v"
    escapeChar c
      | c < ' ' || c == '\DEL' =
          let o = fromEnum c
              d2 = o `div` 64
              d1 = (o `mod` 64) `div` 8
              d0 = o `mod` 8
          in "\\" <> T.pack [chr (d2 + 48), chr (d1 + 48), chr (d0 + 48)]
      | otherwise = T.singleton c


-- | Unified line format for visual mode display.
-- Produces tab-separated output: "lineNum\tmarkerContent*"
data LineFormat = LineFormat
  { lfLineNum    :: !(Maybe Int)  -- ^ Line number (Nothing for headers/new lines)
  , lfDocNum     :: !(Maybe Int)  -- ^ Document number for multi-doc display
  , lfGutterMark :: !Text         -- ^ Marker in gutter area (e.g., ">" for current)
  , lfContent    :: !Text         -- ^ Line content
  , lfSuffixMark :: !Text         -- ^ Suffix after content (e.g., "*" for modified)
  } deriving stock (Eq, Show)


-- | Format a line for visual display with tab-separated gutter/content.
-- Output format: "lineNum\tmarkerContent*" or "docNum:lineNum\tmarkerContent*"
formatDisplayLine :: LineFormat -> Text
formatDisplayLine lf =
  let numPart = case (lfDocNum lf, lfLineNum lf) of
        (Just d, Just n) -> show d <> ":" <> show n
        (Nothing, Just n) -> show n
        _ -> ""
      gutterMark = lfGutterMark lf
      suffixMark = lfSuffixMark lf
  in numPart <> "\t" <> gutterMark <> lfContent lf <> suffixMark
