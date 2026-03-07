module LedViMouse
  ( MouseState(..)
  , emptyMouseState

  , getLineNumberAtRow

  , makeRangeText
  , updateInputWithRange
  , splitRangeAndCommand
  ) where

import qualified Data.Text as T
import qualified Data.Vector as V

import LedViState (DisplayZone(..), DisplayLine(..))
import LedViLine (InputState, inputText, inputFromText)


-- Stores the line number where the mouse button was pressed.
data MouseState = MouseState
  { msClickStart :: !(Maybe Int)  -- ^ Line number where click started (Nothing = no active drag)
  } deriving stock (Eq, Show)

emptyMouseState :: MouseState
emptyMouseState = MouseState Nothing


-- Returns Nothing if row is out of bounds or the display line has no line number.
-- Uses dlSourceLine first (works for wrapped continuation lines),
-- falls back to dlLineNum, then to extracting from formatted text.
getLineNumberAtRow :: Int -> DisplayZone -> Maybe Int
getLineNumberAtRow row dz =
  let lineIdx = dzScrollTop dz + row
  in if lineIdx >= 0 && lineIdx < V.length (dzLines dz)
       then let dl = dzLines dz V.! lineIdx
            in case dlSourceLine dl of
                 Just n -> Just n
                 Nothing -> case dlLineNum dl of
                   Just n -> Just n
                   Nothing -> extractLineNumFromText (dlText dl)
       else Nothing

-- Used for command output that embeds line numbers in the text (n command format).
extractLineNumFromText :: Text -> Maybe Int
extractLineNumFromText txt =
  let stripped = T.stripStart txt
      (numPart, rest) = T.span (\c -> c >= '0' && c <= '9') stripped
  in if T.null numPart
       then Nothing
       else case T.uncons rest of
              -- Accept tab or multiple spaces as separator
              Just ('\t', _) -> readMaybe (T.unpack numPart)
              Just (' ', remaining) | T.null remaining || T.head remaining == ' ' ->
                readMaybe (T.unpack numPart)
              _ -> Nothing


-- Single line: "5"
-- Range: "3,7" (always with smaller number first)
makeRangeText :: Int -> Int -> Text
makeRangeText start end
  | start == end = T.pack (show start)
  | start < end  = T.pack (show start) <> "," <> T.pack (show end)
  | otherwise    = T.pack (show end) <> "," <> T.pack (show start)

-- If input is empty or only whitespace, set it to the range.
-- If input has a command (non-range text), prepend the range to it.
updateInputWithRange :: IORef InputState -> Text -> IO ()
updateInputWithRange inputRef rangeText = do
  input <- readIORef inputRef
  let currentText = inputText input
      stripped = T.strip currentText
  if T.null stripped
    then
      -- Empty input - just set the range
      writeIORef inputRef (inputFromText rangeText)
    else
      -- Has content - check if it starts with a range (digits, comma, special chars)
      -- and replace/prepend appropriately
      let (_existingRange, command) = splitRangeAndCommand stripped
          newText = if T.null command
                      then rangeText  -- Only had a range, replace it
                      else rangeText <> command  -- Prepend to command
      in writeIORef inputRef (inputFromText newText)

-- Range characters: digits, comma, semicolon, dot, dollar, plus, minus, slash, question, quote, backslash
splitRangeAndCommand :: Text -> (Text, Text)
splitRangeAndCommand txt = T.span isRangeChar txt
  where
    isRangeChar c = c `elem` ("0123456789,;.$+-/?'\\" :: String)
