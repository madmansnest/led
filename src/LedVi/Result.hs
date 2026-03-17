module LedVi.Result
  ( resultDisplayToZone
  , showDocumentNoHighlight
  , affectedRangeToZone
  , textLinesToDisplayZone
  , generateDisplayZoneForParse
  , getGlobalCommandInfo
  , indexText
  ) where

import qualified Data.Text as T
import qualified Data.Vector as V

import LedCore (LedState(..))
import LedDocument (documentLines, lineCount)
import LedNexus (DocumentList, dlCurrentDoc, getDocStateAt, DocumentState(..))
import LedParse (DocRange(..), LineRange(..), Command(..), FullRange(..))
import qualified LedUndo
import LedVi.Types
import LedVi.Parse (PartialParse(..))
import LedVi.Preview (generateDisplayForParse, showDocumentList, showEmptyDisplay)


resultDisplayToZone :: ResultDisplay -> LedState -> Int -> Int -> DisplayZone
resultDisplayToZone rd st width height = case rd of
  ResultText lns -> textLinesToDisplayZone lns width height
  ResultAffected ranges idx -> affectedRangeToZone ranges idx st width height
  ResultEmpty -> showDocumentNoHighlight (ledDocumentList st) width height
  ResultInitial -> showEmptyDisplay width height


-- Shows document without any highlighting, scrolled to current line.
-- Used for ResultEmpty (commands that don't change anything like empty append, undo).
showDocumentNoHighlight :: DocumentList -> Int -> Int -> DisplayZone
showDocumentNoHighlight dl width height =
  let curDoc = dlCurrentDoc dl
      mDocState = getDocStateAt curDoc dl
  in case mDocState of
    Nothing ->
      -- No document, show doc list
      showDocumentList dl width height
    Just docState ->
      let doc = docDocument docState
          curLine = docCurrentLine docState
          totalLines = lineCount doc
          allLines = documentLines doc
          -- All lines normal, no highlighting
          displayLines = V.fromList
            [ DisplayLine
              { dlLineNum = Just (i + 1)
              , dlSourceLine = Just (i + 1)
              , dlDocIdx = curDoc
              , dlText = indexText allLines i
              , dlStyle = StyleNormal
              , dlHighlights = []
              }
            | i <- [0 .. totalLines - 1]
            ]
          -- Scroll to show current line (where cursor is after undo)
          targetLine = curLine - 1
          scrollTop = scrollToShowRange targetLine targetLine totalLines height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzTotalLines = totalLines
        , dzWindowStart = 1
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (targetLine, targetLine)
        }


-- Shows the same view as during preview, but without selection highlighting
-- (deleted lines are already gone, added lines shown as normal).
affectedRangeToZone :: [LedUndo.AffectedRange] -> Int -> LedState -> Int -> Int -> DisplayZone
affectedRangeToZone ranges idx st width height
  | null ranges = emptyDisplayZone width height
  | idx < 0 || idx >= length ranges = emptyDisplayZone width height
  | otherwise =
      case drop idx ranges of
        [] -> emptyDisplayZone width height
        (LedUndo.AffectedRange docIdx rangeStart rangeEnd : _) ->
          let dl = ledDocumentList st
          in case getDocStateAt docIdx dl of
            Nothing -> emptyDisplayZone width height
            Just docState ->
              let doc = docDocument docState
                  totalLines = lineCount doc
                  allLines = documentLines doc
                  -- Create display lines for entire document - all StyleNormal
                  -- (no highlighting of affected lines after command completion)
                  displayLines = V.fromList
                    [ DisplayLine
                      { dlLineNum = Just i
                      , dlSourceLine = Just i
                      , dlDocIdx = docIdx
                      , dlText = indexText allLines (i - 1)
                      , dlStyle = StyleNormal
                      , dlHighlights = []
                      }
                    | i <- [1 .. totalLines]
                    ]
                  -- Use minimal scroll to show affected range (0-based indices)
                  targetStart = rangeStart - 1
                  targetEnd = rangeEnd - 1
                  scrollTop = scrollToShowRange targetStart targetEnd totalLines height
              in DisplayZone
                { dzLines = displayLines
                , dzScrollTop = scrollTop
                , dzTotalLines = totalLines
                , dzWindowStart = 1
                , dzHeight = height
                , dzWidth = width
                , dzTargetRange = Just (targetStart, targetEnd)  -- Set target for scroll preservation
                }


indexText :: [Text] -> Int -> Text
indexText xs i
  | i < 0 = ""
  | otherwise = fromMaybe "" (viaNonEmpty head (drop i xs))


generateDisplayZoneForParse :: PartialParse -> LedState -> Int -> Int -> DisplayZone
generateDisplayZoneForParse pp st width height =
  let dl = ledDocumentList st
      paramStack = ledParamStack st
      mPrevLineRange = ledLastLineRange st
  in generateDisplayForParse pp dl paramStack mPrevLineRange width height


-- Returns (invert, pattern, command, docRange, lineRange) for preview handling.
-- Returns Nothing for non-global commands or incomplete global patterns.
-- Takes input text to determine g vs v for partial commands.
getGlobalCommandInfo :: PartialParse -> Text -> Maybe (Bool, Text, Text, DocRange, LineRange)
getGlobalCommandInfo pp inputTxt = case pp of
  -- PPGlobalCommand is global with closed pattern delimiter
  -- Check input to determine if it's v (invert) or g (normal)
  PPGlobalCommand docRange lineRange _delim pat cmd ->
    let invert = hasVCommand inputTxt
    in Just (invert, pat, cmd, docRange, lineRange)
  -- Full commands
  PPCommand (Global (FullRange docRange lineRange) pat cmdlist) ->
    Just (False, pat, cmdlist, docRange, lineRange)
  PPCommand (GlobalReverse (FullRange docRange lineRange) pat cmdlist) ->
    Just (True, pat, cmdlist, docRange, lineRange)
  -- Everything else - not a global command or pattern not closed
  _ -> Nothing
  where
    -- Check if input contains v or V before the delimiter
    hasVCommand txt = case T.find (`elem` ("gGvV" :: String)) txt of
      Just c -> c == 'v' || c == 'V'
      Nothing -> False


textLinesToDisplayZone :: [Text] -> Int -> Int -> DisplayZone
textLinesToDisplayZone lns width height =
  let displayLines = V.fromList
        [ DisplayLine
          { dlLineNum = Nothing
          , dlSourceLine = Nothing  -- Command output has no source line
          , dlDocIdx = 0
          , dlText = line
          , dlStyle = StyleNormal
          , dlHighlights = []
          }
        | line <- lns
        ]
      totalLines = V.length displayLines
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = 0
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Nothing  -- No specific target for text output
    }
