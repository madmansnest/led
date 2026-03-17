module LedVi.Preview
  ( -- * Display generation
    generateDisplayForParse
  , generateDisplayForParseAtView
  , showCurrentPosition
  , showAddressPreview
  , showAddressRange
  , showDocumentList
  , showDocumentListRange
  , showDocumentScrolledTo
  , showEntireDocument
  , showLineRange
  , showCommandRange
  , showEmptyDisplay
  , windowedRenderThreshold

    -- * Edit preview
  , EditMode(..)
  , showEditPreview

    -- * Global preview support
  , computeGlobalMatches
  , showGlobalMatchResults
  , showGlobalCommandResultsWithMatches
  , showGlobalNoMatches
  , showResolvedRangeWithDefault
  , defaultRangeAll
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import LedDocument (Document, documentLines, lineCount, getLines, getLineAt)
import LedGlobal (findMatchingLineNumbers)
import LedNexus (DocumentList, DocumentState(..), BufferChangeFlag(..), dlCurrentDoc, getDocStateAt, documentCount, dlDocListState, unsavedDocuments)
import LedParse (DocRange(..), LineRange(..), Addr(..), Command(..), FullRange(..), Suffix(..), TargetAddr(..), SubstFlags(..), getCommandRange)
import LedPrint (LineFormat(..), formatDisplayLine, listFormat)
import LedSmartReplace (isSmartReplaceEligible, smartReplace, isAllLower)
import qualified LedRegularExpressions as RE
import LedResolve (resolveDocRange, resolveLineRange, resolveAddr)
import LedVi.Types hiding (mkDisplayZone)
import LedVi.Parse (PartialParse(..))


withDocState :: Int -> DocumentList -> Int -> Int -> (DocumentState -> DisplayZone) -> DisplayZone
withDocState docIdx dl width height f =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just ds -> f ds

docInfo :: DocumentState -> (Int, Int, [Text], Map.Map Char Int)
docInfo ds = (docCurrentLine ds, lineCount (docDocument ds), documentLines (docDocument ds), docMarks ds)

substitutePrevLineRange :: Maybe LineRange -> LineRange -> LineRange
substitutePrevLineRange mPrev LinePrevious = fromMaybe LinePrevious mPrev
substitutePrevLineRange _ lr = lr

resolveRange :: Int -> Int -> Map.Map Char Int -> [Text] -> LineRange -> (Int -> Int -> (Int, Int)) -> (Int, Int)
resolveRange curLine total marks allLines lineRange defaultFn =
  case lineRange of
    LineDefault -> defaultFn curLine total
    -- For incomplete ranges (e.g., "5," where end is current but invalid),
    -- try to show at least the start address as a preview
    LineFree from _ -> case resolveLineRange curLine total marks allLines lineRange of
      Right r -> r
      Left _ -> case resolveAddr curLine total marks allLines from of
        Right n | n >= 1 && n <= total -> (n, n)  -- Show start address as fallback
        _ -> defaultFn curLine total
    LineBound from _ -> case resolveLineRange curLine total marks allLines lineRange of
      Right r -> r
      Left _ -> case resolveAddr curLine total marks allLines from of
        Right n | n >= 1 && n <= total -> (n, n)  -- Show start address as fallback
        _ -> defaultFn curLine total
    _ -> case resolveLineRange curLine total marks allLines lineRange of
      Left _  -> defaultFn curLine total
      Right r -> r

resolveRangeAllowZero :: Bool -> Int -> Int -> Map.Map Char Int -> [Text] -> LineRange -> (Int -> Int -> (Int, Int)) -> (Int, Int)
resolveRangeAllowZero allowZero curLine total marks allLines lineRange defaultFn =
  case lineRange of
    LineDefault -> defaultFn curLine total
    LineSingle addr -> case resolveAddr curLine total marks allLines addr of
      Left _ -> defaultFn curLine total
      Right n
        | n < 0 -> defaultFn curLine total
        | n == 0 && not allowZero -> defaultFn curLine total
        | otherwise -> (n, n)
    _ -> case resolveLineRange curLine total marks allLines lineRange of
      Left _  -> defaultFn curLine total
      Right r -> r

mkDisplayZone :: [DisplayLine] -> Int -> Int -> Int -> Int -> Maybe (Int, Int) -> DisplayZone
mkDisplayZone lines' scrollTop totalLines width height targetRange = DisplayZone
  { dzLines = V.fromList lines'
  , dzScrollTop = scrollTop
  , dzTotalLines = totalLines
  , dzWindowStart = 1  -- Non-windowed: window starts at line 1
  , dzHeight = height
  , dzWidth = width
  , dzTargetRange = targetRange
  }

mkWindowedDisplayZone :: [DisplayLine] -> Int -> Int -> Int -> Int -> Int -> Maybe (Int, Int) -> DisplayZone
mkWindowedDisplayZone lines' scrollTop totalLines windowStart width height targetRange = DisplayZone
  { dzLines = V.fromList lines'
  , dzScrollTop = scrollTop
  , dzTotalLines = totalLines
  , dzWindowStart = windowStart
  , dzHeight = height
  , dzWidth = width
  , dzTargetRange = targetRange
  }


-- For documents larger than this, we only render visible lines + buffer
windowedRenderThreshold :: Int
windowedRenderThreshold = 1000

-- Larger buffer = more scrolling freedom before hitting edge
windowBuffer :: Int
windowBuffer = 5


-- Only creates DisplayLine objects for visible window + buffer.
buildWindowedDocument
  :: Int           -- ^ Document index
  -> Int           -- ^ Current line (1-based)
  -> Int           -- ^ Total lines
  -> Int           -- ^ Target line to show (1-based)
  -> Document  -- ^ Source document
  -> Int           -- ^ View width
  -> Int           -- ^ View height
  -> DisplayZone
buildWindowedDocument docIdx curLine total targetLine doc width height =
  let -- Calculate scroll position to show target
      scrollTop = centerOnLine (targetLine - 1) total height
      -- Calculate window bounds with buffer
      bufferLines = height * windowBuffer
      windowStart = max 1 (scrollTop - bufferLines + 1)  -- 1-based
      windowEnd = min total (scrollTop + height + bufferLines)
      -- Get only the lines we need
      windowLines = getLines windowStart windowEnd doc
      -- Build display lines for visible window only
      displayLines =
        [ makeDisplayLine docIdx lineNum (indexTextSafe windowLines (lineNum - windowStart))
            (if lineNum == curLine then StyleSelected else StyleNormal)
        | lineNum <- [windowStart .. windowEnd]
        ]
      -- Adjust scroll position relative to window
      relativeScroll = scrollTop - (windowStart - 1)
      targetIdx = targetLine - 1  -- 0-based for target range
  in mkWindowedDisplayZone displayLines relativeScroll total windowStart width height
       (Just (targetIdx - (windowStart - 1), targetIdx - (windowStart - 1)))
  where
    indexTextSafe xs idx
      | idx < 0 = ""
      | otherwise = fromMaybe "" (viaNonEmpty head (drop idx xs))


buildWindowedDocumentWithHighlight
  :: Int           -- ^ Document index
  -> Int           -- ^ Total lines
  -> Int           -- ^ Highlight start (1-based)
  -> Int           -- ^ Highlight end (1-based)
  -> Document  -- ^ Source document
  -> Int           -- ^ View width
  -> Int           -- ^ View height
  -> DisplayZone
buildWindowedDocumentWithHighlight docIdx total hlStart hlEnd doc width height =
  let -- Calculate scroll position to show highlight range
      targetStart = hlStart - 1  -- 0-based
      targetEnd = hlEnd - 1
      scrollTop = scrollToShowRange targetStart targetEnd total height
      -- Calculate window bounds with buffer
      bufferLines = height * windowBuffer
      windowStart = max 1 (scrollTop - bufferLines + 1)  -- 1-based
      windowEnd = min total (scrollTop + height + bufferLines)
      -- Get only the lines we need
      windowLines = getLines windowStart windowEnd doc
      -- Build display lines for visible window only
      displayLines =
        [ makeDisplayLine docIdx lineNum (indexTextSafe windowLines (lineNum - windowStart))
            (if lineNum >= hlStart && lineNum <= hlEnd then StyleSelected else StyleNormal)
        | lineNum <- [windowStart .. windowEnd]
        ]
      -- Adjust scroll position and target range relative to window
      relativeScroll = scrollTop - (windowStart - 1)
      relTargetStart = targetStart - (windowStart - 1)
      relTargetEnd = targetEnd - (windowStart - 1)
  in mkWindowedDisplayZone displayLines relativeScroll total windowStart width height
       (Just (relTargetStart, relTargetEnd))
  where
    indexTextSafe xs idx
      | idx < 0 = ""
      | otherwise = fromMaybe "" (viaNonEmpty head (drop idx xs))


data EditMode
  = EditDelete                    -- ^ Delete: show lines as deleted
  | EditAppend ![Text]            -- ^ Append: insert new lines after target
  | EditInsert ![Text]            -- ^ Insert: insert new lines before target
  | EditChange ![Text]            -- ^ Change: show deleted + insert new lines
  | EditMove !Int ![Text]         -- ^ Move: delete source, insert at target position
  | EditTransfer !Int ![Text]     -- ^ Transfer: copy lines to target position
  | EditJoin                      -- ^ Join: show lines to be joined, preview result
  deriving stock (Eq, Show)

showEditPreview :: EditMode -> DocRange -> LineRange -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showEditPreview mode docRange lineRange dl paramStack width height =
  case docRange of
    DocDefault ->
      showSingleDocEditPreview mode (dlCurrentDoc dl) lineRange dl width height

    DocSingle addr ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> showSingleDocEditPreview mode docIdx lineRange dl width height

    DocFree addrFrom addrTo ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocFree addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          showMultiDocEditPreview mode docStart docEnd lineRange dl width height

    DocBound addrFrom addrTo ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocBound addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          showMultiDocEditPreview mode docStart docEnd lineRange dl width height

    DocAll ->
      showDocListEditPreview mode lineRange dl width height

    DocManage ->
      showDocListEditPreview mode lineRange dl width height

    DocModified ->
      showDocListEditPreview mode lineRange dl width height

    DocParam ->
      showParamEditPreview mode lineRange paramStack width height

    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height

showDocListEditPreview :: EditMode -> LineRange -> DocumentList -> Int -> Int -> DisplayZone
showDocListEditPreview mode lineRange dl width height =
  let docState = dlDocListState dl
      (curLine, total, allLines, marks) = docInfo docState
      allowZero = case mode of
        EditAppend {} -> True
        EditInsert {} -> True
        _ -> False
      (rangeStart, rangeEnd) = resolveRangeAllowZero allowZero curLine total marks allLines lineRange defaultRangeCurrent
      (displayLines, targetStart, targetEnd) = buildEditDisplay mode 0 rangeStart rangeEnd allLines
  in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd (length displayLines) height) (length displayLines) width height (Just (targetStart, targetEnd))

showSingleDocEditPreview :: EditMode -> Int -> LineRange -> DocumentList -> Int -> Int -> DisplayZone
showSingleDocEditPreview mode docIdx lineRange dl width height =
  withDocState docIdx dl width height $ \docState ->
    let (curLine, total, allLines, marks) = docInfo docState
        allowZero = case mode of
          EditAppend {} -> True
          EditInsert {} -> True
          _ -> False
        (rangeStart, rangeEnd) = resolveRangeAllowZero allowZero curLine total marks allLines lineRange defaultRangeCurrent
        (displayLines, targetStart, targetEnd) = buildEditDisplay mode docIdx rangeStart rangeEnd allLines
    in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd (length displayLines) height) (length displayLines) width height (Just (targetStart, targetEnd))

showParamEditPreview :: EditMode -> LineRange -> ParamStack -> Int -> Int -> DisplayZone
showParamEditPreview mode lineRange paramStack width height =
  case paramStack of
    [] -> emptyDisplayZone width height
    ((_, docState):_) ->
      let (curLine, total, allLines, marks) = docInfo docState
          allowZero = case mode of
            EditAppend {} -> True
            EditInsert {} -> True
            _ -> False
          (rangeStart, rangeEnd) = resolveRangeAllowZero allowZero curLine total marks allLines lineRange defaultRangeCurrent
          (displayLines, targetStart, targetEnd) = buildEditDisplay mode (-1) rangeStart rangeEnd allLines
      in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd (length displayLines) height) (length displayLines) width height (Just (targetStart, targetEnd))


-- Shows affected lines from each document, then new content once at the end.
showMultiDocEditPreview :: EditMode -> Int -> Int -> LineRange -> DocumentList -> Int -> Int -> DisplayZone
showMultiDocEditPreview mode docStart docEnd lineRange dl width height =
  let -- Get affected existing lines from each document (no new content yet)
      existingLines = concatMap (docExistingLines mode lineRange) [docStart..docEnd]
      -- Add new content once at the end
      newContentLines = case mode of
        EditAppend newLines -> map makeNewContentLine newLines
        EditInsert newLines -> map makeNewContentLine newLines
        EditChange newLines -> map makeNewContentLine newLines
        EditJoin -> []  -- Join result shown per-doc since it differs
        EditMove _ _ -> []  -- Move lines differ per source
        EditTransfer _ _ -> []  -- Transfer lines differ per source
        EditDelete -> []
      allDisplayLines = existingLines ++ newContentLines
      displayLines = V.fromList allDisplayLines
      scrollTop = 0
      totalLines = V.length displayLines
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Nothing
    }
  where
    -- Get only the affected existing lines from a document (no new content)
    docExistingLines :: EditMode -> LineRange -> Int -> [DisplayLine]
    docExistingLines editMode lRange idx =
      case getDocStateAt idx dl of
        Nothing -> []
        Just docState ->
          let doc = docDocument docState
              allLines = documentLines doc
              total = lineCount doc
              curLine = docCurrentLine docState
              marks = docMarks docState
              -- For append/insert, allow address 0
              allowZero = case editMode of
                EditAppend {} -> True
                EditInsert {} -> True
                _ -> False
              -- Resolve line range for this document
              (rangeStart, rangeEnd) = resolveRangeAllowZero allowZero curLine total marks allLines lRange defaultRangeCurrent
          in case editMode of
            EditDelete ->
              -- Show lines being deleted
              [ toMultiDocLine idx i (indexText allLines (i - 1)) StyleDeleted
              | i <- [rangeStart .. rangeEnd], i > 0, i <= total ]
            EditAppend _ ->
              -- Show target line (line after which we append)
              [ toMultiDocLine idx rangeEnd (indexText allLines (rangeEnd - 1)) StyleSelected
              | rangeEnd > 0, rangeEnd <= total ]
            EditInsert _ ->
              -- Show target line (line before which we insert)
              [ toMultiDocLine idx rangeStart (indexText allLines (rangeStart - 1)) StyleSelected
              | rangeStart > 0, rangeStart <= total ]
            EditChange _ ->
              -- Show lines being deleted/changed
              [ toMultiDocLine idx i (indexText allLines (i - 1)) StyleDeleted
              | i <- [rangeStart .. rangeEnd], i > 0, i <= total ]
            EditJoin ->
              -- Show lines being joined as deleted, then joined result
              let deletedLines = [ toMultiDocLine idx i (indexText allLines (i - 1)) StyleDeleted
                                 | i <- [rangeStart .. rangeEnd], i > 0, i <= total ]
                  joinedText = T.concat [indexText allLines (i - 1) | i <- [rangeStart .. rangeEnd], i > 0, i <= total]
                  joinedLine = toMultiDocLine idx rangeStart joinedText StyleSelected
              in deletedLines ++ [joinedLine]
            EditMove _ srcLines ->
              -- Show source as deleted + moved content
              let deletedLines = [ toMultiDocLine idx i (indexText allLines (i - 1)) StyleDeleted
                                 | i <- [rangeStart .. rangeEnd], i > 0, i <= total ]
                  movedLines = [ makeNewContentLine txt | txt <- srcLines ]
              in deletedLines ++ movedLines
            EditTransfer _ srcLines ->
              -- Show transferred content
              [ makeNewContentLine txt | txt <- srcLines ]

    -- Convert to doc:line gutter format
    toMultiDocLine :: Int -> Int -> Text -> LineStyle -> DisplayLine
    toMultiDocLine docIdx lineNum text style =
      let formatted = formatDisplayLine LineFormat
            { lfLineNum = Just lineNum
            , lfDocNum = Just docIdx
            , lfGutterMark = ""
            , lfContent = text
            , lfSuffixMark = ""
            }
      in DisplayLine
        { dlLineNum = Nothing
        , dlSourceLine = Just lineNum
        , dlDocIdx = docIdx
        , dlText = formatted
        , dlStyle = style
        , dlHighlights = []
        }

    -- New content line (no doc/line number, just the content)
    makeNewContentLine :: Text -> DisplayLine
    makeNewContentLine text = DisplayLine
      { dlLineNum = Nothing
      , dlSourceLine = Nothing
      , dlDocIdx = 0
      , dlText = text
      , dlStyle = StyleSelected
      , dlHighlights = []
      }


buildEditDisplay :: EditMode -> Int -> Int -> Int -> [Text] -> ([DisplayLine], Int, Int)
buildEditDisplay mode docIdx rangeStart rangeEnd allLines =
  let totalLines = length allLines
      -- Helper to build a normal line
      mkLine i style = DisplayLine
        { dlLineNum = Just i
        , dlSourceLine = Just i
        , dlDocIdx = docIdx
        , dlText = indexText allLines (i - 1)
        , dlStyle = style
        , dlHighlights = []
        }
      -- Helper to build new lines (no line number)
      mkNewLines txts = [ DisplayLine
        { dlLineNum = Nothing
        , dlSourceLine = Nothing
        , dlDocIdx = docIdx
        , dlText = txt
        , dlStyle = StyleSelected
        , dlHighlights = []
        } | txt <- txts ]

  in case mode of
       EditDelete ->
         let dispLines = [mkLine i (if i >= rangeStart && i <= rangeEnd then StyleDeleted else StyleNormal)
                         | i <- [1..totalLines]]
         in (dispLines, rangeStart - 1, rangeEnd - 1)

       -- Append: insert new lines after range end
       EditAppend newLines ->
         let linesBefore = [mkLine i StyleNormal | i <- [1..rangeEnd]]
             linesAfter = [mkLine i StyleNormal | i <- [rangeEnd + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines newLines ++ linesAfter
             tgtStart = if null newLines then max 0 (rangeEnd - 1) else rangeEnd
             tgtEnd = if null newLines then tgtStart else rangeEnd + length newLines - 1
         in (allDisplayLines, tgtStart, tgtEnd)

       EditInsert newLines ->
         let insertIdx = rangeStart - 1  -- 0-based position
             linesBefore = [mkLine i StyleNormal | i <- [1..insertIdx]]
             linesAfter = [mkLine i StyleNormal | i <- [insertIdx + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines newLines ++ linesAfter
             tgtStart = if null newLines then max 0 insertIdx else insertIdx
             tgtEnd = if null newLines then tgtStart else insertIdx + length newLines - 1
         in (allDisplayLines, tgtStart, tgtEnd)

       EditChange newLines ->
         let linesBefore = [mkLine i StyleNormal | i <- [1..rangeStart - 1]]
             linesDeleted = [mkLine i StyleDeleted | i <- [rangeStart..rangeEnd]]
             linesAfter = [mkLine i StyleNormal | i <- [rangeEnd + 1..totalLines]]
             numDeleted = rangeEnd - rangeStart + 1
             allDisplayLines = linesBefore ++ linesDeleted ++ mkNewLines newLines ++ linesAfter
             tgtStart = rangeStart - 1
             tgtEnd = tgtStart + numDeleted + length newLines - 1
         in (allDisplayLines, tgtStart, tgtEnd)

       EditMove targetLine srcLines ->
         let mkSrcLine i = mkLine i (if i >= rangeStart && i <= rangeEnd then StyleDeleted else StyleNormal)
             linesBefore = [mkSrcLine i | i <- [1..targetLine]]
             linesAfter = [mkSrcLine i | i <- [targetLine + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines srcLines ++ linesAfter
         in (allDisplayLines, rangeStart - 1, rangeEnd - 1)

       EditTransfer targetLine srcLines ->
         let linesBefore = [mkLine i StyleNormal | i <- [1..targetLine]]
             linesAfter = [mkLine i StyleNormal | i <- [targetLine + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines srcLines ++ linesAfter
         in (allDisplayLines, rangeStart - 1, rangeEnd - 1)

       EditJoin ->
         let linesBefore = [mkLine i StyleNormal | i <- [1..rangeStart - 1]]
             linesDeleted = [mkLine i StyleDeleted | i <- [rangeStart..rangeEnd]]
             linesAfter = [mkLine i StyleNormal | i <- [rangeEnd + 1..totalLines]]
             -- Preview the joined line
             joinedText = T.concat [indexText allLines (i - 1) | i <- [rangeStart..rangeEnd]]
             joinedLine = DisplayLine
               { dlLineNum = Nothing
               , dlSourceLine = Nothing
               , dlDocIdx = docIdx
               , dlText = joinedText
               , dlStyle = StyleSelected
               , dlHighlights = []
               }
             allDisplayLines = linesBefore ++ linesDeleted ++ [joinedLine] ++ linesAfter
             numDeleted = rangeEnd - rangeStart + 1
             tgtStart = rangeStart - 1
             tgtEnd = tgtStart + numDeleted  -- Include the joined preview line
         in (allDisplayLines, tgtStart, tgtEnd)


-- Display Generation Based on Parse State

-- Type alias for param stack for clarity
type ParamStack = [(Text, DocumentState)]

-- Display behavior:
-- - On entry (PPEmpty): Show current document with current line highlighted
-- - When doc range typed (no line range): Show document list for that range
-- - When specific lines typed: Show entire document scrolled to those lines
generateDisplayForParse :: PartialParse -> DocumentList -> ParamStack -> Maybe LineRange -> Int -> Int -> DisplayZone
generateDisplayForParse pp dl paramStack mPrevLineRange width height =
  -- Substitute LinePrevious with actual previous range where applicable
  let subst = substitutePrevLineRange mPrevLineRange
  in case pp of
  PPEmpty ->
    -- No input - show current document with current line highlighted
    showCurrentPosition dl width height

  PPAddress docRange lineRange ->
    -- Address being typed - show documents or document with highlighted lines
    showAddressPreview docRange (subst lineRange) dl paramStack width height

  PPCommand cmd ->
    -- Complete command - show the range that will be affected
    showCommandRange cmd mPrevLineRange dl paramStack width height

  PPSubstPattern docRange lineRange pat ->
    -- Typing substitute pattern - show matches with strikethrough
    showSubstitutePatternPreview docRange (subst lineRange) pat dl paramStack width height

  PPSubstReplace docRange lineRange pat repl flags ->
    -- Have pattern and replacement - show matches with strikethrough and replacement in yellow
    showSubstituteReplacePreview docRange (subst lineRange) pat repl flags dl paramStack width height

  PPGlobalPattern docRange lineRange _delim _pat ->
    -- Pattern not yet closed - show range only (deferred search)
    -- Matching happens in PPGlobalCommand after closing delimiter is typed
    showResolvedRangeWithDefault docRange (subst lineRange) defaultRangeAll dl paramStack width height

  PPGlobalCommand docRange lineRange _delim pat cmd ->
    -- Have global pattern and command - show command-specific preview
    -- Note: PPGlobalCommand comes from partial parsing of g/ or v/
    -- For now, assume g/ (show matching). Full command parsing handles v/ correctly.
    showGlobalCommandPreview False docRange (subst lineRange) pat cmd dl paramStack width height

  PPAppendText docRange lineRange textLines ->
    -- Append command with text being typed - show preview
    showEditPreview (EditAppend textLines) docRange (subst lineRange) dl paramStack width height

  PPInsertText docRange lineRange textLines ->
    -- Insert command with text being typed - show preview
    showEditPreview (EditInsert textLines) docRange (subst lineRange) dl paramStack width height

  PPChangeText docRange lineRange textLines ->
    -- Change command with text being typed - show preview
    showEditPreview (EditChange textLines) docRange (subst lineRange) dl paramStack width height

  PPCommandPrefix docRange lineRange cmdChar ->
    -- Command letter typed but not complete - show default range for that command
    let defaultRange = commandPrefixDefaultRange cmdChar
    in showResolvedRangeWithDefault docRange (subst lineRange) defaultRange dl paramStack width height

  PPError _ ->
    -- Parse error - keep showing current position
    showCurrentPosition dl width height

  PPIncomplete ->
    -- Incomplete input - show current position
    showCurrentPosition dl width height


-- Used when user scrolls away from the target and we need to rebuild
-- the window around their current view position.
generateDisplayForParseAtView :: PartialParse -> DocumentList -> ParamStack -> Maybe LineRange -> Int -> Int -> Int -> DisplayZone
generateDisplayForParseAtView pp dl paramStack mPrevLineRange viewTopLine width height =
  -- For now, delegate to showDocumentAtViewPosition for the current document
  -- This is used when rebuilding after scroll reaches buffer edge
  case pp of
    PPEmpty -> showDocumentAtViewPosition (dlCurrentDoc dl) viewTopLine dl width height
    PPAddress DocDefault _ -> showDocumentAtViewPosition (dlCurrentDoc dl) viewTopLine dl width height
    PPCommand (SetLine (FullRange DocDefault _) NoSuffix) ->
      showDocumentAtViewPosition (dlCurrentDoc dl) viewTopLine dl width height
    -- For other cases, fall back to normal generation (centered on target)
    _ -> generateDisplayForParse pp dl paramStack mPrevLineRange width height


-- The target/current line is still highlighted, but window is built around viewTopLine.
showDocumentAtViewPosition :: Int -> Int -> DocumentList -> Int -> Int -> DisplayZone
showDocumentAtViewPosition docIdx viewTopLine dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let (curLine, total, _, _) = docInfo docState
          doc = docDocument docState
      in if total > windowedRenderThreshold
         then buildWindowedDocumentAtView docIdx curLine total viewTopLine doc width height
         else let allLines = documentLines doc
                  displayLines = [makeDisplayLine docIdx i (indexText allLines (i - 1))
                                   (if i == curLine then StyleSelected else StyleNormal)
                                 | i <- [1..total]]
                  -- Use viewTopLine as scroll position
                  scrollTop = max 0 (min (total - height) (viewTopLine - 1))
                  targetLine = curLine - 1
              in mkDisplayZone displayLines scrollTop total width height (Just (targetLine, targetLine))


buildWindowedDocumentAtView
  :: Int           -- ^ Document index
  -> Int           -- ^ Current line (1-based, for highlighting)
  -> Int           -- ^ Total lines
  -> Int           -- ^ View top line (1-based, center of window)
  -> Document
  -> Int           -- ^ View width
  -> Int           -- ^ View height
  -> DisplayZone
buildWindowedDocumentAtView docIdx curLine total viewTopLine doc width height =
  let -- Build window around viewTopLine
      bufferLines = height * windowBuffer
      windowStart = max 1 (viewTopLine - bufferLines)
      windowEnd = min total (viewTopLine + height + bufferLines - 1)
      -- Get only the lines we need
      windowLines = getLines windowStart windowEnd doc
      -- Build display lines
      displayLines =
        [ makeDisplayLine docIdx lineNum (indexTextSafe windowLines (lineNum - windowStart))
            (if lineNum == curLine then StyleSelected else StyleNormal)
        | lineNum <- [windowStart .. windowEnd]
        ]
      -- Scroll position relative to window
      relativeScroll = max 0 (viewTopLine - windowStart)
      -- Target is current line, relative to window
      targetIdx = curLine - windowStart
  in mkWindowedDisplayZone displayLines relativeScroll total windowStart width height
       (if curLine >= windowStart && curLine <= windowEnd
        then Just (targetIdx, targetIdx)
        else Nothing)  -- Target not in view
  where
    indexTextSafe xs idx
      | idx < 0 = ""
      | otherwise = fromMaybe "" (viaNonEmpty head (drop idx xs))


showCommandRange :: Command -> Maybe LineRange -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showCommandRange cmd mPrevLineRange dl paramStack width height =
  let subst = substitutePrevLineRange mPrevLineRange
  in case cmd of
    -- Move: show source lines as deleted, insert copies after target
    Move (FullRange docRange lineRange) target _ ->
      showMoveTransferCommand True docRange (subst lineRange) target dl paramStack width height

    -- Transfer (copy): show source lines normal, insert copies after target
    Transfer (FullRange docRange lineRange) target _ ->
      showMoveTransferCommand False docRange (subst lineRange) target dl paramStack width height

    -- Delete: show lines to be deleted as strikethrough
    Delete (FullRange docRange lineRange) _ ->
      showEditPreview EditDelete docRange (subst lineRange) dl paramStack width height

    -- Append: show insertion point (line will be added after)
    Append (FullRange docRange lineRange) txt _ ->
      showEditPreview (EditAppend (T.lines txt)) docRange (subst lineRange) dl paramStack width height

    -- Insert: show insertion point (line will be added before)
    Insert (FullRange docRange lineRange) txt _ ->
      showEditPreview (EditInsert (T.lines txt)) docRange (subst lineRange) dl paramStack width height

    -- Change: show lines to be replaced as deleted, new lines as added
    Change (FullRange docRange lineRange) txt _ ->
      showEditPreview (EditChange (T.lines txt)) docRange (subst lineRange) dl paramStack width height

    -- Join: show lines being joined as deleted, preview joined result
    Join (FullRange docRange lineRange) _ ->
      showEditPreview EditJoin docRange (subst lineRange) dl paramStack width height

    -- Substitute: show matches with strikethrough and replacements in yellow
    Substitute (FullRange docRange lineRange) pat repl flags _ ->
      showSubstituteReplacePreview docRange (subst lineRange) pat repl flags dl paramStack width height

    SetLine (FullRange docRange lineRange) suffix ->
      case suffix of
        NoSuffix -> showAddressPreview docRange (subst lineRange) dl paramStack width height
        _ -> showPrintedLines suffix docRange (subst lineRange) dl paramStack width height

    -- PrintLines: show printed output (just the lines, formatted by suffix)
    PrintLines (FullRange docRange lineRange) suffix ->
      showPrintedLines suffix docRange (subst lineRange) dl paramStack width height

    -- Undo/Redo: no preview change, keep showing current position
    Undo _ -> showCurrentPosition dl width height
    Redo _ -> showCurrentPosition dl width height

    -- Global: show command-specific preview for matching lines
    Global (FullRange docRange lineRange) pat cmdlist ->
      showGlobalCommandPreview False docRange (subst lineRange) pat cmdlist dl paramStack width height

    -- GlobalReverse: show command-specific preview for non-matching lines
    GlobalReverse (FullRange docRange lineRange) pat cmdlist ->
      showGlobalCommandPreview True docRange (subst lineRange) pat cmdlist dl paramStack width height

    -- Other commands: show affected range
    _ -> case getCommandRange cmd of
      Nothing ->
        -- Commands without ranges (Quit, Help, etc.) - show empty display
        emptyDisplayZone width height
      Just (FullRange docRange lineRange) ->
        -- Use command-specific default when LineDefault
        let defaultRange = commandDefaultRange cmd
        in showResolvedRangeWithDefault docRange (subst lineRange) defaultRange dl paramStack width height


-- Shows lines with regex matches highlighted in strikethrough.
showSubstitutePatternPreview :: DocRange -> LineRange -> Text -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showSubstitutePatternPreview docRange lineRange pat dl paramStack width height =
  let mBre = either (const Nothing) Just (RE.parseBRE pat)
      smartMode = isAllLower pat
      defaultFlags = SubstFlags False 0 False
      singleDocPreview docIdx docState =
        let (curLine, total, allLines, marks) = docInfo docState
            (startLine, endLine) = resolveRange curLine total marks allLines lineRange defaultRangeCurrent
            displayLines = buildSubstitutePreviewLines docIdx mBre Nothing defaultFlags smartMode allLines startLine endLine total
            targetStart = startLine - 1
            targetEnd = endLine - 1
        in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd (length displayLines) height) (length displayLines) width height (Just (targetStart, targetEnd))
  in case docRange of
    DocDefault ->
      withDocState (dlCurrentDoc dl) dl width height (singleDocPreview (dlCurrentDoc dl))

    DocSingle addr ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> withDocState docIdx dl width height (singleDocPreview docIdx)

    DocParam ->
      case paramStack of
        [] -> emptyDisplayZone width height
        ((_, docState):_) -> singleDocPreview (-1) docState

    DocFree addrFrom addrTo ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocFree addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          showMultiDocSubstPreview docStart docEnd lineRange pat "" defaultFlags dl width height

    DocBound addrFrom addrTo ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocBound addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          showMultiDocSubstPreview docStart docEnd lineRange pat "" defaultFlags dl width height

    DocAll ->
      singleDocPreview 0 (dlDocListState dl)

    DocManage ->
      singleDocPreview 0 (dlDocListState dl)

    DocModified ->
      singleDocPreview 0 (dlDocListState dl)

    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


showSubstituteReplacePreview :: DocRange -> LineRange -> Text -> Text -> SubstFlags -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showSubstituteReplacePreview docRange lineRange pat repl flags dl paramStack width height =
  let mBre = either (const Nothing) Just (RE.parseBRE pat)
      smartMode = isSmartReplaceEligible pat repl (sfInsensitive flags)
      singleDocPreview docIdx docState =
        let (curLine, total, allLines, marks) = docInfo docState
            (startLine, endLine) = resolveRange curLine total marks allLines lineRange defaultRangeCurrent
            displayLines = buildSubstitutePreviewLines docIdx mBre (Just repl) flags smartMode allLines startLine endLine total
            targetStart = startLine - 1
            targetEnd = endLine - 1
        in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd (length displayLines) height) (length displayLines) width height (Just (targetStart, targetEnd))
  in case docRange of
    DocDefault ->
      withDocState (dlCurrentDoc dl) dl width height (singleDocPreview (dlCurrentDoc dl))

    DocSingle addr ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> withDocState docIdx dl width height (singleDocPreview docIdx)

    DocParam ->
      case paramStack of
        [] -> emptyDisplayZone width height
        ((_, docState):_) -> singleDocPreview (-1) docState

    DocFree addrFrom addrTo ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocFree addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          showMultiDocSubstPreview docStart docEnd lineRange pat repl flags dl width height

    DocBound addrFrom addrTo ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocBound addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          showMultiDocSubstPreview docStart docEnd lineRange pat repl flags dl width height

    DocAll ->
      singleDocPreview 0 (dlDocListState dl)

    DocManage ->
      singleDocPreview 0 (dlDocListState dl)

    DocModified ->
      singleDocPreview 0 (dlDocListState dl)

    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


showMultiDocSubstPreview :: Int -> Int -> LineRange -> Text -> Text -> SubstFlags -> DocumentList -> Int -> Int -> DisplayZone
showMultiDocSubstPreview docStart docEnd lineRange pat repl flags dl width height =
  let mBre = either (const Nothing) Just (RE.parseBRE pat)
      smartMode = isSmartReplaceEligible pat repl (sfInsensitive flags)
      allDisplayLines = concatMap (docSubstLines mBre smartMode) [docStart..docEnd]
      displayLines = V.fromList allDisplayLines
      scrollTop = 0
      totalLines = V.length displayLines
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Nothing
    }
  where
    docSubstLines :: Maybe RE.BRE -> Bool -> Int -> [DisplayLine]
    docSubstLines mBre' smartMode' idx =
      case getDocStateAt idx dl of
        Nothing -> []
        Just docState ->
          let doc = docDocument docState
              allLines = documentLines doc
              total = lineCount doc
              curLine = docCurrentLine docState
              marks = docMarks docState
              -- Resolve line range for this document
              (lineStart, lineEnd) = case resolveLineRange curLine total marks allLines lineRange of
                Left _ -> defaultRangeCurrent curLine total
                Right r -> r
          in [ buildMultiDocSubstLine idx mBre' (Just repl) flags smartMode' i (indexText allLines (i - 1))
             | i <- [lineStart .. lineEnd]
             ]

buildMultiDocSubstLine :: Int -> Maybe RE.BRE -> Maybe Text -> SubstFlags -> Bool -> Int -> Text -> DisplayLine
buildMultiDocSubstLine docIdx mBre mRepl flags smartMode lineNum lineText =
  let baseLine = buildSubstLine docIdx mBre mRepl flags smartMode lineNum lineText True
      -- Reformat with doc:line gutter
      formatted = formatDisplayLine LineFormat
        { lfLineNum = Just lineNum
        , lfDocNum = Just docIdx
        , lfGutterMark = ""
        , lfContent = dlText baseLine
        , lfSuffixMark = ""
        }
  in baseLine { dlLineNum = Nothing, dlText = formatted }


showGlobalCommandPreview :: Bool -> DocRange -> LineRange -> Text -> Text -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showGlobalCommandPreview invert docRange lineRange pat cmdText dl paramStack width height =
  let matchSense = not invert
      singleDocPreview docIdx docState =
        let doc = docDocument docState
            (curLine, total, allLines, marks) = docInfo docState
            (startLine, endLine) = resolveRange curLine total marks allLines lineRange defaultRangeAll
        in case findMatchingLineNumbers matchSense pat startLine endLine doc of
          Left _ -> showResolvedRangeWithDefault docRange lineRange defaultRangeAll dl paramStack width height
          Right matchingLines ->
            if null matchingLines
              then showGlobalNoMatches docIdx width height
              else buildGlobalCommandDisplay docIdx matchingLines pat cmdText doc width height
  in case docRange of
    DocDefault ->
      withDocState (dlCurrentDoc dl) dl width height (singleDocPreview (dlCurrentDoc dl))

    DocSingle addr ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> withDocState docIdx dl width height (singleDocPreview docIdx)

    DocParam ->
      case paramStack of
        [] -> emptyDisplayZone width height
        ((_, docState):_) -> singleDocPreview (-1) docState

    DocAll ->
      singleDocPreview 0 (dlDocListState dl)

    DocManage ->
      singleDocPreview 0 (dlDocListState dl)

    DocModified ->
      singleDocPreview 0 (dlDocListState dl)

    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeAll dl paramStack width height


-- Takes the global pattern for use when substitute pattern is empty (s//).
buildGlobalCommandDisplay :: Int -> [Int] -> Text -> Text -> Document -> Int -> Int -> DisplayZone
buildGlobalCommandDisplay docIdx matchingLineNums globalPat cmdText doc width height =
  let cmd = T.strip cmdText
      -- Parse the first command to determine preview style
      displayLines = case parseGlobalSubCommand cmd of
        GCDelete ->
          -- Show matching lines as deleted (grey strikethrough)
          map (buildDeletedMatchLine docIdx doc) matchingLineNums
        GCSubstitute subPat subRepl flags ->
          -- Show substitution preview for each matching line
          -- If subPat is empty, use the global pattern
          let effectivePat = if T.null subPat then globalPat else subPat
          in map (buildSubstMatchLine docIdx doc effectivePat subRepl flags) matchingLineNums
        GCAppend textLines ->
          -- Show all matching lines, then new text once at the end
          let existingLines = map (buildMatchLine docIdx doc) matchingLineNums
              newLines = map (buildNewTextLine docIdx) textLines
          in existingLines ++ newLines
        GCInsert textLines ->
          -- Show new text once at the start, then all matching lines
          let existingLines = map (buildMatchLine docIdx doc) matchingLineNums
              newLines = map (buildNewTextLine docIdx) textLines
          in newLines ++ existingLines
        GCChange textLines ->
          -- Show all matching lines as deleted, then new text once at the end
          let deletedLines = map (buildDeletedMatchLine docIdx doc) matchingLineNums
              newLines = map (buildNewTextLine docIdx) textLines
          in deletedLines ++ newLines
        GCPrint ->
          -- Just show matching lines (default)
          map (buildMatchLine docIdx doc) matchingLineNums
        GCOther ->
          -- Unknown command - show matching lines highlighted
          map (buildMatchLine docIdx doc) matchingLineNums
      totalLines = length displayLines
  in mkDisplayZone displayLines 0 totalLines width height (Just (0, max 0 (totalLines - 1)))


data GlobalSubCommand
  = GCDelete                             -- ^ d command
  | GCSubstitute Text Text SubstFlags    -- ^ s/old/new/flags
  | GCAppend [Text]                      -- ^ a command with text lines
  | GCInsert [Text]                      -- ^ i command with text lines
  | GCChange [Text]                      -- ^ c command with text lines
  | GCPrint                              -- ^ p, n, l, or empty
  | GCOther                              -- ^ Other command


parseGlobalSubCommand :: Text -> GlobalSubCommand
parseGlobalSubCommand cmd
  | T.null cmd = GCPrint
  | cmd == "p" || cmd == "n" || cmd == "l" = GCPrint
  | cmd == "d" = GCDelete
  | "s" `T.isPrefixOf` cmd = parseSubstCommand cmd
  | "a" `T.isPrefixOf` cmd = parseBlockTextCommand GCAppend cmd
  | "i" `T.isPrefixOf` cmd = parseBlockTextCommand GCInsert cmd
  | "c" `T.isPrefixOf` cmd = parseBlockTextCommand GCChange cmd
  | otherwise = GCOther
  where
    parseSubstCommand t =
      -- Parse s/old/new/flags format
      case T.uncons (T.drop 1 t) of  -- Drop 's', get delimiter
        Nothing -> GCOther
        Just (delim, rest) ->
          let (pat, afterPat) = T.breakOn (T.singleton delim) rest
          in case T.uncons afterPat of
            Nothing -> GCOther  -- No closing delimiter for pattern
            Just (_, rest2) ->
              let (repl, afterRepl) = T.breakOn (T.singleton delim) rest2
                  flags = case T.uncons afterRepl of
                    Nothing -> defaultSubstFlags
                    Just (_, flagStr) -> parseSubstFlags flagStr
              in GCSubstitute pat repl flags

    defaultSubstFlags = SubstFlags False 1 False

    parseSubstFlags flagStr = foldr addFlag defaultSubstFlags (T.unpack flagStr)

    addFlag 'g' f = f { sfGlobal = True }
    addFlag 'i' f = f { sfInsensitive = True }
    addFlag c f | c >= '1' && c <= '9' = f { sfCount = fromEnum c - fromEnum '0' }
    addFlag _ f = f

    -- Parse block text command: a, i, or c followed by newline-separated text
    -- Text lines continue until "." on its own line
    parseBlockTextCommand :: ([Text] -> GlobalSubCommand) -> Text -> GlobalSubCommand
    parseBlockTextCommand ctor t =
      let afterCmd = T.dropWhile (`elem` ("pnl" :: String)) (T.drop 1 t)  -- Drop cmd char and optional suffix
          rawLines = T.lines afterCmd
          -- Filter out empty first line (from the newline after 'a') and "." terminator
          textLines = case rawLines of
            ("":rest) -> takeWhile (/= ".") rest
            _ -> takeWhile (/= ".") rawLines
      in ctor textLines


buildDeletedMatchLine :: Int -> Document -> Int -> DisplayLine
buildDeletedMatchLine docIdx doc lineNum =
  DisplayLine
    { dlLineNum = Just lineNum
    , dlSourceLine = Just lineNum
    , dlDocIdx = docIdx
    , dlText = getLineAt lineNum doc
    , dlStyle = StyleDeleted  -- Grey strikethrough
    , dlHighlights = []
    }


buildNewTextLine :: Int -> Text -> DisplayLine
buildNewTextLine docIdx txt =
  DisplayLine
    { dlLineNum = Nothing
    , dlSourceLine = Nothing
    , dlDocIdx = docIdx
    , dlText = txt
    , dlStyle = StyleSelected
    , dlHighlights = []
    }


-- Shows the line with inline highlights: match in strikethrough, replacement in yellow.
-- Reuses buildSubstLine for consistent s/ preview behavior.
buildSubstMatchLine :: Int -> Document -> Text -> Text -> SubstFlags -> Int -> DisplayLine
buildSubstMatchLine docIdx doc subPat subRepl flags lineNum =
  let lineText = getLineAt lineNum doc
      smartMode = isSmartReplaceEligible subPat subRepl (sfInsensitive flags)
      mBre = either (const Nothing) Just (RE.parseBRE subPat)
  in buildSubstLine docIdx mBre (Just subRepl) flags smartMode lineNum lineText True


showGlobalNoMatches :: Int -> Int -> Int -> DisplayZone
showGlobalNoMatches docIdx width height =
  let noMatchLine = DisplayLine
        { dlLineNum = Nothing
        , dlSourceLine = Nothing
        , dlDocIdx = docIdx
        , dlText = "[No matches]"
        , dlStyle = StyleNormal
        , dlHighlights = []
        }
  in mkDisplayZone [noMatchLine] 0 1 width height Nothing


-- Build display showing only matching lines (grep-style).
-- Uses Document directly for O(log n) line access via Rope.
buildGlobalMatchDisplay :: Int -> [Int] -> Document -> Int -> Int -> DisplayZone
buildGlobalMatchDisplay docIdx matchingLineNums doc width height =
  let displayLines = map (buildMatchLine docIdx doc) matchingLineNums
      matchCount = length matchingLineNums
  in mkDisplayZone displayLines 0 matchCount width height (Just (0, matchCount - 1))


-- Build a single display line for a matching line.
-- Uses getLineAt for O(log n) access instead of O(n) list indexing.
buildMatchLine :: Int -> Document -> Int -> DisplayLine
buildMatchLine docIdx doc lineNum =
  DisplayLine
    { dlLineNum = Just lineNum
    , dlSourceLine = Just lineNum
    , dlDocIdx = docIdx
    , dlText = getLineAt lineNum doc  -- O(log n) via Rope
    , dlStyle = StyleSelected  -- Highlight matching lines
    , dlHighlights = []
    }


-- ---------------------------------------------------------------------------
-- Async Global Preview Support
-- ---------------------------------------------------------------------------

-- This is the expensive operation that should run in background.
-- Returns list of matching line numbers.
-- Uses findMatchingLineNumbers from LedGlobal (same logic as g/re/n).
computeGlobalMatches :: Bool -> Text -> Int -> DocumentList -> IO [Int]
computeGlobalMatches invert pat docIdx dl =
  case getDocStateAt docIdx dl of
    Nothing -> pure []
    Just docState ->
      let doc = docDocument docState
          total = lineCount doc
          matchSense = not invert  -- g/ matches True, v/ matches False
      in case findMatchingLineNumbers matchSense pat 1 total doc of
        Left _ -> pure []
        Right matches -> pure matches


-- Used when async computation completes.
showGlobalMatchResults :: Int -> [Int] -> DocumentList -> Int -> Int -> DisplayZone
showGlobalMatchResults docIdx matchingLineNums dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let doc = docDocument docState
      in if null matchingLineNums
         then showGlobalNoMatches docIdx width height
         else buildGlobalMatchDisplay docIdx matchingLineNums doc width height


-- Used for live preview when the command is being typed.
-- The matches are cached (not recomputed when command changes).
-- Takes the global pattern for use when substitute pattern is empty (s//).
showGlobalCommandResultsWithMatches :: Int -> [Int] -> Text -> Text -> DocumentList -> Int -> Int -> DisplayZone
showGlobalCommandResultsWithMatches docIdx matchingLineNums globalPat cmdText dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let doc = docDocument docState
      in if null matchingLineNums
         then showGlobalNoMatches docIdx width height
         else buildGlobalCommandDisplay docIdx matchingLineNums globalPat cmdText doc width height


-- If mBre is Nothing (invalid regex), just show lines normally.
-- If mRepl is Nothing, only show matches. If mRepl is Just repl, also show replacements.
-- Respects flags (global, count) and smart mode (case-insensitive matching, case-preserving replacement).
buildSubstitutePreviewLines :: Int -> Maybe RE.BRE -> Maybe Text -> SubstFlags -> Bool -> [Text] -> Int -> Int -> Int -> [DisplayLine]
buildSubstitutePreviewLines docIdx mBre mRepl flags smartMode allLines startLine endLine totalLines =
  [ buildSubstLine docIdx mBre mRepl flags smartMode i (indexTextSafe allLines (i - 1)) (i >= startLine && i <= endLine)
  | i <- [1..totalLines]
  ]
  where
    indexTextSafe xs idx
      | idx < 0 = ""
      | otherwise = fromMaybe "" (viaNonEmpty head (drop idx xs))


-- Respects flags and smart mode for matching and replacement.
buildSubstLine :: Int -> Maybe RE.BRE -> Maybe Text -> SubstFlags -> Bool -> Int -> Text -> Bool -> DisplayLine
buildSubstLine docIdx mBre mRepl flags smartMode lineNum lineText isInRange =
  case (mBre, isInRange) of
    (Just bre, True) ->
      -- Find matches using appropriate matching function
      -- Note: smartMode controls case sensitivity (matches actual execution)
      -- The /i flag disables smart mode, resulting in case-sensitive matching
      let allMatches = if smartMode
                       then RE.matchAllBREInsensitive bre lineText
                       else RE.matchAllBRE bre lineText
          -- Filter matches based on flags: global, count, or first-only
          selectedMatches
            | sfGlobal flags = allMatches  -- All matches
            | sfCount flags > 0 = take 1 (drop (sfCount flags - 1) allMatches)  -- Nth match only
            | otherwise = take 1 allMatches  -- First match only
      in if null selectedMatches
         then DisplayLine
           { dlLineNum = Just lineNum
           , dlSourceLine = Just lineNum
           , dlDocIdx = docIdx
           , dlText = lineText
           , dlStyle = StyleNormal
           , dlHighlights = []
           }
         else case mRepl of
           Nothing ->
             -- Pattern only: show selected matches with strikethrough
             DisplayLine
               { dlLineNum = Just lineNum
               , dlSourceLine = Just lineNum
               , dlDocIdx = docIdx
               , dlText = lineText
               , dlStyle = StyleNormal
               , dlHighlights = [(RE.matchStart m, RE.matchEnd m, HLMatch) | m <- selectedMatches]
               }
           Just repl ->
             -- Pattern and replacement: build modified text with highlights
             -- Apply smart replace if in smart mode
             let (newText, highlights) = buildReplacementText lineText selectedMatches repl smartMode
             in DisplayLine
               { dlLineNum = Just lineNum
               , dlSourceLine = Just lineNum
               , dlDocIdx = docIdx
               , dlText = newText
               , dlStyle = StyleNormal
               , dlHighlights = highlights
               }
    _ ->
      -- Not in range or no valid regex
      DisplayLine
        { dlLineNum = Just lineNum
        , dlSourceLine = Just lineNum
        , dlDocIdx = docIdx
        , dlText = lineText
        , dlStyle = StyleNormal
        , dlHighlights = []
        }


-- Returns (new text, list of highlights).
-- Match text is shown with HLMatch (strikethrough), replacement with HLReplacement (yellow).
-- When smartMode is True, applies smart replace (case preservation).
buildReplacementText :: Text -> [RE.Match] -> Text -> Bool -> (Text, [(Int, Int, HighlightStyle)])
buildReplacementText original matches repl smartMode =
  -- Process matches from end to start to preserve positions
  -- Sort descending by position, then use foldl' to process from left (highest position first)
  let sortedMatches = sortOn (negate . RE.matchStart) matches
      (finalText, finalHighlights) = foldl' (flip processMatch) (original, []) sortedMatches
  in (finalText, finalHighlights)
  where
    processMatch :: RE.Match -> (Text, [(Int, Int, HighlightStyle)]) -> (Text, [(Int, Int, HighlightStyle)])
    processMatch m (txt, hls) =
      let mStart = RE.matchStart m
          mEnd = RE.matchEnd m
          matchLen = mEnd - mStart
          matchText = RE.matchText m
          -- Expand backreferences in replacement
          expandedRepl = expandBackrefs repl m
          -- Apply smart replace if in smart mode (case preservation)
          finalRepl = if smartMode then smartReplace matchText expandedRepl else expandedRepl
          replLen = T.length finalRepl
          -- New text: before + matchText + replacement + after
          -- We show BOTH the match (strikethrough) and replacement (yellow) inline
          before = T.take mStart txt
          after = T.drop mEnd txt
          newTxt = before <> matchText <> finalRepl <> after
          -- Calculate highlight positions in the new text
          matchHighlight = (mStart, mStart + matchLen, HLMatch)
          replHighlight = (mStart + matchLen, mStart + matchLen + replLen, HLReplacement)
          -- Adjust existing highlights that come after this position
          adjustedHls = map (adjustHighlight (mStart + matchLen) replLen) hls
      in (newTxt, matchHighlight : replHighlight : adjustedHls)

    adjustHighlight :: Int -> Int -> (Int, Int, HighlightStyle) -> (Int, Int, HighlightStyle)
    adjustHighlight insertPos insertLen (s, e, style)
      | s >= insertPos = (s + insertLen, e + insertLen, style)
      | otherwise = (s, e, style)


-- & or \0 = entire match, \1-\9 = groups
expandBackrefs :: Text -> RE.Match -> Text
expandBackrefs repl m = T.pack $ go (T.unpack repl)
  where
    matchedText = T.unpack (RE.matchText m)
    groups = RE.matchGroups m

    go :: String -> String
    go [] = []
    go ('\\':'\\':rest) = '\\' : go rest
    go ('\\':'&':rest) = '&' : go rest
    go ('\\':'n':rest) = '\n' : go rest
    go ('\\':c:rest)
      | c >= '0' && c <= '9' =
          let n = fromEnum c - fromEnum '0'
          in if n == 0
             then matchedText ++ go rest
             else case IM.lookup n groups of
               Just g -> T.unpack g ++ go rest
               Nothing -> go rest
      | otherwise = '\\' : c : go rest
    go ('&':rest) = matchedText ++ go rest
    go (c:rest) = c : go rest


-- For Move: source lines shown as deleted, copies shown as added after target.
-- For Transfer: source lines shown normally, copies shown as added after target.
showMoveTransferCommand :: Bool -> DocRange -> LineRange -> TargetAddr -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showMoveTransferCommand isMove docRange lineRange target dl paramStack width height =
  let singleDocPreview docIdx docState =
        let (curLine, total, allLines, marks) = docInfo docState
            (srcStart, srcEnd) = resolveRange curLine total marks allLines lineRange defaultRangeCurrent
            targetLine = fromMaybe 0 (resolveTargetAddr target docIdx curLine dl)
            srcLines = [indexText allLines (i - 1) | i <- [srcStart..srcEnd]]
            editMode = if isMove then EditMove targetLine srcLines else EditTransfer targetLine srcLines
            (displayLines, tgtStart, tgtEnd) = buildEditDisplay editMode docIdx srcStart srcEnd allLines
        in mkDisplayZone displayLines (scrollToShowRange tgtStart tgtEnd (length displayLines) height) (length displayLines) width height (Just (tgtStart, tgtEnd))
  in case docRange of
    DocDefault ->
      withDocState (dlCurrentDoc dl) dl width height (singleDocPreview (dlCurrentDoc dl))

    DocSingle addr ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> withDocState docIdx dl width height (singleDocPreview docIdx)

    DocParam ->
      case paramStack of
        [] -> emptyDisplayZone width height
        ((_, docState):_) -> singleDocPreview (-1) docState

    DocAll ->
      singleDocPreview 0 (dlDocListState dl)

    DocManage ->
      singleDocPreview 0 (dlDocListState dl)

    DocModified ->
      singleDocPreview 0 (dlDocListState dl)

    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


resolveTargetAddr :: TargetAddr -> Int -> Int -> DocumentList -> Maybe Int
resolveTargetAddr target curDoc curLine dl =
  let addr = case target of
        LocalTarget a      -> a
        CrossDocTarget _ a -> a
        ParamTarget _ a    -> a
  in case getDocStateAt curDoc dl of
    Nothing -> Nothing
    Just docState ->
      let (_, total, allLines, marks) = docInfo docState
      in either (const Nothing) Just $ resolveAddr curLine total marks allLines addr


-- Returns a function that takes (curLine, totalLines) and returns (startLine, endLine).
commandDefaultRange :: Command -> (Int -> Int -> (Int, Int))
commandDefaultRange cmd = case cmd of
  -- Commands that default to entire file (1,$)
  Write _ _           -> defaultRangeAll
  WriteShell _ _      -> defaultRangeAll
  WriteQuit _ _       -> defaultRangeAll
  WriteQuitShell _ _  -> defaultRangeAll
  Global _ _ _        -> defaultRangeAll
  GlobalInteractive _ _ -> defaultRangeAll
  GlobalReverse _ _ _ -> defaultRangeAll
  GlobalReverseInteractive _ _ -> defaultRangeAll
  -- Commands that default to .,.+1
  Join _ _            -> defaultRangeJoin
  -- All other commands default to current line
  _                   -> defaultRangeCurrent


-- If current line is 0 or invalid, use line 1 (or total if empty)
defaultRangeCurrent :: Int -> Int -> (Int, Int)
defaultRangeCurrent cur total
  | cur < 1   = (max 1 total, max 1 total)
  | otherwise = (cur, cur)

defaultRangeAll :: Int -> Int -> (Int, Int)
defaultRangeAll _ total
  | total < 1 = (1, 1)  -- Empty document
  | otherwise = (1, total)

defaultRangeJoin :: Int -> Int -> (Int, Int)
defaultRangeJoin cur total
  | cur < 1   = (max 1 total, max 1 total)
  | otherwise = (cur, min total (cur + 1))


-- Used when a command letter is typed but the command is not yet complete.
commandPrefixDefaultRange :: Char -> (Int -> Int -> (Int, Int))
commandPrefixDefaultRange c = case c of
  -- Commands that default to entire file (1,$)
  'w' -> defaultRangeAll
  'W' -> defaultRangeAll
  'g' -> defaultRangeAll
  'G' -> defaultRangeAll
  'v' -> defaultRangeAll
  'V' -> defaultRangeAll
  -- Commands that default to .,.+1
  'j' -> defaultRangeJoin
  -- All other commands (including 's') default to current line
  _ -> defaultRangeCurrent


-- Behavior:
-- - DocDefault with LineDefault: show current document
-- - DocDefault with specific lines: show entire document scrolled to those lines
-- - Any doc range with LineDefault: show document list
-- - Any doc range with specific lines: show first document scrolled to those lines
showAddressPreview :: DocRange -> LineRange -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showAddressPreview docRange lineRange dl paramStack width height =
  case docRange of
    DocDefault ->
      case lineRange of
        LineDefault ->
          -- Just typing - show current document
          showCurrentPosition dl width height
        _ ->
          -- Specific lines typed - show entire document scrolled to those lines
          showDocumentScrolledTo (dlCurrentDoc dl) lineRange dl width height

    DocAll ->
      -- & or similar - show document list with line range highlighted
      case lineRange of
        LineDefault -> showDocumentList dl width height
        _ -> showDocumentListWithHighlight lineRange dl width height

    DocManage ->
      -- Document management - show document list
      showDocumentList dl width height

    DocModified ->
      -- &* - show document list with modified documents highlighted
      showModifiedDocumentList dl width height

    DocSingle addr ->
      -- Single document - show that document
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) ->
          case lineRange of
            LineDefault -> showEntireDocument docIdx dl width height
            _ -> showDocumentScrolledTo docIdx lineRange dl width height

    DocFree addrFrom addrTo ->
      -- Document range - show document list or lines across all docs
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocFree addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          case lineRange of
            LineDefault -> showDocumentListRange docStart docEnd dl width height
            _ -> showDocRangeWithDefault docStart docEnd lineRange defaultRangeCurrent dl width height

    DocBound addrFrom addrTo ->
      -- Document range (bound) - same as DocFree
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocBound addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          case lineRange of
            LineDefault -> showDocumentListRange docStart docEnd dl width height
            _ -> showDocRangeWithDefault docStart docEnd lineRange defaultRangeCurrent dl width height

    DocParam ->
      -- Parameter document - show param doc with line range highlighting
      showParamDocument lineRange paramStack width height

    DocPrevious ->
      -- Previous range - should have been substituted, show empty
      emptyDisplayZone width height


showDocumentScrolledTo :: Int -> LineRange -> DocumentList -> Int -> Int -> DisplayZone
showDocumentScrolledTo docIdx lineRange dl width height =
  withDocState docIdx dl width height $ \docState ->
    let (curLine, total, _, marks) = docInfo docState
        doc = docDocument docState
        allLines = documentLines doc  -- Need all lines for resolveRange
        (hlStart, hlEnd) = resolveRange curLine total marks allLines lineRange defaultRangeCurrent
    in if total > windowedRenderThreshold
       -- Use windowed rendering for large documents
       then buildWindowedDocumentWithHighlight docIdx total hlStart hlEnd doc width height
       -- Use full rendering for small documents
       else let displayLines = [makeDisplayLine docIdx i (indexText allLines (i - 1))
                                 (if i >= hlStart && i <= hlEnd then StyleSelected else StyleNormal)
                               | i <- [1..total]]
                targetStart = hlStart - 1
                targetEnd = hlEnd - 1
            in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd total height) total width height (Just (targetStart, targetEnd))


showEntireDocument :: Int -> DocumentList -> Int -> Int -> DisplayZone
showEntireDocument docIdx dl width height =
  withDocState docIdx dl width height $ \docState ->
    let (curLine, total, _, _) = docInfo docState
        doc = docDocument docState
    in if total > windowedRenderThreshold
       -- Use windowed rendering for large documents
       then buildWindowedDocument docIdx curLine total curLine doc width height
       -- Use full rendering for small documents
       else let allLines = documentLines doc
                displayLines = [makeDisplayLine docIdx i (indexText allLines (i - 1))
                                 (if i == curLine then StyleSelected else StyleNormal)
                               | i <- [1..total]]
                targetLine = curLine - 1
            in mkDisplayZone displayLines (centerOnLine targetLine total height) total width height (Just (targetLine, targetLine))


showDocumentListRange :: Int -> Int -> DocumentList -> Int -> Int -> DisplayZone
showDocumentListRange docStart docEnd dl width height =
  let dlState = dlDocListState dl
      dlDoc = docDocument dlState
      allLines = documentLines dlDoc
      curDoc = dlCurrentDoc dl
      -- Create display lines only for docs in range
      displayLines = V.fromList
        [ makeDocListLineForRange (i + 1) (indexText allLines i) curDoc
        | i <- [docStart - 1 .. docEnd - 1]
        , i >= 0 && i < lineCount dlDoc
        ]
      scrollTop = 0  -- Start at top for filtered list
      totalLines = V.length displayLines
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Nothing  -- Document list, no specific line target
    }
  where
    makeDocListLineForRange docIdx text curDocIdx =
      let isCurrent = docIdx == curDocIdx
          isModified = case getDocStateAt docIdx dl of
            Nothing -> False
            Just ds -> docChangeFlag ds /= Unchanged
          style = if isCurrent then StyleSelected else StyleNormal
          formatted = formatDisplayLine LineFormat
            { lfLineNum = Just docIdx
            , lfDocNum = Nothing
            , lfGutterMark = if isCurrent then ">" else " "
            , lfContent = text
            , lfSuffixMark = if isModified then "*" else ""
            }
      in DisplayLine
        { dlLineNum = Nothing  -- Line number already in formatted text
        , dlSourceLine = Just docIdx  -- Source is the document index
        , dlDocIdx = 0
        , dlText = formatted
        , dlStyle = style
        , dlHighlights = []
        }


-- Used by commands to show their affected range.
showResolvedRangeWithDefault :: DocRange -> LineRange -> (Int -> Int -> (Int, Int)) -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showResolvedRangeWithDefault docRange lineRange defaultRangeFn dl paramStack width height =
  case docRange of
    DocDefault ->
      -- Single document - show entire document with lines highlighted
      showLineRangeInDocWithDefault (dlCurrentDoc dl) lineRange defaultRangeFn dl width height

    DocAll ->
      -- All documents - show document list with line range highlighted
      showDocumentListWithHighlightDefault lineRange defaultRangeFn dl width height

    DocManage ->
      -- Document management - show document list
      showDocumentList dl width height

    DocModified ->
      -- Modified documents - show document list with modified highlighted
      showModifiedDocumentList dl width height

    DocSingle addr ->
      -- Single document by address - resolve and show
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> showLineRangeInDocWithDefault docIdx lineRange defaultRangeFn dl width height

    DocFree addrFrom addrTo ->
      -- Document range - show documents in range
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocFree addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) -> showDocRangeWithDefault docStart docEnd lineRange defaultRangeFn dl width height

    DocBound addrFrom addrTo ->
      -- Document range (bound) - show documents in range
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocBound addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) -> showDocRangeWithDefault docStart docEnd lineRange defaultRangeFn dl width height

    DocParam ->
      -- Parameter document - show param doc with line range
      showParamDocumentWithDefault lineRange defaultRangeFn paramStack width height

    DocPrevious ->
      -- Previous range - should have been substituted, show empty
      emptyDisplayZone width height




-- Shows entire document with the specified range highlighted.
showLineRangeInDocWithDefault :: Int -> LineRange -> (Int -> Int -> (Int, Int)) -> DocumentList -> Int -> Int -> DisplayZone
showLineRangeInDocWithDefault docIdx lineRange defaultRangeFn dl width height =
  withDocState docIdx dl width height $ \docState ->
    let (curLine, total, _, marks) = docInfo docState
        doc = docDocument docState
        allLines = documentLines doc  -- Need for resolveRange
        (lineStart, lineEnd) = resolveRange curLine total marks allLines lineRange defaultRangeFn
    in if total > windowedRenderThreshold
       then buildWindowedDocumentWithHighlight docIdx total lineStart lineEnd doc width height
       else let displayLines = [makeDisplayLine docIdx i (indexText allLines (i - 1))
                                 (if i >= lineStart && i <= lineEnd then StyleSelected else StyleNormal)
                               | i <- [1..total]]
                targetStart = lineStart - 1
                targetEnd = lineEnd - 1
            in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd total height) total width height (Just (targetStart, targetEnd))




showLineRange :: Int -> Int -> Int -> DocumentList -> Int -> Int -> DisplayZone
showLineRange docIdx lineStart lineEnd dl width height =
  withDocState docIdx dl width height $ \docState ->
    let doc = docDocument docState
        rangeLineCount = lineEnd - lineStart + 1
    in if rangeLineCount > windowedRenderThreshold
       -- Large range: use windowed rendering
       then buildWindowedLineRange docIdx lineStart lineEnd doc width height
       -- Small range: render all lines
       else let allLines = documentLines doc
                displayLines = [makeDisplayLine docIdx i (indexText allLines (i - 1)) StyleSelected
                               | i <- [lineStart..lineEnd]]
            in mkDisplayZone displayLines (centerOnLine (rangeLineCount `div` 2) rangeLineCount height) rangeLineCount width height (Just (0, rangeLineCount - 1))


buildWindowedLineRange
  :: Int           -- ^ Document index
  -> Int           -- ^ Range start (1-based)
  -> Int           -- ^ Range end (1-based)
  -> Document
  -> Int           -- ^ View width
  -> Int           -- ^ View height
  -> DisplayZone
buildWindowedLineRange docIdx rangeStart rangeEnd doc width height =
  let rangeLineCount = rangeEnd - rangeStart + 1
      -- Center on middle of range
      targetLine = rangeStart + rangeLineCount `div` 2
      scrollTop = centerOnLine (targetLine - rangeStart) rangeLineCount height
      -- Calculate window bounds with buffer
      bufferLines = height * windowBuffer
      windowStart = max rangeStart (scrollTop + rangeStart - bufferLines)
      windowEnd = min rangeEnd (scrollTop + rangeStart + height + bufferLines - 1)
      -- Get only the lines we need
      windowLines = getLines windowStart windowEnd doc
      -- Build display lines
      displayLines =
        [ makeDisplayLine docIdx lineNum (indexTextSafe windowLines (lineNum - windowStart)) StyleSelected
        | lineNum <- [windowStart .. windowEnd]
        ]
      -- Adjust scroll and target range relative to window
      relativeScroll = scrollTop - (windowStart - rangeStart)
      windowSize = windowEnd - windowStart + 1
  in mkWindowedDisplayZone displayLines relativeScroll rangeLineCount (windowStart - rangeStart + 1) width height (Just (0, windowSize - 1))
  where
    indexTextSafe xs idx
      | idx < 0 = ""
      | otherwise = fromMaybe "" (viaNonEmpty head (drop idx xs))


-- Uses windowed rendering for large ranges to avoid freezing.
showPrintedLines :: Suffix -> DocRange -> LineRange -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showPrintedLines suffix docRange lineRange dl paramStack width height =
  let singleDocPreview docIdx docState =
        let (curLine, total, _, marks) = docInfo docState
            doc = docDocument docState
            allLines = documentLines doc
            (lineStart, lineEnd) = resolveRange curLine total marks allLines lineRange defaultRangeCurrent
            rangeLineCount = lineEnd - lineStart + 1
        in if rangeLineCount > windowedRenderThreshold
           then buildWindowedPrintedLines docIdx suffix lineStart lineEnd doc width height
           else let displayLines = [ makePrintedLine docIdx suffix i (indexText allLines (i - 1))
                                   | i <- [lineStart..lineEnd]]
                in mkDisplayZone displayLines (centerOnLine (rangeLineCount `div` 2) rangeLineCount height) rangeLineCount width height Nothing
  in case docRange of
    DocDefault ->
      withDocState (dlCurrentDoc dl) dl width height (singleDocPreview (dlCurrentDoc dl))

    DocSingle addr ->
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocSingle addr) of
        Left _ -> emptyDisplayZone width height
        Right (docIdx, _) -> withDocState docIdx dl width height (singleDocPreview docIdx)

    DocParam ->
      case paramStack of
        [] -> emptyDisplayZone width height
        ((_, docState):_) -> singleDocPreview (-1) docState

    DocAll ->
      singleDocPreview 0 (dlDocListState dl)

    DocManage ->
      singleDocPreview 0 (dlDocListState dl)

    DocModified ->
      singleDocPreview 0 (dlDocListState dl)

    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


buildWindowedPrintedLines
  :: Int           -- ^ Document index
  -> Suffix        -- ^ Print suffix (p, n, l)
  -> Int           -- ^ Range start (1-based)
  -> Int           -- ^ Range end (1-based)
  -> Document  -- ^ Source document
  -> Int           -- ^ View width
  -> Int           -- ^ View height
  -> DisplayZone
buildWindowedPrintedLines docIdx suffix rangeStart rangeEnd doc width height =
  let rangeLineCount = rangeEnd - rangeStart + 1
      -- Center on middle of range
      targetLine = rangeStart + rangeLineCount `div` 2
      scrollTop = centerOnLine (targetLine - rangeStart) rangeLineCount height
      -- Calculate window bounds with buffer
      bufferLines = height * windowBuffer
      windowStart = max rangeStart (scrollTop + rangeStart - bufferLines)
      windowEnd = min rangeEnd (scrollTop + rangeStart + height + bufferLines - 1)
      -- Get only the lines we need from the document
      windowLines = getLines windowStart windowEnd doc
      -- Build display lines for visible window only
      displayLines =
        [ makePrintedLine docIdx suffix lineNum (indexTextSafe windowLines (lineNum - windowStart))
        | lineNum <- [windowStart .. windowEnd]
        ]
      -- Adjust scroll position relative to window
      relativeScroll = scrollTop - (windowStart - rangeStart)
  in mkWindowedDisplayZone displayLines relativeScroll rangeLineCount (windowStart - rangeStart + 1) width height Nothing
  where
    indexTextSafe xs idx
      | idx < 0 = ""
      | otherwise = fromMaybe "" (viaNonEmpty head (drop idx xs))

makePrintedLine :: Int -> Suffix -> Int -> Text -> DisplayLine
makePrintedLine docIdx suffix lineNum lineText =
  let formattedText = case suffix of
        NoSuffix    -> lineText
        PrintSuffix -> lineText
        NumberSuffix -> show lineNum <> "\t" <> lineText
        ListSuffix  -> listFormat lineText
  in DisplayLine
    { dlLineNum = Nothing  -- No gutter line number (text includes it if needed)
    , dlSourceLine = Just lineNum
    , dlDocIdx = docIdx
    , dlText = formattedText
    , dlStyle = StyleNormal
    , dlHighlights = []
    }


showDocRangeWithDefault :: Int -> Int -> LineRange -> (Int -> Int -> (Int, Int)) -> DocumentList -> Int -> Int -> DisplayZone
showDocRangeWithDefault docStart docEnd lineRange defaultRangeFn dl width height =
  -- Build display lines for all documents in range using doc:line gutter format
  let allDisplayLines = concatMap (docDisplayLines dl lineRange defaultRangeFn) [docStart..docEnd]
      displayLines = V.fromList allDisplayLines
      scrollTop = 0  -- Start at top for multi-doc view
      totalLines = V.length displayLines
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Nothing  -- Multi-doc view, no specific line target
    }
  where
    docDisplayLines :: DocumentList -> LineRange -> (Int -> Int -> (Int, Int)) -> Int -> [DisplayLine]
    docDisplayLines docList lRange defFn idx =
      case getDocStateAt idx docList of
        Nothing -> []
        Just docState ->
          let doc = docDocument docState
              allLines = documentLines doc
              total = lineCount doc
              curLine = docCurrentLine docState
              marks = docMarks docState
              -- Apply default range when LineDefault
              effectiveRange = case lRange of
                LineDefault ->
                  let (s, e) = defFn curLine total
                  in LineFree (Number s) (Number e)
                _ -> lRange
              -- Resolve the line range for this document
              (lineStart, lineEnd) = case resolveLineRange curLine total marks allLines effectiveRange of
                Left _ -> defFn curLine total  -- Use default on error
                Right r -> r
              -- Content lines using doc:line format (like n command)
          in [ makeMultiDocDisplayLine idx i (indexText allLines (i - 1)) StyleSelected
             | i <- [lineStart .. lineEnd]
             ]

-- Uses tab-separated format so renderLine displays doc:line in gutter.
makeMultiDocDisplayLine :: Int -> Int -> Text -> LineStyle -> DisplayLine
makeMultiDocDisplayLine docIdx lineNum text style =
  let formatted = formatDisplayLine LineFormat
        { lfLineNum = Just lineNum
        , lfDocNum = Just docIdx
        , lfGutterMark = ""
        , lfContent = text
        , lfSuffixMark = ""
        }
  in DisplayLine
    { dlLineNum = Nothing  -- Use tab-based gutter rendering
    , dlSourceLine = Just lineNum
    , dlDocIdx = docIdx
    , dlText = formatted
    , dlStyle = style
    , dlHighlights = []
    }




-- Display Generation

makeDisplayLine :: Int -> Int -> Text -> LineStyle -> DisplayLine
makeDisplayLine docIdx lineNum text style = DisplayLine
  { dlLineNum = Just lineNum
  , dlSourceLine = Just lineNum
  , dlDocIdx = docIdx
  , dlText = text
  , dlStyle = style
  , dlHighlights = []
  }




showCurrentPosition :: DocumentList -> Int -> Int -> DisplayZone
showCurrentPosition dl width height =
  let curDoc = dlCurrentDoc dl
  in case getDocStateAt curDoc dl of
    Nothing -> showDocumentList dl width height
    Just docState ->
      let (curLine, total, _, _) = docInfo docState
          doc = docDocument docState
      in if total > windowedRenderThreshold
         then buildWindowedDocument curDoc curLine total curLine doc width height
         else let allLines = documentLines doc
                  displayLines = [makeDisplayLine curDoc i (indexText allLines (i - 1))
                                   (if i == curLine then StyleSelected else StyleNormal)
                                 | i <- [1..total]]
                  targetLine = curLine - 1
              in mkDisplayZone displayLines (centerOnLine targetLine total height) total width height (Just (targetLine, targetLine))


-- Unlike showDocumentScrolledTo, this shows the target as the "new current line"
-- rather than as a "selected range for output"
showNewCurrentLine :: Int -> LineRange -> DocumentList -> Int -> Int -> DisplayZone
showNewCurrentLine docIdx lineRange dl width height =
  withDocState docIdx dl width height $ \docState ->
    let (curLine, total, _, marks) = docInfo docState
        doc = docDocument docState
        allLines = documentLines doc  -- Need for resolveRange
        -- Resolve to single line (use end of range as target)
        (_, targetLine) = resolveRange curLine total marks allLines lineRange defaultRangeCurrent
    in if total > windowedRenderThreshold
       then buildWindowedDocument docIdx targetLine total targetLine doc width height
       else let displayLines = [makeDisplayLine docIdx i (indexText allLines (i - 1))
                                 (if i == targetLine then StyleSelected else StyleNormal)
                               | i <- [1..total]]
                targetIdx = targetLine - 1
            in mkDisplayZone displayLines (centerOnLine targetIdx total height) total width height (Just (targetIdx, targetIdx))


showDocumentList :: DocumentList -> Int -> Int -> DisplayZone
showDocumentList dl width height =
  let dlState = dlDocListState dl
      dlDoc = docDocument dlState
      allLines = documentLines dlDoc
      totalLines = lineCount dlDoc
      curLine = docCurrentLine dlState
      curDoc = dlCurrentDoc dl
      -- Create display lines with doc markers
      displayLines = V.fromList
        [ makeDocListLine (i + 1) (indexText allLines i) curDoc
        | i <- [0 .. totalLines - 1]
        ]
      targetLine = curLine - 1
      scrollTop = centerOnLine targetLine totalLines height
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Just (targetLine, targetLine)
    }
  where
    makeDocListLine docIdx text curDocIdx =
      let isCurrent = docIdx == curDocIdx
          isModified = case getDocStateAt docIdx dl of
            Nothing -> False
            Just ds -> docChangeFlag ds /= Unchanged
          style = if isCurrent then StyleSelected else StyleNormal
          formatted = formatDisplayLine LineFormat
            { lfLineNum = Just docIdx
            , lfDocNum = Nothing
            , lfGutterMark = if isCurrent then ">" else " "
            , lfContent = text
            , lfSuffixMark = if isModified then "*" else ""
            }
      in DisplayLine
        { dlLineNum = Nothing  -- Line number already in formatted text
        , dlSourceLine = Just docIdx  -- Source is the document index
        , dlDocIdx = 0
        , dlText = formatted
        , dlStyle = style
        , dlHighlights = []
        }


-- Non-consecutive modified documents are all highlighted.
showModifiedDocumentList :: DocumentList -> Int -> Int -> DisplayZone
showModifiedDocumentList dl width height =
  let dlState = dlDocListState dl
      dlDoc = docDocument dlState
      allLines = documentLines dlDoc
      totalLines = lineCount dlDoc
      curDoc = dlCurrentDoc dl
      modifiedDocs = unsavedDocuments dl
      -- Create display lines with modified docs highlighted
      displayLines = V.fromList
        [ makeModifiedDocListLine (i + 1) (indexText allLines i) curDoc modifiedDocs
        | i <- [0 .. totalLines - 1]
        ]
      -- Scroll to first modified doc if any, else current
      firstModified = case modifiedDocs of
        (m:_) -> m - 1
        [] -> curDoc - 1
      scrollTop = scrollToShowRange firstModified firstModified totalLines height
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = case modifiedDocs of
        [] -> Nothing
        (m:_) -> Just (m - 1, m - 1)  -- Target first modified
    }
  where
    makeModifiedDocListLine docIdx text curDocIdx modDocs =
      let isCurrent = docIdx == curDocIdx
          isModified = docIdx `elem` modDocs
          -- Highlight modified docs, not just current
          style = if isModified then StyleSelected else StyleNormal
          formatted = formatDisplayLine LineFormat
            { lfLineNum = Just docIdx
            , lfDocNum = Nothing
            , lfGutterMark = if isCurrent then ">" else " "
            , lfContent = text
            , lfSuffixMark = if isModified then "*" else ""
            }
      in DisplayLine
        { dlLineNum = Nothing
        , dlSourceLine = Just docIdx
        , dlDocIdx = 0
        , dlText = formatted
        , dlStyle = style
        , dlHighlights = []
        }


-- Used for previewing address like "1,5&" - shows all documents, highlights entries 1-5.
showDocumentListWithHighlight :: LineRange -> DocumentList -> Int -> Int -> DisplayZone
showDocumentListWithHighlight lineRange dl width height =
  showDocumentListWithHighlightDefault lineRange defaultRangeCurrent dl width height


showDocumentListWithHighlightDefault :: LineRange -> (Int -> Int -> (Int, Int)) -> DocumentList -> Int -> Int -> DisplayZone
showDocumentListWithHighlightDefault lineRange defaultRangeFn dl width height =
  let dlState = dlDocListState dl
      dlDoc = docDocument dlState
      allLines = documentLines dlDoc
      totalLines = lineCount dlDoc
      curLine = docCurrentLine dlState
      curDoc = dlCurrentDoc dl
      marks = docMarks dlState
      -- Resolve the line range within the document list
      (hlStart, hlEnd) = resolveRange curLine totalLines marks allLines lineRange defaultRangeFn
      -- Create display lines with range highlighting
      displayLines = V.fromList
        [ makeHighlightedDocListLine (i + 1) (indexText allLines i) curDoc hlStart hlEnd
        | i <- [0 .. totalLines - 1]
        ]
      targetStart = hlStart - 1
      targetEnd = hlEnd - 1
      scrollTop = scrollToShowRange targetStart targetEnd totalLines height
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
    , dzTotalLines = totalLines
    , dzWindowStart = 1
    , dzHeight = height
    , dzWidth = width
    , dzTargetRange = Just (targetStart, targetEnd)
    }
  where
    makeHighlightedDocListLine docIdx text curDocIdx rangeStart rangeEnd =
      let isCurrent = docIdx == curDocIdx
          isInRange = docIdx >= rangeStart && docIdx <= rangeEnd
          isModified = case getDocStateAt docIdx dl of
            Nothing -> False
            Just ds -> docChangeFlag ds /= Unchanged
          -- Highlight if in range, otherwise normal
          style = if isInRange then StyleSelected else StyleNormal
          formatted = formatDisplayLine LineFormat
            { lfLineNum = Just docIdx
            , lfDocNum = Nothing
            , lfGutterMark = if isCurrent then ">" else " "
            , lfContent = text
            , lfSuffixMark = if isModified then "*" else ""
            }
      in DisplayLine
        { dlLineNum = Nothing
        , dlSourceLine = Just docIdx
        , dlDocIdx = 0
        , dlText = formatted
        , dlStyle = style
        , dlHighlights = []
        }


showParamDocument :: LineRange -> [(Text, DocumentState)] -> Int -> Int -> DisplayZone
showParamDocument lineRange paramStack width height =
  showParamDocumentWithDefault lineRange defaultRangeCurrent paramStack width height

showParamDocumentWithDefault :: LineRange -> (Int -> Int -> (Int, Int)) -> ParamStack -> Int -> Int -> DisplayZone
showParamDocumentWithDefault lineRange defaultRangeFn paramStack width height =
  case paramStack of
    [] -> emptyDisplayZone width height
    ((_, docState):_) ->
      let (curLine, total, allLines, marks) = docInfo docState
          (hlStart, hlEnd) = resolveRange curLine total marks allLines lineRange defaultRangeFn
          displayLines = [DisplayLine
              { dlLineNum = Just i
              , dlSourceLine = Just i
              , dlDocIdx = -1  -- Negative to indicate param doc
              , dlText = indexText allLines (i - 1)
              , dlStyle = if i >= hlStart && i <= hlEnd then StyleSelected else StyleNormal
              , dlHighlights = []
              }
            | i <- [1..total]]
          targetStart = hlStart - 1
          targetEnd = hlEnd - 1
      in mkDisplayZone displayLines (scrollToShowRange targetStart targetEnd total height) total width height (Just (targetStart, targetEnd))


showAddressRange :: Int -> Int -> Int -> Int -> DocumentList -> Int -> Int -> DisplayZone
showAddressRange _docStart _docEnd lineStart lineEnd dl width height =
  showLineRange (dlCurrentDoc dl) lineStart lineEnd dl width height


indexText :: [Text] -> Int -> Text
indexText xs i
  | i < 0 = ""
  | otherwise = fromMaybe "" (viaNonEmpty head (drop i xs))


-- Used when entering visual mode to avoid implicit file display.
showEmptyDisplay :: Int -> Int -> DisplayZone
showEmptyDisplay = emptyDisplayZone
