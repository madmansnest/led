module LedViRender
  ( renderDisplayZone
  , renderToVty
  , renderInputLine

  , generateDisplayForParse
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

  -- Soft wrapping
  , wrapDisplayZone
  , gutterWidth

  -- Unified edit preview
  , EditMode(..)
  , showEditPreview

  , displayZoneBg
  , normalAttr
  , lineNumAttr
  , headerAttr
  , addedAttr
  , deletedAttr
  , matchAttr
  , replacementAttr
  , errorAttr
  , inputAttr
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty

import LedDocument (documentLines, lineCount)
import LedNexus (DocumentList, DocumentState(..), BufferChangeFlag(..), dlCurrentDoc, getDocStateAt, documentCount, dlDocListState, unsavedDocuments)
import LedParse (DocRange(..), LineRange(..), Addr(..), Command(..), FullRange(..), TargetAddr(..), SubstFlags(..), getCommandRange)
import LedPrint (LineFormat(..), formatDisplayLine)
import LedSmartReplace (isSmartReplaceEligible, smartReplace, isAllLower)
import qualified LedRegularExpressions as RE
import LedResolve (resolveDocRange, resolveLineRange, resolveAddr)
import LedViState


-- Color Scheme

displayZoneBg :: Vty.Color
displayZoneBg = Vty.ISOColor 234

normalAttr :: Vty.Attr
normalAttr = Vty.defAttr
  `Vty.withForeColor` Vty.white
  `Vty.withBackColor` displayZoneBg

lineNumAttr :: Vty.Attr
lineNumAttr = Vty.defAttr `Vty.withForeColor` Vty.white

headerAttr :: Vty.Attr
headerAttr = Vty.defAttr
  `Vty.withForeColor` Vty.white
  `Vty.withBackColor` Vty.ISOColor 17
  `Vty.withStyle` Vty.bold

addedAttr :: Vty.Attr
addedAttr = Vty.defAttr
  `Vty.withForeColor` Vty.green
  `Vty.withBackColor` displayZoneBg

deletedAttr :: Vty.Attr
deletedAttr = Vty.defAttr
  `Vty.withForeColor` Vty.ISOColor 245
  `Vty.withBackColor` displayZoneBg
  `Vty.withStyle` Vty.strikethrough
  `Vty.withStyle` Vty.italic

matchAttr :: Vty.Attr
matchAttr = Vty.defAttr
  `Vty.withForeColor` Vty.ISOColor 245  -- Grey
  `Vty.withBackColor` displayZoneBg

replacementAttr :: Vty.Attr
replacementAttr = Vty.defAttr
  `Vty.withForeColor` Vty.black
  `Vty.withBackColor` Vty.yellow
  `Vty.withStyle` Vty.italic

errorAttr :: Vty.Attr
errorAttr = Vty.defAttr
  `Vty.withForeColor` Vty.red

inputAttr :: Vty.Attr
inputAttr = Vty.defAttr


-- Unified Edit Preview
-- All edit previews (delete/append/insert/change/move/transfer) use this system

data EditMode
  = EditDelete                    -- ^ Delete: show lines as deleted
  | EditAppend ![Text]            -- ^ Append: insert new lines after target
  | EditInsert ![Text]            -- ^ Insert: insert new lines before target
  | EditChange ![Text]            -- ^ Change: show deleted + insert new lines
  | EditMove !Int ![Text]         -- ^ Move: delete source, insert at target position
  | EditTransfer !Int ![Text]     -- ^ Transfer: copy lines to target position
  deriving stock (Eq, Show)

-- Handles all edit command previews with a single code path.
showEditPreview :: EditMode -> DocRange -> LineRange -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showEditPreview mode docRange lineRange dl paramStack width height =
  case docRange of
    DocDefault ->
      let curDoc = dlCurrentDoc dl
      in case getDocStateAt curDoc dl of
        Nothing -> emptyDisplayZone width height
        Just docState ->
          let doc = docDocument docState
              curLine = docCurrentLine docState
              totalLines = lineCount doc
              allLines = documentLines doc
              marks = docMarks docState
              -- Resolve the line range
              (rangeStart, rangeEnd) = case lineRange of
                LineDefault -> defaultRangeCurrent curLine totalLines
                _ -> case resolveLineRange curLine totalLines marks allLines lineRange of
                  Left _ -> defaultRangeCurrent curLine totalLines
                  Right r -> r
              -- Build display based on edit mode
              (displayLines, targetStart, targetEnd) = buildEditDisplay mode curDoc rangeStart rangeEnd allLines
              scrollTop = scrollToShowRange targetStart targetEnd (length displayLines) height
          in DisplayZone
            { dzLines = V.fromList displayLines
            , dzScrollTop = scrollTop
            , dzHeight = height
            , dzWidth = width
            , dzTargetRange = Just (targetStart, targetEnd)
            }
    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


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
       -- Delete: mark range as deleted
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

       -- Insert: insert new lines before range start
       EditInsert newLines ->
         let insertIdx = rangeStart - 1  -- 0-based position
             linesBefore = [mkLine i StyleNormal | i <- [1..insertIdx]]
             linesAfter = [mkLine i StyleNormal | i <- [insertIdx + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines newLines ++ linesAfter
             tgtStart = if null newLines then max 0 insertIdx else insertIdx
             tgtEnd = if null newLines then tgtStart else insertIdx + length newLines - 1
         in (allDisplayLines, tgtStart, tgtEnd)

       -- Change: show deleted lines, then new lines
       EditChange newLines ->
         let linesBefore = [mkLine i StyleNormal | i <- [1..rangeStart - 1]]
             linesDeleted = [mkLine i StyleDeleted | i <- [rangeStart..rangeEnd]]
             linesAfter = [mkLine i StyleNormal | i <- [rangeEnd + 1..totalLines]]
             numDeleted = rangeEnd - rangeStart + 1
             allDisplayLines = linesBefore ++ linesDeleted ++ mkNewLines newLines ++ linesAfter
             tgtStart = rangeStart - 1
             tgtEnd = tgtStart + numDeleted + length newLines - 1
         in (allDisplayLines, tgtStart, tgtEnd)

       -- Move: delete source, insert copies at target
       EditMove targetLine srcLines ->
         let mkSrcLine i = mkLine i (if i >= rangeStart && i <= rangeEnd then StyleDeleted else StyleNormal)
             linesBefore = [mkSrcLine i | i <- [1..targetLine]]
             linesAfter = [mkSrcLine i | i <- [targetLine + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines srcLines ++ linesAfter
         in (allDisplayLines, rangeStart - 1, rangeEnd - 1)

       -- Transfer: copy lines to target (source stays normal)
       EditTransfer targetLine srcLines ->
         let linesBefore = [mkLine i StyleNormal | i <- [1..targetLine]]
             linesAfter = [mkLine i StyleNormal | i <- [targetLine + 1..totalLines]]
             allDisplayLines = linesBefore ++ mkNewLines srcLines ++ linesAfter
         in (allDisplayLines, rangeStart - 1, rangeEnd - 1)


-- Rendering

renderToVty :: Vty.Vty -> DisplayZone -> Text -> Bool -> IO ()
renderToVty vty dz inputText isError = do
  let displayImage = renderDisplayZone dz
      inputImage = renderInputLine inputText isError (dzWidth dz)
      fullImage = displayImage Vty.<-> inputImage
      pic = Vty.picForImage fullImage
  Vty.update vty pic


renderDisplayZone :: DisplayZone -> Vty.Image
renderDisplayZone dz
  | V.null (dzLines dz) = emptyZoneImage (dzWidth dz) (dzHeight dz)
  | otherwise =
      let visibleLines = V.slice
            (dzScrollTop dz)
            (min (dzHeight dz) (V.length (dzLines dz) - dzScrollTop dz))
            (dzLines dz)
          lineImages = V.toList $ V.map (renderLine (dzWidth dz)) visibleLines
          -- Pad with empty lines if needed
          padding = dzHeight dz - length lineImages
          paddingImages = replicate padding (emptyLineImage (dzWidth dz))
      in Vty.vertCat (lineImages ++ paddingImages)


emptyZoneImage :: Int -> Int -> Vty.Image
emptyZoneImage width height =
  let emptyLine = emptyLineImage width
  in Vty.vertCat (replicate height emptyLine)


-- Layout: 8-column gutter (default bg) + content area (black bg)
-- - Lines with dlLineNum: gutter shows line number, content area shows text
-- - Command output with tab: gutter shows text up to tab (n command), rest in content
-- - Other command output: empty gutter, text in content area
renderLine :: Int -> DisplayLine -> Vty.Image
renderLine width dl =
  let contentWidth = width - gutterWidth
  in case dlLineNum dl of
    -- Line with explicit line number: show in gutter (left-aligned like ,n command)
    Just n ->
      let lineNumImg = Vty.string lineNumAttr (rightPad gutterWidth (show n))
          contentStr = T.unpack (dlText dl)
          contentAttr = styleToAttr (dlStyle dl)
          contentImg = if null (dlHighlights dl)
            then Vty.string contentAttr (take contentWidth contentStr)
            else renderWithHighlights contentAttr contentStr (dlHighlights dl) contentWidth
          contentLen = min contentWidth (length contentStr)
          paddingLen = contentWidth - contentLen
          paddingImg = Vty.string normalAttr (replicate paddingLen ' ')
      in lineNumImg Vty.<|> contentImg Vty.<|> paddingImg
    -- No explicit line number: check for tab (n command output)
    Nothing ->
      let rawText = T.unpack (dlText dl)
      in case break (== '\t') rawText of
        -- Has tab: text before tab goes in gutter (n command), rest in content
        (beforeTab, '\t':afterTab) ->
          let gutterStr = take gutterWidth (beforeTab ++ replicate gutterWidth ' ')
              gutterImg = Vty.string lineNumAttr gutterStr
              contentAttr = styleToAttr (dlStyle dl)
              contentImg = if null (dlHighlights dl)
                then Vty.string contentAttr (take contentWidth afterTab)
                else renderWithHighlights contentAttr afterTab (dlHighlights dl) contentWidth
              contentLen = min contentWidth (length afterTab)
              paddingLen = contentWidth - contentLen
              paddingImg = Vty.string normalAttr (replicate paddingLen ' ')
          in gutterImg Vty.<|> contentImg Vty.<|> paddingImg
        -- No tab: empty gutter, all text in content area
        _ ->
          let gutterImg = Vty.string lineNumAttr (replicate gutterWidth ' ')
              contentAttr = styleToAttr (dlStyle dl)
              contentImg = if null (dlHighlights dl)
                then Vty.string contentAttr (take contentWidth rawText)
                else renderWithHighlights contentAttr rawText (dlHighlights dl) contentWidth
              contentLen = min contentWidth (length rawText)
              paddingLen = contentWidth - contentLen
              paddingImg = Vty.string normalAttr (replicate paddingLen ' ')
          in gutterImg Vty.<|> contentImg Vty.<|> paddingImg


styleToAttr :: LineStyle -> Vty.Attr
styleToAttr StyleNormal   = normalAttr
styleToAttr StyleAdded    = addedAttr
styleToAttr StyleDeleted  = deletedAttr
styleToAttr StyleHeader   = headerAttr
styleToAttr StyleSelected = normalAttr `Vty.withStyle` Vty.reverseVideo


renderWithHighlights :: Vty.Attr -> String -> [(Int, Int, HighlightStyle)] -> Int -> Vty.Image
renderWithHighlights baseAttr content highlights maxLen =
  let truncated = take maxLen content
      -- Sort highlights by start position
      sortedHLs = sortOn (\(s, _, _) -> s) highlights
      -- Build image segments
      segments = buildSegments 0 truncated sortedHLs
  in Vty.horizCat segments
  where
    buildSegments :: Int -> String -> [(Int, Int, HighlightStyle)] -> [Vty.Image]
    buildSegments _ [] _ = []
    buildSegments _ str [] = [Vty.string baseAttr str]
    buildSegments pos str ((start, end, hl):hls)
      | pos < start =
          let (before, rest) = splitAt (start - pos) str
          in Vty.string baseAttr before : buildSegments start rest ((start, end, hl):hls)
      | pos >= start && pos < end =
          let hlLen = min (end - pos) (length str)
              (hlText, rest) = splitAt hlLen str
              hlAttr = highlightToAttr hl
          in Vty.string hlAttr hlText : buildSegments (pos + hlLen) rest hls
      | otherwise = buildSegments pos str hls

    highlightToAttr HLMatch       = matchAttr
    highlightToAttr HLReplacement = replacementAttr
    highlightToAttr HLSelection   = normalAttr `Vty.withStyle` Vty.reverseVideo


emptyLineImage :: Int -> Vty.Image
emptyLineImage width =
  let contentWidth = width - gutterWidth
      gutterImg = Vty.string lineNumAttr (replicate gutterWidth ' ')
      contentImg = Vty.string normalAttr (replicate contentWidth ' ')
  in gutterImg Vty.<|> contentImg


renderInputLine :: Text -> Bool -> Int -> Vty.Image
renderInputLine inputText isError width =
  let attr = if isError then errorAttr else inputAttr
      content = T.unpack inputText
      padded = content ++ replicate (width - length content) ' '
  in Vty.string attr (take width padded)


rightPad :: Int -> String -> String
rightPad n s = s ++ replicate (n - length s) ' '


gutterWidth :: Int
gutterWidth = 8

-- Long lines are split at the last whitespace before the limit.
-- Continuation lines have dlLineNum = Nothing.
-- Updates dzLines and adjusts dzScrollTop to account for wrapped lines.
wrapDisplayZone :: DisplayZone -> DisplayZone
wrapDisplayZone dz =
  let contentWidth = dzWidth dz - gutterWidth
      -- Wrap each line and track the mapping from logical to visual indices
      (wrappedLines, logicalToVisual) = wrapAllLines contentWidth (V.toList (dzLines dz))
      totalVisualLines = length wrappedLines
      -- Adjust target range if present, and recalculate scroll based on visual range
      (newScrollTop, newTargetRange) = case dzTargetRange dz of
        Nothing ->
          -- No target range: just convert scroll position
          (findVisualIndex (dzScrollTop dz) logicalToVisual, Nothing)
        Just (start, end) ->
          -- Convert target range to visual coordinates
          let vStart = findVisualIndex start logicalToVisual
              vEnd = findVisualIndexEnd end logicalToVisual totalVisualLines
              -- Recalculate scroll position based on visual target range
              vScrollTop = scrollToShowRange vStart vEnd totalVisualLines (dzHeight dz)
          in (vScrollTop, Just (vStart, vEnd))
  in dz { dzLines = V.fromList wrappedLines
        , dzScrollTop = newScrollTop
        , dzTargetRange = newTargetRange
        }

-- The mapping is a list where index i contains the visual index of logical line i.
wrapAllLines :: Int -> [DisplayLine] -> ([DisplayLine], [Int])
wrapAllLines contentWidth logicalLines =
  let (wrapped, indices, _) = foldl' wrapOne ([], [], 0) logicalLines
  in (reverse wrapped, reverse indices)
  where
    wrapOne :: ([DisplayLine], [Int], Int) -> DisplayLine -> ([DisplayLine], [Int], Int)
    wrapOne (accLines, accIndices, visualIdx) dl =
      let wrappedDl = wrapDisplayLine contentWidth dl
          numVisual = length wrappedDl
      in (reverse wrappedDl ++ accLines, visualIdx : accIndices, visualIdx + numVisual)

findVisualIndex :: Int -> [Int] -> Int
findVisualIndex logicalIdx mapping
  | logicalIdx < 0 = 0
  | logicalIdx < length mapping = fromMaybe 0 (viaNonEmpty head (drop logicalIdx mapping))
  | otherwise = fromMaybe 0 (viaNonEmpty last mapping)

-- Find the visual index of the END of a logical line.
-- For lines other than the last, this is (start of next line - 1).
-- For the last line, this is (totalVisualLines - 1).
findVisualIndexEnd :: Int -> [Int] -> Int -> Int
findVisualIndexEnd logicalIdx mapping totalVisualLines
  | logicalIdx < 0 = 0
  | logicalIdx + 1 < length mapping =
      fromMaybe 0 ((\x -> x - 1) <$> viaNonEmpty head (drop (logicalIdx + 1) mapping))
  | otherwise = max 0 (totalVisualLines - 1)

-- The first line keeps the line number; continuation lines have dlLineNum = Nothing.
wrapDisplayLine :: Int -> DisplayLine -> [DisplayLine]
wrapDisplayLine contentWidth dl
  | contentWidth <= 0 = [dl]  -- Safety: avoid infinite loop
  | T.length (dlText dl) <= contentWidth = [dl]  -- No wrapping needed
  | otherwise = wrapLine contentWidth dl

wrapLine :: Int -> DisplayLine -> [DisplayLine]
wrapLine contentWidth dl = go True (dlText dl) (dlHighlights dl)
  where
    go :: Bool -> Text -> [(Int, Int, HighlightStyle)] -> [DisplayLine]
    go _ "" _ = []
    go isFirst txt hls =
      let (chunk, rest, chunkLen) = splitAtWrap contentWidth txt
          -- Extract highlights for this chunk and adjust remaining highlights
          (chunkHls, restHls) = splitHighlights chunkLen hls
          thisLine = dl
            { dlLineNum = if isFirst then dlLineNum dl else Nothing
            , dlText = chunk
            , dlHighlights = chunkHls
            }
      in thisLine : go False rest restHls

-- Tries to break at the last whitespace before the limit.
-- Returns (chunk, rest, chunkLength).
splitAtWrap :: Int -> Text -> (Text, Text, Int)
splitAtWrap maxLen txt
  | T.length txt <= maxLen = (txt, "", T.length txt)
  | otherwise =
      let prefix = T.take maxLen txt
          -- Find last whitespace in prefix
          lastWsIdx = T.length $ T.dropWhileEnd (not . isWrapSpace) prefix
      in if lastWsIdx > 0 && lastWsIdx < maxLen
           -- Break after the last whitespace
           then let chunk = T.take lastWsIdx prefix
                    rest = T.drop lastWsIdx txt
                in (chunk, rest, T.length chunk)
           -- No whitespace found, hard break at limit
           else (prefix, T.drop maxLen txt, maxLen)
  where
    isWrapSpace c = c == ' ' || c == '\t'

-- Returns (highlights for current chunk, adjusted highlights for rest).
splitHighlights :: Int -> [(Int, Int, HighlightStyle)] -> ([(Int, Int, HighlightStyle)], [(Int, Int, HighlightStyle)])
splitHighlights splitPoint hls = (current, shifted)
  where
    current = mapMaybe (clipHighlight 0 splitPoint) hls
    shifted = mapMaybe (shiftHighlight splitPoint) hls

    -- Clip a highlight to fit within [lo, hi)
    clipHighlight :: Int -> Int -> (Int, Int, HighlightStyle) -> Maybe (Int, Int, HighlightStyle)
    clipHighlight lo hi (s, e, style)
      | e <= lo = Nothing
      | s >= hi = Nothing
      | otherwise = Just (max lo s, min hi e, style)

    -- Shift a highlight by subtracting the split point
    shiftHighlight :: Int -> (Int, Int, HighlightStyle) -> Maybe (Int, Int, HighlightStyle)
    shiftHighlight offset (s, e, style)
      | e <= offset = Nothing
      | otherwise = Just (max 0 (s - offset), e - offset, style)


-- Display Generation Based on Parse State

-- Type alias for param stack for clarity
type ParamStack = [(Text, DocumentState)]

-- This is the main function for Phase 2 - it examines the parse state
-- and generates appropriate display content.
-- Display behavior:
-- - On entry (PPEmpty): Show current document with current line highlighted
-- - When doc range typed (no line range): Show document list for that range
-- - When specific lines typed: Show entire document scrolled to those lines
generateDisplayForParse :: PartialParse -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
generateDisplayForParse pp dl paramStack width height = case pp of
  PPEmpty ->
    -- No input - show current document with current line highlighted
    showCurrentPosition dl width height

  PPAddress docRange lineRange ->
    -- Address being typed - show documents or document with highlighted lines
    showAddressPreview docRange lineRange dl paramStack width height

  PPCommand cmd ->
    -- Complete command - show the range that will be affected
    showCommandRange cmd dl paramStack width height

  PPSubstPattern docRange lineRange pat ->
    -- Typing substitute pattern - show matches with strikethrough
    showSubstitutePatternPreview docRange lineRange pat dl paramStack width height

  PPSubstReplace docRange lineRange pat repl flags ->
    -- Have pattern and replacement - show matches with strikethrough and replacement in yellow
    showSubstituteReplacePreview docRange lineRange pat repl flags dl paramStack width height

  PPGlobalPattern docRange lineRange _delim _pat ->
    -- Typing global pattern - show the range
    -- TODO: Phase 3 will add match highlighting
    showResolvedRangeWithDefault docRange lineRange defaultRangeAll dl paramStack width height

  PPGlobalCommand docRange lineRange _delim _pat _cmd ->
    -- Have global pattern and command
    showResolvedRangeWithDefault docRange lineRange defaultRangeAll dl paramStack width height

  PPAppendText docRange lineRange textLines ->
    -- Append command with text being typed - show preview
    showEditPreview (EditAppend textLines) docRange lineRange dl paramStack width height

  PPInsertText docRange lineRange textLines ->
    -- Insert command with text being typed - show preview
    showEditPreview (EditInsert textLines) docRange lineRange dl paramStack width height

  PPChangeText docRange lineRange textLines ->
    -- Change command with text being typed - show preview
    showEditPreview (EditChange textLines) docRange lineRange dl paramStack width height

  PPCommandPrefix docRange lineRange cmdChar ->
    -- Command letter typed but not complete - show default range for that command
    let defaultRange = commandPrefixDefaultRange cmdChar
    in showResolvedRangeWithDefault docRange lineRange defaultRange dl paramStack width height

  PPError _ ->
    -- Parse error - keep showing current position
    showCurrentPosition dl width height

  PPIncomplete ->
    -- Incomplete input - show current position
    showCurrentPosition dl width height


showCommandRange :: Command -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showCommandRange cmd dl paramStack width height =
  case cmd of
    -- Move: show source lines as deleted, insert copies after target
    Move (FullRange docRange lineRange) target _ ->
      showMoveTransferCommand True docRange lineRange target dl paramStack width height

    -- Transfer (copy): show source lines normal, insert copies after target
    Transfer (FullRange docRange lineRange) target _ ->
      showMoveTransferCommand False docRange lineRange target dl paramStack width height

    -- Delete: show lines to be deleted as strikethrough
    Delete (FullRange docRange lineRange) _ ->
      showEditPreview EditDelete docRange lineRange dl paramStack width height

    -- Append: show insertion point (line will be added after)
    Append (FullRange docRange lineRange) txt _ ->
      showEditPreview (EditAppend (T.lines txt)) docRange lineRange dl paramStack width height

    -- Insert: show insertion point (line will be added before)
    Insert (FullRange docRange lineRange) txt _ ->
      showEditPreview (EditInsert (T.lines txt)) docRange lineRange dl paramStack width height

    -- Change: show lines to be replaced as deleted, new lines as added
    Change (FullRange docRange lineRange) txt _ ->
      showEditPreview (EditChange (T.lines txt)) docRange lineRange dl paramStack width height

    -- Substitute: show matches with strikethrough and replacements in yellow
    Substitute (FullRange docRange lineRange) pat repl flags _ ->
      showSubstituteReplacePreview docRange lineRange pat repl flags dl paramStack width height

    -- Undo/Redo: no preview change, keep showing current position
    Undo _ -> showCurrentPosition dl width height
    Redo _ -> showCurrentPosition dl width height

    -- Other commands: show affected range
    _ -> case getCommandRange cmd of
      Nothing ->
        -- Commands without ranges (Quit, Help, etc.) - show empty display
        emptyDisplayZone width height
      Just (FullRange docRange lineRange) ->
        -- Use command-specific default when LineDefault
        let defaultRange = commandDefaultRange cmd
        in showResolvedRangeWithDefault docRange lineRange defaultRange dl paramStack width height


-- Shows lines with regex matches highlighted in strikethrough.
-- When pattern only is available, shows first match per line with smart search if pattern is all lowercase.
showSubstitutePatternPreview :: DocRange -> LineRange -> Text -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showSubstitutePatternPreview docRange lineRange pat dl paramStack width height =
  case docRange of
    DocDefault ->
      let curDoc = dlCurrentDoc dl
      in case getDocStateAt curDoc dl of
        Nothing -> emptyDisplayZone width height
        Just docState ->
          let doc = docDocument docState
              curLine = docCurrentLine docState
              totalLines = lineCount doc
              allLines = documentLines doc
              marks = docMarks docState
              -- Resolve line range
              (startLine, endLine) = case lineRange of
                LineDefault -> (curLine, curLine)
                _ -> case resolveLineRange curLine totalLines marks allLines lineRange of
                  Left _ -> (curLine, curLine)
                  Right (s, e) -> (s, e)
              -- Try to compile the regex
              mBre = case RE.parseBRE pat of
                Left _ -> Nothing
                Right bre -> Just bre
              -- Smart search: pattern all lowercase means case-insensitive matching
              -- No replacement yet, so we use default flags (first match only)
              smartMode = isAllLower pat
              defaultFlags = SubstFlags False 0 False  -- first match only
              -- Build display lines with match highlights
              displayLines = buildSubstitutePreviewLines curDoc mBre Nothing defaultFlags smartMode allLines startLine endLine totalLines
              -- Target range for scroll check (0-based)
              targetStart = startLine - 1
              targetEnd = endLine - 1
              scrollTop = scrollToShowRange targetStart targetEnd (length displayLines) height
          in DisplayZone
            { dzLines = V.fromList displayLines
            , dzScrollTop = scrollTop
            , dzHeight = height
            , dzWidth = width
            , dzTargetRange = Just (targetStart, targetEnd)
            }
    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


-- Shows lines with regex matches in strikethrough and replacement text in yellow.
-- Respects smart search/replace and substitute flags.
showSubstituteReplacePreview :: DocRange -> LineRange -> Text -> Text -> SubstFlags -> DocumentList -> ParamStack -> Int -> Int -> DisplayZone
showSubstituteReplacePreview docRange lineRange pat repl flags dl paramStack width height =
  case docRange of
    DocDefault ->
      let curDoc = dlCurrentDoc dl
      in case getDocStateAt curDoc dl of
        Nothing -> emptyDisplayZone width height
        Just docState ->
          let doc = docDocument docState
              curLine = docCurrentLine docState
              totalLines = lineCount doc
              allLines = documentLines doc
              marks = docMarks docState
              -- Resolve line range
              (startLine, endLine) = case lineRange of
                LineDefault -> (curLine, curLine)
                _ -> case resolveLineRange curLine totalLines marks allLines lineRange of
                  Left _ -> (curLine, curLine)
                  Right (s, e) -> (s, e)
              -- Try to compile the regex
              mBre = case RE.parseBRE pat of
                Left _ -> Nothing
                Right bre -> Just bre
              -- Smart search/replace mode: pattern and replacement are all lowercase, no i flag
              smartMode = isSmartReplaceEligible pat repl (sfInsensitive flags)
              -- Build display lines with match and replacement highlights
              displayLines = buildSubstitutePreviewLines curDoc mBre (Just repl) flags smartMode allLines startLine endLine totalLines
              -- Target range for scroll check (0-based)
              targetStart = startLine - 1
              targetEnd = endLine - 1
              scrollTop = scrollToShowRange targetStart targetEnd (length displayLines) height
          in DisplayZone
            { dzLines = V.fromList displayLines
            , dzScrollTop = scrollTop
            , dzHeight = height
            , dzWidth = width
            , dzTargetRange = Just (targetStart, targetEnd)
            }
    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


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
      let allMatches = if smartMode || sfInsensitive flags
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
  case docRange of
    DocDefault ->
      let curDoc = dlCurrentDoc dl
      in case getDocStateAt curDoc dl of
        Nothing -> emptyDisplayZone width height
        Just docState ->
          let doc = docDocument docState
              curLine = docCurrentLine docState
              totalLines = lineCount doc
              allLines = documentLines doc
              marks = docMarks docState
              -- Resolve source range
              (srcStart, srcEnd) = case lineRange of
                LineDefault -> defaultRangeCurrent curLine totalLines
                _ -> case resolveLineRange curLine totalLines marks allLines lineRange of
                  Left _ -> defaultRangeCurrent curLine totalLines
                  Right r -> r
              -- Resolve target
              mTargetLine = resolveTargetAddr target curDoc curLine dl
              targetLine = fromMaybe 0 mTargetLine
              -- Get source line texts
              srcLines = [indexText allLines (i - 1) | i <- [srcStart..srcEnd]]
              -- Build display using unified edit system
              editMode = if isMove
                then EditMove targetLine srcLines
                else EditTransfer targetLine srcLines
              (displayLines, tgtStart, tgtEnd) = buildEditDisplay editMode curDoc srcStart srcEnd allLines
              scrollTop = scrollToShowRange tgtStart tgtEnd (length displayLines) height
          in DisplayZone
            { dzLines = V.fromList displayLines
            , dzScrollTop = scrollTop
            , dzHeight = height
            , dzWidth = width
            , dzTargetRange = Just (tgtStart, tgtEnd)
            }
    _ ->
      showResolvedRangeWithDefault docRange lineRange defaultRangeCurrent dl paramStack width height


resolveTargetAddr :: TargetAddr -> Int -> Int -> DocumentList -> Maybe Int
resolveTargetAddr target curDoc curLine dl =
  case target of
    LocalTarget addr ->
      case getDocStateAt curDoc dl of
        Nothing -> Nothing
        Just docState ->
          let totalLines = lineCount (docDocument docState)
              marks = docMarks docState
              allLines = documentLines (docDocument docState)
          in case resolveAddr curLine totalLines marks allLines addr of
            Left _ -> Nothing
            Right n -> Just n

    CrossDocTarget _docRange addr ->
      case getDocStateAt curDoc dl of
        Nothing -> Nothing
        Just docState ->
          let totalLines = lineCount (docDocument docState)
              marks = docMarks docState
              allLines = documentLines (docDocument docState)
          in case resolveAddr curLine totalLines marks allLines addr of
            Left _ -> Nothing
            Right n -> Just n

    ParamTarget _ addr ->
      case getDocStateAt curDoc dl of
        Nothing -> Nothing
        Just docState ->
          let totalLines = lineCount (docDocument docState)
              marks = docMarks docState
              allLines = documentLines (docDocument docState)
          in case resolveAddr curLine totalLines marks allLines addr of
            Left _ -> Nothing
            Right n -> Just n


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
      -- & or similar - show document list
      case lineRange of
        LineDefault -> showDocumentList dl width height
        _ -> showDocumentScrolledTo 1 lineRange dl width height

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
      -- Document range - show document list or first document with lines
      let curDoc = dlCurrentDoc dl
          totalDocs = documentCount dl
          filenames = documentLines (docDocument (dlDocListState dl))
          marks = docMarks (dlDocListState dl)
      in case resolveDocRange curDoc totalDocs filenames marks (DocFree addrFrom addrTo) of
        Left _ -> emptyDisplayZone width height
        Right (docStart, docEnd) ->
          case lineRange of
            LineDefault -> showDocumentListRange docStart docEnd dl width height
            _ -> showDocumentScrolledTo docStart lineRange dl width height

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
            _ -> showDocumentScrolledTo docStart lineRange dl width height

    DocParam ->
      -- Parameter document - show param doc with line range highlighting
      showParamDocument lineRange paramStack width height

    DocPrevious ->
      -- Previous range - should have been substituted, show empty
      emptyDisplayZone width height


showDocumentScrolledTo :: Int -> LineRange -> DocumentList -> Int -> Int -> DisplayZone
showDocumentScrolledTo docIdx lineRange dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let doc = docDocument docState
          curLine = docCurrentLine docState
          totalLines = lineCount doc
          allLines = documentLines doc
          marks = docMarks docState
          -- Resolve line range
          (hlStart, hlEnd) = case resolveLineRange curLine totalLines marks allLines lineRange of
            Left _ -> (curLine, curLine)  -- Fallback to current line
            Right r -> r
          -- Create display lines for the entire document
          displayLines = V.fromList
            [ DisplayLine
              { dlLineNum = Just i
              , dlSourceLine = Just i
              , dlDocIdx = docIdx
              , dlText = indexText allLines (i - 1)
              , dlStyle = if i >= hlStart && i <= hlEnd then StyleSelected else StyleNormal
              , dlHighlights = []
              }
            | i <- [1 .. totalLines]
            ]
          -- Scroll to show entire highlighted range if possible
          targetStart = hlStart - 1
          targetEnd = hlEnd - 1
          scrollTop = scrollToShowRange targetStart targetEnd totalLines height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (targetStart, targetEnd)
        }


showEntireDocument :: Int -> DocumentList -> Int -> Int -> DisplayZone
showEntireDocument docIdx dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let doc = docDocument docState
          curLine = docCurrentLine docState
          totalLines = lineCount doc
          allLines = documentLines doc
          -- Create display lines for the entire document
          displayLines = V.fromList
            [ DisplayLine
              { dlLineNum = Just i
              , dlSourceLine = Just i
              , dlDocIdx = docIdx
              , dlText = indexText allLines (i - 1)
              , dlStyle = if i == curLine then StyleSelected else StyleNormal
              , dlHighlights = []
              }
            | i <- [1 .. totalLines]
            ]
          -- Center on current line
          targetLine = curLine - 1
          scrollTop = centerOnLine targetLine totalLines height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (targetLine, targetLine)
        }


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
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
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
      -- All documents - show document list
      showDocumentList dl width height

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




-- Shows the ENTIRE document with the specified range highlighted.
showLineRangeInDocWithDefault :: Int -> LineRange -> (Int -> Int -> (Int, Int)) -> DocumentList -> Int -> Int -> DisplayZone
showLineRangeInDocWithDefault docIdx lineRange defaultRangeFn dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let doc = docDocument docState
          curLine = docCurrentLine docState
          totalLines = lineCount doc
          allLines = documentLines doc
          marks = docMarks docState
          -- Apply default range when LineDefault
          effectiveRange = case lineRange of
            LineDefault ->
              let (s, e) = defaultRangeFn curLine totalLines
              in LineFree (Number s) (Number e)
            _ -> lineRange
      in case resolveLineRange curLine totalLines marks allLines effectiveRange of
        Left _ -> emptyDisplayZone width height  -- Show empty on resolution error
        Right (lineStart, lineEnd) ->
          -- Show entire document with the range highlighted
          let displayLines = V.fromList
                [ DisplayLine
                  { dlLineNum = Just i
                  , dlSourceLine = Just i
                  , dlDocIdx = docIdx
                  , dlText = indexText allLines (i - 1)
                  , dlStyle = if i >= lineStart && i <= lineEnd then StyleSelected else StyleNormal
                  , dlHighlights = []
                  }
                | i <- [1 .. totalLines]
                ]
              -- Scroll to show entire range if possible, otherwise first line at top
              targetStart = lineStart - 1
              targetEnd = lineEnd - 1
              scrollTop = scrollToShowRange targetStart targetEnd totalLines height
          in DisplayZone
            { dzLines = displayLines
            , dzScrollTop = scrollTop
            , dzHeight = height
            , dzWidth = width
            , dzTargetRange = Just (targetStart, targetEnd)
            }




showLineRange :: Int -> Int -> Int -> DocumentList -> Int -> Int -> DisplayZone
showLineRange docIdx lineStart lineEnd dl width height =
  case getDocStateAt docIdx dl of
    Nothing -> emptyDisplayZone width height
    Just docState ->
      let doc = docDocument docState
          allLines = documentLines doc
          -- Only include lines within the range
          rangeLineCount = lineEnd - lineStart + 1
          displayLines = V.fromList
            [ makeDisplayLine docIdx i (indexText allLines (i - 1)) StyleSelected
            | i <- [lineStart .. lineEnd]
            ]
          -- Start at top since we're only showing the range
          scrollTop = centerOnLine (rangeLineCount `div` 2) rangeLineCount height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (0, rangeLineCount - 1)  -- All displayed lines are the target
        }


showDocRangeWithDefault :: Int -> Int -> LineRange -> (Int -> Int -> (Int, Int)) -> DocumentList -> Int -> Int -> DisplayZone
showDocRangeWithDefault docStart docEnd lineRange defaultRangeFn dl width height =
  -- Build display lines for all documents in range
  let allDisplayLines = concatMap (docDisplayLines dl lineRange defaultRangeFn) [docStart..docEnd]
      displayLines = V.fromList allDisplayLines
      scrollTop = 0  -- Start at top for multi-doc view
  in DisplayZone
    { dzLines = displayLines
    , dzScrollTop = scrollTop
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
              totalLines = lineCount doc
              curLine = docCurrentLine docState
              filenames = documentLines (docDocument (dlDocListState docList))
              filename = indexText filenames (idx - 1)
              marks = docMarks docState
              -- Apply default range when LineDefault
              effectiveRange = case lRange of
                LineDefault ->
                  let (s, e) = defFn curLine totalLines
                  in LineFree (Number s) (Number e)
                _ -> lRange
              -- Resolve the line range for this document
              (lineStart, lineEnd) = case resolveLineRange curLine totalLines marks allLines effectiveRange of
                Left _ -> defFn curLine totalLines  -- Use default on error
                Right r -> r
              -- Header line
              headerLine = DisplayLine
                { dlLineNum = Nothing
                , dlSourceLine = Nothing  -- Headers have no source line
                , dlDocIdx = idx
                , dlText = "=== " <> filename <> " ==="
                , dlStyle = StyleHeader
                , dlHighlights = []
                }
              -- Content lines (only the lines in the range)
              contentLines =
                [ makeDisplayLine idx i (indexText allLines (i - 1)) StyleSelected
                | i <- [lineStart .. lineEnd]
                ]
          in headerLine : contentLines




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
          -- Create display lines
          displayLines = V.fromList
            [ makeDisplayLine curDoc (i + 1) (indexText allLines i) $
                if i + 1 == curLine then StyleSelected else StyleNormal
            | i <- [0 .. totalLines - 1]
            ]
          -- Center on current line
          targetLine = curLine - 1
          scrollTop = centerOnLine targetLine totalLines height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (targetLine, targetLine)
        }


-- Markers: > in gutter = current document, * suffix = modified
-- Format matches &,n command output: "lineNum\t>filename*"
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


-- | Show document list with only modified documents highlighted.
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


-- | Show a parameter document with optional line range highlighting.
showParamDocument :: LineRange -> [(Text, DocumentState)] -> Int -> Int -> DisplayZone
showParamDocument lineRange paramStack width height =
  case paramStack of
    [] -> emptyDisplayZone width height
    ((_, docState):_) ->
      let doc = docDocument docState
          curLine = docCurrentLine docState
          totalLines = lineCount doc
          allLines = documentLines doc
          marks = docMarks docState
          -- Resolve line range
          (hlStart, hlEnd) = case lineRange of
            LineDefault -> (curLine, curLine)
            _ -> case resolveLineRange curLine totalLines marks allLines lineRange of
              Left _ -> (curLine, curLine)
              Right r -> r
          -- Create display lines
          displayLines = V.fromList
            [ DisplayLine
              { dlLineNum = Just i
              , dlSourceLine = Just i
              , dlDocIdx = -1  -- Negative to indicate param doc
              , dlText = indexText allLines (i - 1)
              , dlStyle = if i >= hlStart && i <= hlEnd then StyleSelected else StyleNormal
              , dlHighlights = []
              }
            | i <- [1 .. totalLines]
            ]
          targetStart = hlStart - 1
          targetEnd = hlEnd - 1
          scrollTop = scrollToShowRange targetStart targetEnd totalLines height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (targetStart, targetEnd)
        }


-- | Show a parameter document with default range function.
showParamDocumentWithDefault :: LineRange -> (Int -> Int -> (Int, Int)) -> ParamStack -> Int -> Int -> DisplayZone
showParamDocumentWithDefault lineRange defaultRangeFn paramStack width height =
  case paramStack of
    [] -> emptyDisplayZone width height
    ((_, docState):_) ->
      let doc = docDocument docState
          curLine = docCurrentLine docState
          totalLines = lineCount doc
          allLines = documentLines doc
          marks = docMarks docState
          -- Apply default range when LineDefault
          effectiveRange = case lineRange of
            LineDefault ->
              let (s, e) = defaultRangeFn curLine totalLines
              in LineFree (Number s) (Number e)
            _ -> lineRange
          -- Resolve line range
          (hlStart, hlEnd) = case resolveLineRange curLine totalLines marks allLines effectiveRange of
            Left _ -> (curLine, curLine)
            Right r -> r
          -- Create display lines
          displayLines = V.fromList
            [ DisplayLine
              { dlLineNum = Just i
              , dlSourceLine = Just i
              , dlDocIdx = -1  -- Negative to indicate param doc
              , dlText = indexText allLines (i - 1)
              , dlStyle = if i >= hlStart && i <= hlEnd then StyleSelected else StyleNormal
              , dlHighlights = []
              }
            | i <- [1 .. totalLines]
            ]
          targetStart = hlStart - 1
          targetEnd = hlEnd - 1
          scrollTop = scrollToShowRange targetStart targetEnd totalLines height
      in DisplayZone
        { dzLines = displayLines
        , dzScrollTop = scrollTop
        , dzHeight = height
        , dzWidth = width
        , dzTargetRange = Just (targetStart, targetEnd)
        }


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
