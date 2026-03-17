module LedVi.Render
  ( -- * Color Scheme / Attributes
    displayZoneBg
  , normalAttr
  , lineNumAttr
  , headerAttr
  , addedAttr
  , deletedAttr
  , matchAttr
  , replacementAttr
  , errorAttr
  , inputAttr
  , styleToAttr
  , highlightToAttr

    -- * Core Rendering
  , renderToVty
  , renderDisplayZone
  , renderLine
  , renderInputLine

    -- * Wrapping
  , wrapDisplayZone
  , gutterWidth
  , wrapDisplayLine

    -- * Utilities
  , expandTabs
  ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty

import LedVi.Types


-- ---------------------------------------------------------------------------
-- Color Scheme
-- ---------------------------------------------------------------------------

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


-- ---------------------------------------------------------------------------
-- Core Rendering
-- ---------------------------------------------------------------------------

renderToVty :: Vty.Vty -> DisplayZone -> Text -> Bool -> IO ()
renderToVty vty dz inputTxt isError = do
  let displayImage = renderDisplayZone dz
      inputImage = renderInputLine inputTxt isError (dzWidth dz)
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


highlightToAttr :: HighlightStyle -> Vty.Attr
highlightToAttr HLMatch       = matchAttr
highlightToAttr HLReplacement = replacementAttr
highlightToAttr HLSelection   = normalAttr `Vty.withStyle` Vty.reverseVideo


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


emptyLineImage :: Int -> Vty.Image
emptyLineImage width =
  let contentWidth = width - gutterWidth
      gutterImg = Vty.string lineNumAttr (replicate gutterWidth ' ')
      contentImg = Vty.string normalAttr (replicate contentWidth ' ')
  in gutterImg Vty.<|> contentImg


renderInputLine :: Text -> Bool -> Int -> Vty.Image
renderInputLine inputTxt isError width =
  let attr = if isError then errorAttr else inputAttr
      content = T.unpack inputTxt
      padded = content ++ replicate (width - length content) ' '
  in Vty.string attr (take width padded)


rightPad :: Int -> String -> String
rightPad n s = s ++ replicate (n - length s) ' '


gutterWidth :: Int
gutterWidth = 8


-- ---------------------------------------------------------------------------
-- Wrapping
-- ---------------------------------------------------------------------------

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


-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

expandTabs :: String -> String
expandTabs = go 0
  where
    go _ [] = []
    go col ('\t':rest) =
      let spaces = 8 - (col `mod` 8)
      in replicate spaces ' ' ++ go (col + spaces) rest
    go col (c:rest) = c : go (col + 1) rest
