{-# LANGUAGE StrictData #-}
module LedDocument
  ( Document(..)
  , emptyDocument
  , fromText
  , fromRope
  , fromLines
  , documentText
  , documentLines
  , documentRope
  , lineCount
  , getLineAt
  , getLines
  , getLinesAsRope
  , replaceLines
  , appendAfter
  , appendRopeAfter
  , deleteLines
  , deleteLinesBatch
  , joinLines
  , mapLinesInRange
  , LineDiff(..)
  , computeLineDiffs
  , applyLineDiffs
  ) where

import Data.Hashable (hash)
import Data.Text.Rope (Rope)
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import qualified Data.Text.Rope as Rope

data Document = Document
  { docRope       :: !Rope
  , docLineCount  :: !Int   -- ^ Cached line count for O(1) access
  , docHash       :: !Int   -- ^ Content hash for O(1) equality check
  } deriving stock (Show)

instance Eq Document where
  d1 == d2
    | docHash d1 /= docHash d2 = False      -- Different hash -> definitely different
    | docLineCount d1 /= docLineCount d2 = False  -- Same hash, different count -> different
    | otherwise = docRope d1 == docRope d2  -- Same hash & count -> compare content

emptyDocument :: Document
emptyDocument = Document mempty 0 0

fromText :: Text -> Document
fromText t =
  let rope = Rope.fromText t
  in Document rope (fromIntegral (Rope.lengthInLines rope)) (hash t)

-- Useful for cross-document operations to avoid intermediate text conversion.
fromRope :: Rope -> Document
fromRope rope =
  let t = Rope.toText rope
  in Document rope (fromIntegral (Rope.lengthInLines rope)) (hash t)

fromLines :: [Text] -> Document
fromLines [] = emptyDocument
fromLines lns = fromText (T.unlines lns)

documentText :: Document -> Text
documentText = Rope.toText . docRope

-- Prefer 'getLines' for accessing specific ranges.
documentLines :: Document -> [Text]
documentLines doc
  | docLineCount doc == 0 = []
  | otherwise = Rope.lines (docRope doc)

-- Useful for cross-document operations.
documentRope :: Document -> Rope
documentRope = docRope

lineCount :: Document -> Int
lineCount = docLineCount

-- Returns empty text if index is out of bounds.
getLineAt :: Int -> Document -> Text
getLineAt n doc
  | n < 1 || n > docLineCount doc = ""
  | otherwise = Rope.toText (Rope.getLine (fromIntegral (n - 1)) (docRope doc))

-- where k is the number of lines retrieved.
getLines :: Int -> Int -> Document -> [Text]
getLines start end doc
  | start > end = []
  | start < 1 = getLines 1 end doc
  | end > docLineCount doc = getLines start (docLineCount doc) doc
  | otherwise =
      -- Split at start-1 to get lines from 'start' onwards
      -- Then split at (end - start + 1) to limit the range
      let (_, fromStart) = splitAtLineRope (start - 1) (docRope doc)
          count = end - start + 1
          (range, _) = splitAtLineRope count fromStart
      in Rope.lines range

getLinesAsRope :: Int -> Int -> Document -> Rope
getLinesAsRope start end doc
  | start > end = mempty
  | start < 1 = getLinesAsRope 1 end doc
  | end > docLineCount doc = getLinesAsRope start (docLineCount doc) doc
  | otherwise =
      let (_, fromStart) = splitAtLineRope (start - 1) (docRope doc)
          count = end - start + 1
          (range, _) = splitAtLineRope count fromStart
      in range

splitAtLineRope :: Int -> Rope -> (Rope, Rope)
splitAtLineRope n rope = Rope.splitAtLine (fromIntegral n) rope

-- Line numbers are 1-based.
replaceLines :: Int -> Int -> [Text] -> Document -> Document
replaceLines start end newLines doc
  | docLineCount doc == 0 && null newLines = emptyDocument
  | docLineCount doc == 0 = fromLines newLines
  | otherwise =
      let (before, rest) = splitAtLineRope (start - 1) (docRope doc)
          (_, after) = splitAtLineRope (end - start + 1) rest
          middle = if null newLines
                   then mempty
                   else Rope.fromText (T.unlines newLines)
          newRope = before <> middle <> after
      in fromRope newRope

appendAfter :: Int -> [Text] -> Document -> Document
appendAfter nth newLines doc
  | null newLines = doc
  | nth <= 0 = replaceLines 1 0 newLines doc
  | nth >= docLineCount doc =
      -- Append at end - just concatenate
      let addition = Rope.fromText (T.unlines newLines)
          -- Ensure there's a newline before appending if document is non-empty
          newRope = if docLineCount doc == 0
                    then addition
                    else docRope doc <> addition
      in fromRope newRope
  | otherwise = replaceLines (nth + 1) nth newLines doc

-- Efficient for cross-document transfers - avoids text conversion.
appendRopeAfter :: Int -> Rope -> Document -> Document
appendRopeAfter nth rope doc
  | Rope.null rope = doc
  | nth <= 0 =
      let newRope = rope <> docRope doc
      in fromRope newRope
  | nth >= docLineCount doc =
      let newRope = docRope doc <> rope
      in fromRope newRope
  | otherwise =
      let (before, after) = splitAtLineRope nth (docRope doc)
          newRope = before <> rope <> after
      in fromRope newRope

deleteLines :: Int -> Int -> Document -> Document
deleteLines start end doc = replaceLines start end [] doc

joinLines :: Int -> Int -> Document -> Document
joinLines start end doc
  | start > end || start < 1 || end > docLineCount doc = doc
  | start == end = doc  -- Single line, nothing to join
  | otherwise =
      let lns = getLines start end doc
          joined = T.concat lns
      in replaceLines start end [joined] doc

-- ---------------------------------------------------------------------------
-- Batch operations for global commands
-- ---------------------------------------------------------------------------

-- Lines to delete should be a list of 1-based line numbers.
deleteLinesBatch :: [Int] -> Document -> Document
deleteLinesBatch [] doc = doc
deleteLinesBatch linesToDelete doc =
  let deleteSet = IntSet.fromList linesToDelete
      lns = documentLines doc
      kept = [ line | (i, line) <- zip [1..] lns, not (IntSet.member i deleteSet) ]
  in fromLines kept

-- The function receives (lineNumber, lineContent) and returns Maybe newContent.
-- Nothing means delete the line, Just content means replace it.
-- Returns (newDocument, lastModifiedLine, anyChanged).
-- This is O(n) but processes all lines in a single pass.
mapLinesInRange :: Int -> Int -> (Int -> Text -> Maybe Text) -> Document -> (Document, Int, Bool)
mapLinesInRange start end f doc
  | start > end || start < 1 = (doc, 0, False)
  | otherwise =
      let lns = documentLines doc
          total = docLineCount doc
          actualEnd = min end total
          -- Process lines: before range, in range (mapped), after range
          beforeLines = take (start - 1) lns
          rangeLines = take (actualEnd - start + 1) (drop (start - 1) lns)
          afterLines = drop actualEnd lns
          -- Map the function over range lines, tracking changes
          (mappedLines, lastMod, changed) = processLines start rangeLines
          newLines = beforeLines ++ mappedLines ++ afterLines
      in (fromLines newLines, lastMod, changed)
  where
    processLines _ [] = ([], 0, False)
    processLines lineNum (l:ls) =
      let result = f lineNum l
          (restLines, restLast, restChanged) = processLines (lineNum + 1) ls
      in case result of
        Nothing ->
          -- Line deleted
          (restLines, if restLast == 0 then lineNum else restLast, True)
        Just newL ->
          let thisChanged = newL /= l
              newLast = if thisChanged then lineNum else restLast
          in (newL : restLines, if restLast == 0 then newLast else restLast, thisChanged || restChanged)

-- Represents replacing lines [start, end) with new lines.
-- Line numbers are 1-based, end is exclusive.
data LineDiff = LineDiff
  { ldStart    :: !Int      -- ^ Start line (1-based, inclusive)
  , ldEnd      :: !Int      -- ^ End line (1-based, exclusive)
  , ldNewLines :: ![Text]   -- ^ Lines to insert
  } deriving stock (Eq, Show)

-- Uses a simple approach: find common prefix, common suffix, replace middle.
computeLineDiffs :: [Text] -> [Text] -> [LineDiff]
computeLineDiffs src tgt
  | src == tgt = []
  | otherwise =
      let prefix = length (takeWhile id (zipWith (==) src tgt))
          srcRev = reverse (drop prefix src)
          tgtRev = reverse (drop prefix tgt)
          suffix = length (takeWhile id (zipWith (==) srcRev tgtRev))
          srcMiddle = take (length src - prefix - suffix) (drop prefix src)
          tgtMiddle = take (length tgt - prefix - suffix) (drop prefix tgt)
          start = prefix + 1  -- 1-based
          end = prefix + length srcMiddle + 1  -- exclusive end
      in if null srcMiddle && null tgtMiddle
         then []
         else [LineDiff start end tgtMiddle]

-- Diffs are applied in order; each diff's line numbers are absolute.
applyLineDiffs :: [LineDiff] -> [Text] -> [Text]
applyLineDiffs diffs lns = foldl' applyOneDiff lns diffs
  where
    applyOneDiff :: [Text] -> LineDiff -> [Text]
    applyOneDiff ls (LineDiff start end newLines) =
      let before = take (start - 1) ls
          after = drop (end - 1) ls
      in before ++ newLines ++ after
