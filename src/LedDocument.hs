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
  , replaceLinesRope
  , LineDiff(..)
  , computeLineDiffs
  , applyLineDiffs
  ) where

import Data.Hashable (hash)
import Data.Text.Rope (Rope)
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

replaceLines :: Int -> Int -> [Text] -> Document -> Document
replaceLines index count newLines doc =
  let newContent = if null newLines
                   then mempty
                   else Rope.fromText (T.unlines newLines)
  in replaceLinesRope index count newContent doc

replaceLinesRope :: Int -> Int -> Rope -> Document -> Document
replaceLinesRope index count newContent doc
  | docLineCount doc == 0 && Rope.null newContent = emptyDocument
  | docLineCount doc == 0 = fromRope newContent
  | otherwise =
      let safeIndex = max 1 index
          safeCount = max 0 count
          (before, rest) = splitAtLineRope (safeIndex - 1) (docRope doc)
          (_, after) = splitAtLineRope safeCount rest
          newRope = before <> newContent <> after
      in fromRope newRope

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
