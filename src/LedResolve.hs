{-# LANGUAGE StrictData #-}
-- | Address resolution for led.
-- This module handles resolving symbolic addresses (like '.', '$', marks, searches)
-- to concrete line or document numbers.
module LedResolve
  ( -- * Line address resolution
    resolveAddr
  , resolveLineRange
  , resolveLineAddr
    -- * Document address resolution
  , resolveDocAddr
  , resolveDocRange
    -- * Search functions
  , searchForward
  , searchBackward
  ) where

import Data.List ((!?))
import qualified Data.Map.Strict as Map

import LedParse (Addr(..), DocRange(..), LineRange(..))
import qualified LedRegularExpressions as RE

-- ---------------------------------------------------------------------------
-- Line address resolution
-- ---------------------------------------------------------------------------

-- | Resolve a single address to a concrete line number.
-- Takes current line, total line count, marks map, and document lines.
-- Result is bounds-checked to [0 .. lineCount].
resolveAddr :: Int -> Int -> Map.Map Char Int -> [Text] -> Addr -> Either Text Int
resolveAddr cur total marks docLines addr = case addr of
  Current -> Right cur
  LastLine -> Right total
  Number n -> boundsCheck n
  Mark c -> case Map.lookup c marks of
    Nothing -> Left "Mark not set"
    Just n | n < 1 || n > total -> Left "Mark references deleted line"
    Just n -> Right n
  Next reText -> searchForward reText cur total docLines
  Prev reText -> searchBackward reText cur total docLines
  AddrOffset base off -> do
    baseVal <- resolveAddr cur total marks docLines base
    boundsCheck (baseVal + off)
  where
    boundsCheck n
      | n < 0 || n > total = Left "Invalid address"
      | otherwise = Right n

-- | Resolve a line range to (start, end) line numbers.
-- Semicolon ranges set current line to the first address before resolving second.
resolveLineRange :: Int -> Int -> Map.Map Char Int -> [Text] -> LineRange -> Either Text (Int, Int)
resolveLineRange cur total marks docLines = \case
  LineDefault -> Left "Invalid address"  -- No range provided
  LinePrevious -> Left "No previous line range"  -- Must be substituted before calling
  LineSingle addr -> do
    n <- resolveAddr cur total marks docLines addr
    when (n < 1) (Left "Invalid address")
    pure (n, n)
  LineFree from to -> do
    s <- resolveAddr cur total marks docLines from
    when (s < 1) (Left "Invalid address")
    e <- resolveAddr cur total marks docLines to
    when (e < 1) (Left "Invalid address")
    when (s > e) (Left "Invalid address")
    pure (s, e)
  LineBound from to -> do
    s <- resolveAddr cur total marks docLines from
    when (s < 1) (Left "Invalid address")
    -- For semicolon, current line becomes s before resolving second address
    e <- resolveAddr s total marks docLines to
    when (e < 1) (Left "Invalid address")
    when (s > e) (Left "Invalid address")
    pure (s, e)

-- | Resolve a single line address (for commands that take a single address).
-- allowZero permits address 0 (used for append).
resolveLineAddr :: Int -> Int -> Map.Map Char Int -> [Text] -> Bool -> LineRange -> Either Text Int
resolveLineAddr cur total marks docLines allowZero = \case
  LineDefault -> Left "Invalid address"
  LinePrevious -> Left "No previous line range"  -- Must be substituted before calling
  LineSingle addr -> do
    n <- resolveAddr cur total marks docLines addr
    when (not allowZero && n < 1) (Left "Invalid address")
    when (n < 0) (Left "Invalid address")
    pure n
  LineFree {} -> Left "Invalid address"
  LineBound {} -> Left "Invalid address"

-- ---------------------------------------------------------------------------
-- Document address resolution
-- ---------------------------------------------------------------------------

-- | Resolve a document address to a concrete document number.
resolveDocAddr :: Int -> Int -> [Text] -> Map.Map Char Int -> Addr -> Either Text Int
resolveDocAddr cur total docFilenames marks addr = case addr of
  Current -> Right cur
  LastLine -> Right total
  Number n -> boundsCheck n
  Mark c -> case Map.lookup c marks of
    Nothing -> Left "Mark not set"
    Just n | n < 1 || n > total -> Left "Mark references deleted line"
    Just n -> Right n
  Next reText -> searchForward reText cur total docFilenames
  Prev reText -> searchBackward reText cur total docFilenames
  AddrOffset base off -> do
    baseVal <- resolveDocAddr cur total docFilenames marks base
    boundsCheck (baseVal + off)
  where
    boundsCheck n
      | n < 0 || n > total = Left "Invalid address"
      | otherwise = Right n

-- | Resolve a document range to (start, end) document numbers.
resolveDocRange :: Int -> Int -> [Text] -> Map.Map Char Int -> DocRange -> Either Text (Int, Int)
resolveDocRange cur total docFilenames marks = \case
  DocDefault -> Left "Invalid address"
  DocPrevious -> Left "No previous document range"  -- Must be substituted before calling
  DocSingle addr -> do
    n <- resolveDocAddr cur total docFilenames marks addr
    when (n < 1) (Left "Invalid address")
    pure (n, n)
  DocFree from to -> do
    s <- resolveDocAddr cur total docFilenames marks from
    when (s < 1) (Left "Invalid address")
    e <- resolveDocAddr cur total docFilenames marks to
    when (e < 1) (Left "Invalid address")
    when (s > e) (Left "Invalid address")
    pure (s, e)
  DocBound from to -> do
    s <- resolveDocAddr cur total docFilenames marks from
    when (s < 1) (Left "Invalid address")
    e <- resolveDocAddr s total docFilenames marks to
    when (e < 1) (Left "Invalid address")
    when (s > e) (Left "Invalid address")
    pure (s, e)
  DocAll -> if total == 0 then Right (0, 0) else Right (1, total)
  DocManage -> if total == 0 then Right (0, 0) else Right (1, total)
  DocModified -> Left "Invalid document range"  -- Modified is not a contiguous range
  DocParam -> Left "Invalid document range"  -- Param is not a doc range

-- ---------------------------------------------------------------------------
-- Search functions
-- ---------------------------------------------------------------------------

-- | Forward search: search from cur+1 wrapping around to cur.
searchForward :: Text -> Int -> Int -> [Text] -> Either Text Int
searchForward = searchWith (\c k t -> (c + k - 1) `mod` t + 1)

-- | Backward search: search from cur-1 wrapping around to cur.
searchBackward :: Text -> Int -> Int -> [Text] -> Either Text Int
searchBackward = searchWith (\c k t -> (c - k - 1 + t) `mod` t + 1)

-- | Generic search with index function.
searchWith :: (Int -> Int -> Int -> Int) -> Text -> Int -> Int -> [Text] -> Either Text Int
searchWith idxFn reText cur total items
  | total == 0 = Left "No match"
  | otherwise = case RE.parseBRE reText of
      Left err -> Left err
      Right bre ->
        let idx k = idxFn cur k total
            matchAt k = case items !? (idx k - 1) of
              Just item -> RE.matchesBRE bre item
              Nothing -> False
        in case find matchAt [1..total] of
          Just k -> Right (idx k)
          Nothing -> Left "No match"
