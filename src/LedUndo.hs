module LedUndo
  ( -- * Creation
    newUndoManager
    -- * Capturing state
  , capturePreState
  , commitUndoStep
  , discardPreState
    -- * Undo/Redo operations
  , performUndo
  , performRedo
  , clearRedoStack
    -- * Affected ranges
  , getAffectedRanges
  , getUndoAffectedRanges
  , AffectedRange(..)
  ) where

import LedCore (UndoManager(..), UndoStep(..), DocDiff(..), capturePreStateFrom, buildUndoStep, mkRedoStep, applyDocDiffsUndo, applyDocDiffsRedo)
import LedNexus (DocumentList)
import qualified LedDocument

-- | An affected range in a document after a command.
data AffectedRange = AffectedRange
  { arDocIdx   :: !Int  -- ^ Document index (1-based, 0 for doc list)
  , arStart    :: !Int  -- ^ Start line (1-based, inclusive)
  , arEnd      :: !Int  -- ^ End line (1-based, inclusive)
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Creation
-- ---------------------------------------------------------------------------

-- | Create a new undo manager.
newUndoManager :: IO UndoManager
newUndoManager = UndoManager
  <$> newIORef []
  <*> newIORef []
  <*> newIORef Nothing

-- ---------------------------------------------------------------------------
-- Capturing state
-- ---------------------------------------------------------------------------

-- | Capture pre-command state from the document list.
capturePreState :: UndoManager -> DocumentList -> IO ()
capturePreState mgr dl = writeIORef (umPreState mgr) (Just (capturePreStateFrom dl))

-- | Commit the undo step by comparing pre-state to current state.
-- Returns the UndoStep if there were changes, Nothing otherwise.
commitUndoStep :: UndoManager -> DocumentList -> IO (Maybe UndoStep)
commitUndoStep mgr dl = do
  mPre <- readIORef (umPreState mgr)
  writeIORef (umPreState mgr) Nothing
  case mPre of
    Nothing -> pure Nothing
    Just pre -> case buildUndoStep pre dl of
      Nothing -> pure Nothing
      Just step -> do
        modifyIORef' (umUndoStack mgr) (step :)
        pure (Just step)

-- | Discard the pre-state without committing.
discardPreState :: UndoManager -> IO ()
discardPreState mgr = writeIORef (umPreState mgr) Nothing

-- ---------------------------------------------------------------------------
-- Undo/Redo operations
-- ---------------------------------------------------------------------------

-- | Perform undo: pop from undo stack, apply reverse diffs, push to redo stack.
performUndo :: UndoManager -> DocumentList -> IO (Either Text (UndoStep, DocumentList))
performUndo mgr dl = do
  stack <- readIORef (umUndoStack mgr)
  case stack of
    [] -> pure (Left "Nothing to undo")
    (step:rest) -> do
      writeIORef (umUndoStack mgr) rest
      let redoStep = mkRedoStep step dl
      modifyIORef' (umRedoStack mgr) (redoStep :)
      let dl' = applyDocDiffsUndo step dl
      pure (Right (step, dl'))

-- | Perform redo: pop from redo stack, apply forward diffs, push to undo stack.
performRedo :: UndoManager -> DocumentList -> IO (Either Text (UndoStep, DocumentList))
performRedo mgr dl = do
  stack <- readIORef (umRedoStack mgr)
  case stack of
    [] -> pure (Left "Nothing to redo")
    (step:rest) -> do
      writeIORef (umRedoStack mgr) rest
      let undoStep = mkRedoStep step dl
      modifyIORef' (umUndoStack mgr) (undoStep :)
      let dl' = applyDocDiffsRedo step dl
      pure (Right (step, dl'))

-- | Clear the redo stack (called after any modifying command).
clearRedoStack :: UndoManager -> IO ()
clearRedoStack mgr = writeIORef (umRedoStack mgr) []

-- ---------------------------------------------------------------------------
-- Affected ranges
-- ---------------------------------------------------------------------------

-- | Get the affected ranges from an undo step (for normal commands).
-- Returns the line ranges that were modified by the command (the new lines).
-- Only includes actual document changes (docIdx > 0), not doc list changes.
getAffectedRanges :: UndoStep -> [AffectedRange]
getAffectedRanges step =
  [ range
  | diff <- stepDiffs step
  , diffDocIdx diff > 0  -- Skip doc list (index 0)
  , range <- diffToRanges (diffDocIdx diff) (diffForward diff)
  ]
  where
    -- Convert a list of LineDiffs to affected ranges
    -- LineDiff: ldStart is where change starts, ldNewLines are the inserted lines
    diffToRanges :: Int -> [LedDocument.LineDiff] -> [AffectedRange]
    diffToRanges docIdx diffs =
      [ AffectedRange docIdx (LedDocument.ldStart ld) endLine
      | ld <- diffs
      , let newCount = length (LedDocument.ldNewLines ld)
      , newCount > 0  -- Only if lines were added/changed
      , let endLine = LedDocument.ldStart ld + newCount - 1
      ]

-- | Get the affected ranges for an undo operation.
-- Returns the line ranges that were restored by the undo (using reverse diffs).
-- For insertions (lines restored), highlights the restored lines.
-- For deletions (undoing an append), highlights the line at the deletion point.
getUndoAffectedRanges :: UndoStep -> [AffectedRange]
getUndoAffectedRanges step =
  [ range
  | diff <- stepDiffs step
  , diffDocIdx diff > 0  -- Skip doc list (index 0)
  , range <- diffToRanges (diffDocIdx diff) (diffReverse diff)
  ]
  where
    diffToRanges :: Int -> [LedDocument.LineDiff] -> [AffectedRange]
    diffToRanges docIdx diffs =
      [ AffectedRange docIdx startLine endLine
      | ld <- diffs
      , let newCount = length (LedDocument.ldNewLines ld)
            startLine = LedDocument.ldStart ld
            -- For insertions: highlight the inserted lines
            -- For deletions: highlight the line before deletion point (or line 1)
            endLine = if newCount > 0
                      then startLine + newCount - 1
                      else max 1 (startLine - 1)
      ]
