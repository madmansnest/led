{-# LANGUAGE StrictData #-}
module LedCore
  ( LedState(..)
  , initialState
  , UndoManager(..)
  , DocDiff(..)
  , UndoStep(..)
  , PreState(..)
  , capturePreStateFrom
  , buildUndoStep
  , mkDocDiff
  , mkRedoStep
  , applyDocDiffsUndo
  , applyDocDiffsRedo
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.List ((!?))

import LedDocument (Document, LineDiff(..), documentLines, fromLines, lineCount, computeLineDiffs, applyLineDiffs)
import LedNexus (BufferChangeFlag(..), DocumentState(..), DocumentList(..), dlDocuments, emptyDocumentState, emptyDocumentList, documentCount, getDocStateAt, setDocStateAt, dlCurrentDoc, dlDocListState, setDlCurrentDoc)
import LedParse (DocRange, LineRange)
import qualified LedRegularExpressions as RE

data UndoManager = UndoManager
  { umUndoStack   :: !(IORef [UndoStep])
  , umRedoStack   :: !(IORef [UndoStep])
  , umPreState    :: !(IORef (Maybe PreState))
  }

data DocDiff = DocDiff
  { diffDocKey  :: !Text       -- ^ Document identifier (filename or "&" for doc list)
  , diffDocIdx  :: !Int        -- ^ Document index (1-based, 0 for doc list)
  , diffReverse :: ![LineDiff] -- ^ Diffs to apply for undo
  , diffForward :: ![LineDiff] -- ^ Diffs to apply for redo
  } deriving stock (Eq, Show)

data UndoStep = UndoStep
  { stepDiffs       :: ![DocDiff]       -- ^ Changes to documents
  , stepActiveDoc   :: !Int             -- ^ Which doc was active before
  , stepCurLines    :: !(Map Int Int)   -- ^ Doc index -> cursor position before
  , stepMarks       :: !(Map Int (Map Char Int))  -- ^ Doc index -> marks before
  , stepDlCurLine   :: !Int             -- ^ Doc list cursor before
  , stepDlMarks     :: !(Map Char Int)  -- ^ Doc list marks before
  , stepDeletedDocs :: ![(Int, DocumentState)]  -- ^ (pre-index, state) for deleted docs
  , stepPreDocCount :: !Int             -- ^ Document count before the operation
  } deriving stock (Eq, Show)

data PreState = PreState
  { preDocStates   :: ![(Int, Text, Document)] -- ^ (docIdx, key, document)
  , preDlDoc       :: !Document                -- ^ Doc list document
  , preActiveDoc   :: !Int                     -- ^ Which doc was active
  , preCurLines    :: !(Map Int Int)           -- ^ Doc index -> cursor
  , preMarks       :: !(Map Int (Map Char Int)) -- ^ Doc index -> marks
  , preDlCurLine   :: !Int                     -- ^ Doc list cursor
  , preDlMarks     :: !(Map Char Int)          -- ^ Doc list marks
  } deriving stock (Eq, Show)


capturePreStateFrom :: DocumentList -> PreState
capturePreStateFrom dl =
  let totalDocs = documentCount dl
      activeDoc = dlCurrentDoc dl
      dlState = dlDocListState dl
      dlDoc = docDocument dlState
      dlLines = documentLines dlDoc
      dlCur = docCurrentLine dlState
      dlMarks' = docMarks dlState
      -- Store Document directly, not [Text] - avoids O(n) conversion per doc
      docStates = [ (idx, key, docDocument ds)
                  | idx <- [1..totalDocs]
                  , Just ds <- [getDocStateAt idx dl]
                  , let key = fromMaybe "" (indexList dlLines (idx - 1))
                  ]
      curLines = Map.fromList
        [ (idx, docCurrentLine ds)
        | idx <- [1..totalDocs]
        , Just ds <- [getDocStateAt idx dl]
        ]
      marks' = Map.fromList
        [ (idx, docMarks ds)
        | idx <- [1..totalDocs]
        , Just ds <- [getDocStateAt idx dl]
        ]
  in PreState
       { preDocStates  = docStates
       , preDlDoc      = dlDoc
       , preActiveDoc  = activeDoc
       , preCurLines   = curLines
       , preMarks      = marks'
       , preDlCurLine  = dlCur
       , preDlMarks    = dlMarks'
       }

buildUndoStep :: PreState -> DocumentList -> Maybe UndoStep
buildUndoStep pre dl =
  let totalDocs = documentCount dl
      preTotalDocs = length (preDocStates pre)
      dlState = dlDocListState dl
      dlDocNow = docDocument dlState
      -- Diffs for documents that still exist (may have changed content)
      existingDocDiffs = catMaybes
        [ mkDocDiffFromDocs idx key oldDoc newDoc
        | (idx, key, oldDoc) <- preDocStates pre
        , idx <= totalDocs
        , Just ds <- [getDocStateAt idx dl]
        , let newDoc = docDocument ds
        ]
      -- Diffs for documents that were deleted
      deletedDocDiffs = catMaybes
        [ mkDocDiff idx key (documentLines oldDoc) []
        | (idx, key, oldDoc) <- preDocStates pre
        , idx > totalDocs || isNothing (getDocStateAt idx dl)
        ]
      -- Store full state for deleted documents (to restore on undo)
      deletedDocs =
        [ (idx, reconstructDocState idx oldDoc pre)
        | (idx, _key, oldDoc) <- preDocStates pre
        , idx > totalDocs || isNothing (getDocStateAt idx dl)
        ]
      dlDiff = mkDocDiffFromDocs 0 "&" (preDlDoc pre) dlDocNow
      allDiffs = maybeToList dlDiff ++ existingDocDiffs ++ deletedDocDiffs
  in if null allDiffs
     then Nothing
     else Just UndoStep
            { stepDiffs       = allDiffs
            , stepActiveDoc   = preActiveDoc pre
            , stepCurLines    = preCurLines pre
            , stepMarks       = preMarks pre
            , stepDlCurLine   = preDlCurLine pre
            , stepDlMarks     = preDlMarks pre
            , stepDeletedDocs = deletedDocs
            , stepPreDocCount = preTotalDocs
            }

reconstructDocState :: Int -> Document -> PreState -> DocumentState
reconstructDocState idx doc pre = DocumentState
  { docDocument    = doc
  , docCurrentLine = Map.findWithDefault (lineCount doc) idx (preCurLines pre)
  , docMarks       = Map.findWithDefault Map.empty idx (preMarks pre)
  , docChangeFlag  = Changed  -- Restored docs are considered changed
  }

mkDocDiffFromDocs :: Int -> Text -> Document -> Document -> Maybe DocDiff
mkDocDiffFromDocs idx key oldDoc newDoc
  | oldDoc == newDoc = Nothing  -- Fast equality check, no line conversion
  | otherwise = mkDocDiff idx key (documentLines oldDoc) (documentLines newDoc)

mkDocDiff :: Int -> Text -> [Text] -> [Text] -> Maybe DocDiff
mkDocDiff idx key oldLines newLines
  | oldLines == newLines = Nothing
  | otherwise =
      let reverseDiffs = computeLineDiffs newLines oldLines  -- new -> old for undo
          forwardDiffs = computeLineDiffs oldLines newLines  -- old -> new for redo
      in Just DocDiff
           { diffDocKey  = key
           , diffDocIdx  = idx
           , diffReverse = reverseDiffs
           , diffForward = forwardDiffs
           }

mkRedoStep :: UndoStep -> DocumentList -> UndoStep
mkRedoStep origStep dl =
  let totalDocs = documentCount dl
      activeDoc = dlCurrentDoc dl
      dlState = dlDocListState dl
      curLines = Map.fromList
        [ (idx, docCurrentLine ds)
        | idx <- [1..totalDocs]
        , Just ds <- [getDocStateAt idx dl]
        ]
      marks' = Map.fromList
        [ (idx, docMarks ds)
        | idx <- [1..totalDocs]
        , Just ds <- [getDocStateAt idx dl]
        ]
      -- For redo, we need to know which docs to delete (those that were restored by undo)
      -- These are the docs that exist now but didn't before the original operation
      docsToDeleteOnRedo =
        [ (idx, ds)
        | (idx, _) <- stepDeletedDocs origStep
        , Just ds <- [getDocStateAt idx dl]
        ]
  in UndoStep
       { stepDiffs       = stepDiffs origStep
       , stepActiveDoc   = activeDoc
       , stepCurLines    = curLines
       , stepMarks       = marks'
       , stepDlCurLine   = docCurrentLine dlState
       , stepDlMarks     = docMarks dlState
       , stepDeletedDocs = docsToDeleteOnRedo  -- Docs to delete when redoing
       , stepPreDocCount = totalDocs           -- Current count (pre-redo)
       }

applyDocDiffsUndo :: UndoStep -> DocumentList -> DocumentList
applyDocDiffsUndo step dl0 =
  -- First, apply the doc list diff (index 0) to restore filenames
  let dlDiffs = [d | d <- stepDiffs step, diffDocIdx d == 0]
      dl1 = foldl' applyDocDiffUndo dl0 dlDiffs
      -- Insert deleted documents back into dlDocuments (sorted by index, ascending)
      sortedDeleted = sortOn fst (stepDeletedDocs step)
      dl2 = foldl' insertDocStateAt dl1 sortedDeleted
      -- Now apply content diffs to individual documents (index > 0)
      docDiffs = [d | d <- stepDiffs step, diffDocIdx d > 0]
      dl3 = foldl' applyDocDiffUndo dl2 docDiffs
      -- Restore cursors, marks, and active document
      dl4 = restoreCursorsAndMarks step dl3
      dl5 = setDlCurrentDoc (stepActiveDoc step) dl4
  in dl5

insertDocStateAt :: DocumentList -> (Int, DocumentState) -> DocumentList
insertDocStateAt dl (idx, ds)
  | idx < 1 = dl
  | otherwise =
      let docs = dlDocuments dl
          (before, after) = Seq.splitAt (idx - 1) docs
          newDocs = before Seq.>< Seq.singleton ds Seq.>< after
      in dl { dlDocuments = newDocs }

applyDocDiffsRedo :: UndoStep -> DocumentList -> DocumentList
applyDocDiffsRedo step dl0 =
  -- First, apply the doc list diff (index 0) to update filenames
  let dlDiffs = [d | d <- stepDiffs step, diffDocIdx d == 0]
      dl1 = foldl' applyDocDiffRedo dl0 dlDiffs
      -- Remove documents that were restored by undo (sorted by index, descending to preserve indices)
      sortedToDelete = sortOn (Down . fst) (stepDeletedDocs step)
      dl2 = foldl' removeDocStateAt dl1 sortedToDelete
      -- Now apply content diffs to individual documents (index > 0)
      docDiffs = [d | d <- stepDiffs step, diffDocIdx d > 0]
      dl3 = foldl' applyDocDiffRedo dl2 docDiffs
      -- Restore cursors, marks, and active document
      dl4 = restoreCursorsAndMarks step dl3
      dl5 = setDlCurrentDoc (stepActiveDoc step) dl4
  in dl5

removeDocStateAt :: DocumentList -> (Int, DocumentState) -> DocumentList
removeDocStateAt dl (idx, _)
  | idx < 1 || idx > Seq.length (dlDocuments dl) = dl
  | otherwise =
      let docs = dlDocuments dl
          (before, rest) = Seq.splitAt (idx - 1) docs
          after = Seq.drop 1 rest
          newDocs = before Seq.>< after
      in dl { dlDocuments = newDocs }

applyDocDiffUndo :: DocumentList -> DocDiff -> DocumentList
applyDocDiffUndo dl diff
  | diffDocIdx diff == 0 =
      let dlState = dlDocListState dl
          doc = docDocument dlState
          oldLines = documentLines doc
          newLines = applyLineDiffs (diffReverse diff) oldLines
          newDoc = fromLines newLines
      in dl { dlDocListState = dlState { docDocument = newDoc } }
  | otherwise =
      case getDocStateAt (diffDocIdx diff) dl of
        Nothing -> dl
        Just ds ->
          let oldLines = documentLines (docDocument ds)
              newLines = applyLineDiffs (diffReverse diff) oldLines
              newDoc = fromLines newLines
          in setDocStateAt (diffDocIdx diff) (ds { docDocument = newDoc }) dl

applyDocDiffRedo :: DocumentList -> DocDiff -> DocumentList
applyDocDiffRedo dl diff
  | diffDocIdx diff == 0 =
      let dlState = dlDocListState dl
          doc = docDocument dlState
          oldLines = documentLines doc
          newLines = applyLineDiffs (diffForward diff) oldLines
          newDoc = fromLines newLines
      in dl { dlDocListState = dlState { docDocument = newDoc } }
  | otherwise =
      case getDocStateAt (diffDocIdx diff) dl of
        Nothing -> dl
        Just ds ->
          let oldLines = documentLines (docDocument ds)
              newLines = applyLineDiffs (diffForward diff) oldLines
              newDoc = fromLines newLines
          in setDocStateAt (diffDocIdx diff) (ds { docDocument = newDoc }) dl

restoreCursorsAndMarks :: UndoStep -> DocumentList -> DocumentList
restoreCursorsAndMarks step dl0 =
  let dlState = dlDocListState dl0
      dlState' = dlState
        { docCurrentLine = stepDlCurLine step
        , docMarks = stepDlMarks step
        }
      dl1 = dl0 { dlDocListState = dlState' }
      dl2 = foldl' restoreDocCursorMarks dl1 (Map.toList (stepCurLines step))
  in dl2
  where
    restoreDocCursorMarks dl (idx, cur) =
      case getDocStateAt idx dl of
        Nothing -> dl
        Just ds ->
          let marks' = Map.findWithDefault Map.empty idx (stepMarks step)
          in setDocStateAt idx (ds { docCurrentLine = cur, docMarks = marks' }) dl


data LedState = LedState
  { ledPrompt           :: Text
  , ledPromptActive     :: Bool
  , ledLastError        :: Maybe Text
  , ledHelpMode         :: Bool
  , ledSilent           :: Bool
  , ledLastShellCommand :: Maybe Text
  , ledLastRE           :: Maybe RE.BRE
  , ledLastReplacement  :: Maybe Text
  , ledErrorOccurred    :: Bool
  , ledCommandError     :: Bool
  , ledIsInteractive    :: Bool
  , ledDefinedFunctions :: Map Text ([Text], Text)  -- name -> (param names, body)
  , ledInputQueue       :: [Text]
  , ledCaptureOutput    :: Maybe [Text]  -- Nothing = normal output, Just acc = capturing (reverse order)
  , ledImportDirStack   :: [FilePath]    -- Stack of import directories
  , ledImportFileStack  :: [FilePath]    -- Stack of import file names
  , ledCurrentInputLine :: Maybe Text    -- Input line being processed
  , ledStartupFiles     :: [FilePath]    -- Files to open at startup
  , ledExecFiles        :: [FilePath]    -- Files to execute via 'im' on startup
  , ledManageWarned     :: Bool          -- True after a && warning
  , ledParamStack       :: [(Text, DocumentState)]
  , ledDocumentList     :: DocumentList
  , ledUndoManager      :: !UndoManager
  , ledUndoDepth        :: !Int          -- 0 = toplevel, >0 = nested
  , ledLastDocRange     :: Maybe DocRange
  , ledLastLineRange    :: Maybe LineRange
  , ledMultiDocPrint    :: Maybe Int  -- Just docNum when printing across multiple docs
  , ledVisualMode       :: Bool       -- Currently in visual mode?
  }

initialState :: UndoManager -> Bool -> Bool -> Maybe Text -> [FilePath] -> [FilePath] -> FilePath -> LedState
initialState undoMgr silent interactive prompt execFiles files cwd = LedState
  { ledPrompt           = fromMaybe "*" prompt
  , ledPromptActive     = isJust prompt
  , ledLastError        = Nothing
  , ledHelpMode         = False
  , ledSilent           = silent
  , ledLastShellCommand = Nothing
  , ledLastRE           = Nothing
  , ledLastReplacement  = Nothing
  , ledErrorOccurred    = False
  , ledCommandError     = False
  , ledIsInteractive    = interactive
  , ledDefinedFunctions = Map.empty
  , ledInputQueue       = []
  , ledCaptureOutput    = Nothing
  , ledImportDirStack   = [cwd]
  , ledImportFileStack  = []
  , ledCurrentInputLine = Nothing
  , ledStartupFiles     = files
  , ledExecFiles        = execFiles
  , ledManageWarned     = False
  , ledParamStack       = [("main", emptyDocumentState)]
  , ledDocumentList     = emptyDocumentList
  , ledUndoManager      = undoMgr
  , ledUndoDepth        = 0
  , ledLastDocRange     = Nothing
  , ledLastLineRange    = Nothing
  , ledMultiDocPrint    = Nothing
  , ledVisualMode       = False
  }

-- 1-based indexing
indexList :: [a] -> Int -> Maybe a
indexList = (. subtract 1) . (!?)
