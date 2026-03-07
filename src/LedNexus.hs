module LedNexus
  ( BufferChangeFlag(..)
  , DocumentState(..)
  , DocumentList(..)
  , emptyDocumentState
  , emptyDocumentList
  , dlDocList
  , dlCurrentDoc
  , setDlCurrentDoc
  , documentCount
  , getCurrentDocState
  , getDocStateAt
  , setDocStateAt
  , insertDocAfter
  , deleteDocRange
  , getFilenameAt
  , setFilenameAt
  , getCurrentFilename
  , setCurrentFilename
  , moveDocument
  , hasUnsavedChanges
  , unsavedDocuments
  , changedDocuments
  , singletonDocumentList
  , modifyCurrentDocState
  , insertLinesInDocStateAt
  , adjustMarksForInsert
  , adjustMarksForDelete
  , insertLinesIntoDocState
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import LedDocument (Document, emptyDocument, documentLines, fromLines, appendAfter, deleteLines, lineCount)
import LedParse (Addr(..))
import LedResolve (resolveAddr)

data BufferChangeFlag = Unchanged | Changed | ChangedAndWarned
  deriving stock (Eq, Show)

data DocumentState = DocumentState
  { docDocument    :: Document
  , docCurrentLine :: Int
  , docMarks       :: Map.Map Char Int
  , docChangeFlag  :: BufferChangeFlag
  } deriving stock (Eq, Show)

data DocumentList = DocumentList
  { dlDocListState :: DocumentState
  , dlDocuments    :: Seq DocumentState
  } deriving stock (Eq, Show)

emptyDocumentState :: DocumentState
emptyDocumentState = DocumentState
  { docDocument    = emptyDocument
  , docCurrentLine = 0
  , docMarks       = Map.empty
  , docChangeFlag  = Unchanged
  }

emptyDocumentList :: DocumentList
emptyDocumentList = DocumentList
  { dlDocListState = emptyDocumentState
  , dlDocuments    = Seq.empty
  }

dlDocList :: DocumentList -> Document
dlDocList = docDocument . dlDocListState

dlCurrentDoc :: DocumentList -> Int
dlCurrentDoc = docCurrentLine . dlDocListState

setDlCurrentDoc :: Int -> DocumentList -> DocumentList
setDlCurrentDoc n dl = dl { dlDocListState = (dlDocListState dl) { docCurrentLine = n } }

documentCount :: DocumentList -> Int
documentCount = Seq.length . dlDocuments

getCurrentDocState :: DocumentList -> Maybe DocumentState
getCurrentDocState dl
  | dlCurrentDoc dl == 0 = Nothing
  | otherwise = getDocStateAt (dlCurrentDoc dl) dl

-- | Get the document state at a 1-based index.
getDocStateAt :: Int -> DocumentList -> Maybe DocumentState
getDocStateAt idx dl
  | idx < 1 || idx > documentCount dl = Nothing
  | otherwise = Just (Seq.index (dlDocuments dl) (idx - 1))

-- | Set the document state at a 1-based index.
setDocStateAt :: Int -> DocumentState -> DocumentList -> DocumentList
setDocStateAt idx ds dl
  | idx < 1 || idx > documentCount dl = dl
  | otherwise = dl { dlDocuments = Seq.update (idx - 1) ds (dlDocuments dl) }

-- | Insert a new document after the given position (0 = insert at beginning).
-- Also inserts the filename into dlDocList to maintain synchronization.
insertDocAfter :: Int -> Text -> DocumentState -> DocumentList -> DocumentList
insertDocAfter pos filename ds dl =
  let oldDlState = dlDocListState dl
      newDlDoc = appendAfter pos [filename] (docDocument oldDlState)
      (before, after) = Seq.splitAt pos (dlDocuments dl)
      newDocuments = before <> Seq.singleton ds <> after
  in dl { dlDocListState = oldDlState { docDocument = newDlDoc }, dlDocuments = newDocuments }

-- | Delete documents in the range [start, end] (1-based, inclusive).
-- Also deletes the corresponding lines from dlDocument.
deleteDocRange :: Int -> Int -> DocumentList -> DocumentList
deleteDocRange start end dl
  | start < 1 || end < start || start > documentCount dl = dl
  | otherwise =
      let oldDlState = dlDocListState dl
          curDoc = docCurrentLine oldDlState
          newDlDoc = deleteLines start end (docDocument oldDlState)
          count = documentCount dl
          actualEnd = min end count
          (before, rest) = Seq.splitAt (start - 1) (dlDocuments dl)
          after = Seq.drop ((actualEnd - start + 1)) rest
          newDocuments = before <> after
          newCurrent
            | curDoc < start = curDoc
            | curDoc > actualEnd = curDoc - (actualEnd - start + 1)
            | Seq.length newDocuments == 0 = 0
            | start <= Seq.length newDocuments = start
            | otherwise = Seq.length newDocuments
      in dl { dlDocListState = oldDlState { docDocument = newDlDoc, docCurrentLine = newCurrent }
            , dlDocuments = newDocuments }

-- | Get the filename at a 1-based document index.
getFilenameAt :: Int -> DocumentList -> Maybe Text
getFilenameAt idx dl
  | idx < 1 || idx > documentCount dl = Nothing
  | otherwise =
      let lns = documentLines (dlDocList dl)
      in viaNonEmpty head (drop (idx - 1) lns)

-- | Set the filename at a 1-based document index.
setFilenameAt :: Int -> Text -> DocumentList -> DocumentList
setFilenameAt idx filename dl
  | idx < 1 || idx > documentCount dl = dl
  | otherwise =
      let oldDlState = dlDocListState dl
          lns = documentLines (docDocument oldDlState)
          (before, rest) = splitAt (idx - 1) lns
      in case rest of
        (_:after) -> dl { dlDocListState = oldDlState { docDocument = fromLines (before ++ [filename] ++ after) } }
        [] -> dl  -- Should not happen given bounds check

-- | Get the current filename (filename of current document).
getCurrentFilename :: DocumentList -> Maybe Text
getCurrentFilename dl
  | dlCurrentDoc dl == 0 = Nothing
  | otherwise = getFilenameAt (dlCurrentDoc dl) dl

-- | Set the current filename.
setCurrentFilename :: Text -> DocumentList -> DocumentList
setCurrentFilename filename dl
  | dlCurrentDoc dl == 0 = dl
  | otherwise = setFilenameAt (dlCurrentDoc dl) filename dl

-- | Move a document from one position to another.
-- Similar to ed's m command but for documents.
moveDocument :: Int -> Int -> Int -> DocumentList -> DocumentList
moveDocument start end dest dl
  | dest >= start && dest <= end = dl  -- Invalid move
  | start < 1 || end > documentCount dl = dl
  | otherwise =
      let curDoc = dlCurrentDoc dl
          srcDocs = Seq.take ((end - start + 1))
                  $ Seq.drop (start - 1) (dlDocuments dl)
          srcNames = take ((end - start + 1))
                   $ drop (start - 1) (documentLines (dlDocList dl))
          -- Delete source
          dl' = deleteDocRange start end dl
          -- Adjust destination
          adjDest = if dest > end then dest - (end - start + 1) else dest
          -- Insert at new position
          (beforeDocs, afterDocs) = Seq.splitAt adjDest (dlDocuments dl')
          newDocuments = beforeDocs <> srcDocs <> afterDocs
          beforeNames = take adjDest (documentLines (dlDocList dl'))
          afterNames = drop adjDest (documentLines (dlDocList dl'))
          newDlDoc = fromLines (beforeNames ++ srcNames ++ afterNames)
          -- Update current if it was in the moved range
          newCurrent
            | curDoc >= start && curDoc <= end =
                adjDest + (curDoc - start) + 1
            | curDoc > end && curDoc <= dest =
                curDoc - (end - start + 1)
            | curDoc >= dest && curDoc < start =
                curDoc + (end - start + 1)
            | otherwise = dlCurrentDoc dl'
          oldDlState' = dlDocListState dl'
      in dl' { dlDocListState = oldDlState' { docDocument = newDlDoc, docCurrentLine = newCurrent }
             , dlDocuments = newDocuments }

-- | Check if any document in the list has unsaved changes.
hasUnsavedChanges :: DocumentList -> Bool
hasUnsavedChanges dl = any isChanged (toList (dlDocuments dl))
  where isChanged ds = docChangeFlag ds /= Unchanged

-- | Get list of document indices with unsaved changes.
unsavedDocuments :: DocumentList -> [Int]
unsavedDocuments dl =
  [ idx
  | (idx, ds) <- zip [1..] (toList (dlDocuments dl))
  , docChangeFlag ds /= Unchanged
  ]
  
-- | Get list of document indices with changed and unwarned status.
changedDocuments :: DocumentList -> [Int]
changedDocuments dl =
  [ idx
  | (idx, ds) <- zip [1..] (toList (dlDocuments dl))
  , docChangeFlag ds == Changed
  ]

-- | Create a document list from a single document state and filename.
singletonDocumentList :: Text -> DocumentState -> DocumentList
singletonDocumentList filename ds = DocumentList
  { dlDocListState = emptyDocumentState { docDocument = fromLines [filename], docCurrentLine = 1 }
  , dlDocuments = Seq.singleton ds
  }

-- | Modify the current document state using a function.
modifyCurrentDocState :: (DocumentState -> DocumentState) -> DocumentList -> DocumentList
modifyCurrentDocState f dl
  | dlCurrentDoc dl == 0 = dl
  | otherwise = case getCurrentDocState dl of
      Nothing -> dl
      Just ds -> setDocStateAt (dlCurrentDoc dl) (f ds) dl

-- ---------------------------------------------------------------------------
-- Pure DocumentState operations
-- ---------------------------------------------------------------------------

-- | Insert lines after a given position in a DocumentState.
-- Returns the updated DocumentState with adjusted current line and marks.
-- Position 0 inserts at the beginning.
insertLinesInDocStateAt :: Int -> [Text] -> DocumentState -> DocumentState
insertLinesInDocStateAt dest srcLines ds =
  let doc = docDocument ds
      count = length srcLines
      doc' = appendAfter dest srcLines doc
      newLine = dest + count
      marks' = adjustMarksForInsert dest count (docMarks ds)
  in ds { docDocument = doc'
        , docCurrentLine = newLine
        , docMarks = marks'
        , docChangeFlag = Changed
        }

-- | Adjust marks after inserting count lines after position pos.
-- Marks pointing after pos are shifted by count.
adjustMarksForInsert :: Int -> Int -> Map.Map Char Int -> Map.Map Char Int
adjustMarksForInsert pos count = Map.map adj
  where adj ln = if ln > pos then ln + count else ln

-- | Adjust marks after deleting lines from start to end (inclusive).
-- Marks pointing to deleted lines are removed; marks after are shifted.
adjustMarksForDelete :: Int -> Int -> Map.Map Char Int -> Map.Map Char Int
adjustMarksForDelete start end = Map.mapMaybe adj
  where
    count = end - start + 1
    adj ln
      | ln >= start && ln <= end = Nothing
      | ln > end                 = Just (ln - count)
      | otherwise                = Just ln

-- ---------------------------------------------------------------------------
-- Address-based insertion
-- ---------------------------------------------------------------------------

-- | Resolve an address within a DocumentState, insert lines,
-- adjust marks, and return the updated DocumentState.
-- Uses insertLinesInDocStateAt for the pure part after address resolution.
insertLinesIntoDocState :: Addr -> [Text] -> DocumentState -> Either Text DocumentState
insertLinesIntoDocState lineAddr srcLines ds =
  let doc = docDocument ds
      cur = docCurrentLine ds
      total = lineCount doc
      marks = docMarks ds
      docLines = documentLines doc
  in case resolveAddr cur total marks docLines lineAddr of
    Left err -> Left err
    Right dest
      | dest < 0 -> Left "Invalid address"
      | otherwise -> Right (insertLinesInDocStateAt dest srcLines ds)
