module LedDiff
  ( ManageDiff(..)
  , ManageWarnings(..)
  , computeManageDiff
  , hasWarnings
  , formatManageWarnings
  ) where

import qualified Data.Text as T

data ManageDiff = ManageDiff
  { mdCreates     :: [Text]              -- ^ Files to create (new names with no old counterpart)
  , mdDeletes     :: [(Int, Text)]       -- ^ (1-based index in old list, filename) to delete
  , mdRenames     :: [(Int, Text, Text)] -- ^ (1-based index in old list, oldName, newName) to rename
  , mdDeleteStart :: Int                 -- ^ 1-based start of delete range in old doc list
  , mdDeleteEnd   :: Int                 -- ^ 1-based end of delete range in old doc list
  , mdInsertPos   :: Int                 -- ^ 0-based position for inserts in new doc list
  } deriving stock (Eq, Show)

data ManageWarnings = ManageWarnings
  { mwExistingFiles :: [Text]         -- ^ Files to create that already exist on disk
  , mwFilesToDelete :: [Text]         -- ^ Files that will be deleted from disk
  , mwMassRenames   :: [(Text, Text)] -- ^ (old, new) pairs if 2+ renames
  , mwRenameTargetExists :: [Text]    -- ^ Rename targets that already exist on disk
  } deriving stock (Eq, Show)

-- Uses the same prefix/suffix matching as applyDocListDiff:
-- finds common prefix and suffix, then in the changed middle region
-- pairs up old and new lines positionally (renames), with extras
-- being deletes or creates.
computeManageDiff :: [Text] -> [Text] -> ManageDiff
computeManageDiff oldLines newLines =
  let oldCount = length oldLines
      newCount = length newLines
      prefix = length (takeWhile id (zipWith (==) oldLines newLines))
      oldRev = reverse (drop prefix oldLines)
      newRev = reverse (drop prefix newLines)
      suffix = length (takeWhile id (zipWith (==) oldRev newRev))
      deletedCount = oldCount - prefix - suffix
      insertedCount = newCount - prefix - suffix
      deletedLines = take deletedCount (drop prefix oldLines)
      insertedLines = take insertedCount (drop prefix newLines)
      -- Pair up positionally: min(deleted, inserted) are renames,
      -- extra deleted are pure deletes, extra inserted are pure creates
      pairedCount = min (length deletedLines) (length insertedLines)
      deleteStart = prefix + 1
      renames = [ (deleteStart + i, old, new)
                | (i, (old, new)) <- zip [0..] (zip (take pairedCount deletedLines) (take pairedCount insertedLines))
                , old /= new
                ]
      extraDeletes = zip [deleteStart + pairedCount ..] (drop pairedCount deletedLines)
      extraCreates = drop pairedCount insertedLines
  in ManageDiff
    { mdCreates = extraCreates
    , mdDeletes = extraDeletes
    , mdRenames = renames
    , mdDeleteStart = deleteStart
    , mdDeleteEnd = deleteStart + deletedCount - 1
    , mdInsertPos = prefix
    }

hasWarnings :: ManageWarnings -> Bool
hasWarnings w = not (null (mwExistingFiles w))
             || not (null (mwFilesToDelete w))
             || not (null (mwMassRenames w))
             || not (null (mwRenameTargetExists w))

formatManageWarnings :: ManageWarnings -> Text
formatManageWarnings w = T.intercalate "; " $ concat
  [ if null (mwFilesToDelete w) then []
    else ["will delete: " <> T.intercalate ", " (mwFilesToDelete w)]
  , if null (mwExistingFiles w) then []
    else ["already exists: " <> T.intercalate ", " (mwExistingFiles w)]
  , if null (mwRenameTargetExists w) then []
    else ["rename target exists: " <> T.intercalate ", " (mwRenameTargetExists w)]
  , if null (mwMassRenames w) then []
    else ["will rename: " <> T.intercalate ", " [old <> " -> " <> new | (old, new) <- mwMassRenames w]]
  ]
