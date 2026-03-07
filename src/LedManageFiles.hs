-- | File management IO operations for led.
-- This module handles disk-side operations for the && (manage) prefix.
-- Pure diff computation is in LedDiff.
module LedManageFiles
  ( -- * Re-exports from LedDiff
    ManageDiff(..)
  , ManageWarnings(..)
  , computeManageDiff
  , hasWarnings
  , formatManageWarnings
    -- * IO operations
  , checkManageWarnings
  , executeManageActions
  ) where

import Control.Exception (catch)
import System.Directory (doesFileExist, removeFile, renamePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO.Error (IOError)

import LedDiff (ManageDiff(..), ManageWarnings(..), computeManageDiff, hasWarnings, formatManageWarnings)

-- | Check which warnings apply by inspecting disk state.
checkManageWarnings :: (FilePath -> IO FilePath) -> ManageDiff -> IO ManageWarnings
checkManageWarnings expandPath diff = do
  -- Check which create targets already exist on disk
  existingCreates <- filterExisting expandPath (mdCreates diff)
  let filesToDelete = map snd (mdDeletes diff)
  let renames = [(old, new) | (_, old, new) <- mdRenames diff]
      massRenames = if length renames >= 2 then renames else []
  -- Check if rename targets already exist on disk
  existingRenames <- filterExisting expandPath (map snd renames)
  pure ManageWarnings
    { mwExistingFiles = existingCreates
    , mwFilesToDelete = filesToDelete
    , mwMassRenames = massRenames
    , mwRenameTargetExists = existingRenames
    }
  where
    filterExisting expand names = do
      results <- forM names $ \name -> do
        path <- expand (toString name)
        exists <- doesFileExist path
        pure (if exists then Just name else Nothing)
      pure (catMaybes results)

-- | Execute the disk-side file management operations.
-- Returns a list of error messages for any failed operations.
executeManageActions :: (FilePath -> IO FilePath) -> ManageDiff -> IO [Text]
executeManageActions expandPath diff = do
  errors <- newIORef ([] :: [Text])
  -- Execute deletes
  forM_ (mdDeletes diff) $ \(_, filename) -> do
    path <- expandPath (toString filename)
    removeFile path `catch` \(e :: IOError) ->
      modifyIORef errors (toText (displayException e) :)
  -- Execute renames
  forM_ (mdRenames diff) $ \(_, oldName, newName) -> do
    oldPath <- expandPath (toString oldName)
    newPath <- expandPath (toString newName)
    let dir = takeDirectory newPath
    createDirectoryIfMissing True dir
    renamePath oldPath newPath `catch` \(e :: IOError) ->
      modifyIORef errors (toText (displayException e) :)
  -- Execute creates (touch empty files)
  forM_ (mdCreates diff) $ \filename -> do
    path <- expandPath (toString filename)
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    writeFile path ("" :: String) `catch` \(e :: IOError) ->
      modifyIORef errors (toText (displayException e) :)
  readIORef errors
