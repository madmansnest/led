module LedIO
  ( readDocument
  , writeDocument
  , writeBytes
  , readTextFile
  , runShellCommand
  , formatReadError
  , formatWriteError
  , humanisePath
  , expandPath
  ) where

import Data.List (stripPrefix)
import System.Exit (ExitCode)
import System.IO.Error (IOError, isDoesNotExistError, isPermissionError)
import Control.Exception (catch)
import System.Directory (canonicalizePath, doesDirectoryExist, getHomeDirectory, makeRelativeToCurrentDirectory)
import System.FilePath ((</>), pathSeparator, takeDirectory)
import System.Process (readProcessWithExitCode)

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import LedDocument (Document, documentText, fromText)

readDocument :: FilePath -> IO (Either Text (Document, Int))
readDocument path =
  doesDirectoryExist path >>= bool readFileSafe isDirError
  where
    isDirError =
      pure $ Left (toText path <> ": Is a directory")
    readFileSafe =
      fmap decodeAndCount (B.readFile path)
        `catch` (pure . Left . formatReadError path)
    decodeAndCount bs =
      Right (fromText (TE.decodeUtf8 bs), B.length bs)

writeDocument :: FilePath -> Document -> IO (Either Text Int)
writeDocument path doc = do
  doesDirectoryExist path >>= bool checkParentAndWrite isDirError
  where
    isDirError =
      pure $ Left (toText path <> ": Is a directory")
    checkParentAndWrite = do
      let parentDir = takeDirectory path
      -- Check if parent directory exists (unless it's empty/current dir)
      parentExists <- if null parentDir || parentDir == "."
                      then pure True
                      else doesDirectoryExist parentDir
      if parentExists
        then writeFileSafe
        else pure $ Left (toText parentDir <> ": Directory does not exist")
    writeFileSafe = (B.writeFile path b $> Right (B.length b))
      `catch` (pure . Left . formatWriteError path)
    b = TE.encodeUtf8 $ documentText doc

formatReadError :: FilePath -> IOError -> Text
formatReadError path e
  | isDoesNotExistError e = toText path <> ": No such file or directory"
  | isPermissionError e   = toText path <> ": Permission denied"
  | otherwise             = toText path <> ": " <> toText @String (show e)

formatWriteError :: FilePath -> IOError -> Text
formatWriteError path e
  | isDoesNotExistError e = toText path <> ": No such file or directory"
  | isPermissionError e   = toText path <> ": Permission denied"
  | otherwise             = toText path <> ": " <> toText @String (show e)

writeBytes :: FilePath -> ByteString -> IO (Either Text Int)
writeBytes path bs = do
  let parentDir = takeDirectory path
  parentExists <- if null parentDir || parentDir == "."
                  then pure True
                  else doesDirectoryExist parentDir
  if not parentExists
    then pure $ Left (toText parentDir <> ": Directory does not exist")
    else (B.writeFile path bs $> Right (B.length bs))
           `catch` (pure . Left . formatWriteError path)

readTextFile :: FilePath -> IO (Either Text Text)
readTextFile path =
  (Right . TE.decodeUtf8 <$> B.readFile path)
    `catch` (pure . Left . toText @String . displayException @SomeException)

runShellCommand :: String -> String -> IO (ExitCode, String, String)
runShellCommand cmd input = readProcessWithExitCode "/bin/sh" ["-c", cmd] input

-- 1. If path starts with ~, expand it first
-- 2. Canonicalise (resolve symlinks, make absolute)
-- 3. Make relative to current directory if possible
-- 4. If still absolute and in home directory, replace home prefix with ~
-- On any error, returns the original path unchanged.
humanisePath :: FilePath -> IO FilePath
humanisePath path =
    (expandTildeForCanon path >>= canonicalizePath >>= makeRelativeToCurrentDirectory >>= unexpandTildeIfAbsolute)
      `catch` (\(_ :: SomeException) -> pure path)
  where
    -- Expand ~ so canonicalizePath can work (it doesn't understand ~)
    expandTildeForCanon p = case p of
      "~" -> getHomeDirectory
      ('~':c:rest) | c == pathSeparator -> do
        home <- getHomeDirectory
        pure (home </> rest)
      _ -> pure p
    unexpandTildeIfAbsolute path'
      | isAbsolute path' = unexpandTilde path'
      | otherwise = pure path'
    unexpandTilde path' = do
      home <- getHomeDirectory
      -- Canonicalize home too, so we compare canonical paths
      canonHome <- canonicalizePath home `catch` (\(_ :: SomeException) -> pure home)
      pure $ case stripPrefix canonHome path' of
        -- Only replace if home is followed by path separator or is the entire path
        Just "" -> "~"
        Just rest@(c:_) | c == pathSeparator -> '~' : rest
        _ -> path'
    isAbsolute p = case p of
      (c:_) -> c == pathSeparator
      _ -> False

-- 1. Replace leading ~ with home directory
-- 2. Canonicalise (resolve symlinks, make absolute)
-- On any error (e.g., missing home directory, non-existent path), returns the original path unchanged.
expandPath :: FilePath -> IO FilePath
expandPath path =
  (expandTilde path >>= canonicalizePath)
    `catch` (\(_ :: SomeException) -> pure path)
  where
    expandTilde p = case p of
      -- ~ alone
      "~" -> getHomeDirectory
      -- ~/... (where / is pathSeparator)
      ('~':c:rest) | c == pathSeparator -> do
        home <- getHomeDirectory
        pure (home </> rest)
      -- ~username/... is not supported, return as-is
      _ -> pure p
