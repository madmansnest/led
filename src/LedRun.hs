module LedRun (run, sighupSave) where

import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory, XdgDirectory(XdgState))
import System.FilePath ((</>))

import qualified Data.Text as T

import LedCore (LedState(..), initialState)
import LedExec (processInput, readBlockContinuation, readSubContinuation, readBackslashContinuation, evaluatePrompt, importMacroCommand)
import LedIO (writeDocument, expandPath)
import LedInput (Led, LedEnv(..), HupRef, withLedEnv, runLed, getInputLineLed, handleInterrupt, outputLine)
import LedNexus (BufferChangeFlag(..), DocumentState(..), insertDocAfter, documentCount, getFilenameAt, getDocStateAt, setDlCurrentDoc, emptyDocumentList, emptyDocumentState)
import LedOptions (Options(..))
import LedReadWrite (editFile)
import LedSession (printHelpIfActive)
import LedState (ensureNonEmptyDocList, flagError, guardChanged, setErrorText)
import LedUndo (newUndoManager)
import LedVi (enterVisualMode, VisualModeResult(..))
import qualified LedDocument
import qualified LedUndo


getHistoryPath :: IO (Maybe FilePath)
getHistoryPath = do
  dir <- getXdgDirectory XdgState "led"
  let path = dir </> "history"
  createDirectoryIfMissing True dir
  pure (Just path)


-- | Run the editor. Returns True if any error occurred during the session.
run :: Bool -> IORef (IO ()) -> Options -> IO Bool
run interactive hupRef options = do
  cwd <- getCurrentDirectory
  histFile <- if interactive then getHistoryPath else pure Nothing
  undoMgr <- newUndoManager
  let st0 = initialState undoMgr (optSilent options) interactive (optPrompt options) (optExecFiles options) (optFiles options) cwd
  -- Run with LedLine environment (interactive) or simple stdin (non-interactive)
  withLedEnv interactive histFile $ \env -> do
    -- Run startup
    (_, st1) <- runLed env st0 startup
    -- Main loop
    finalSt <- mainLoop env hupRef st1
    pure (ledErrorOccurred finalSt)


startup :: Led ()
startup = do
  modify (\s -> s { ledDocumentList = emptyDocumentList })
  gets ledStartupFiles >>=
    traverse_ (\rawPath -> do
      path <- liftIO (expandPath rawPath)
      dl <- gets ledDocumentList
      let newIdx = documentCount dl + 1
      -- Insert empty document and make it current
      modify (\s -> s { ledDocumentList = setDlCurrentDoc newIdx (insertDocAfter (documentCount dl) "" emptyDocumentState dl) })
      -- Use editFile to load the file (same as 'e' command)
      editFile path)
  modify (\s -> s { ledDocumentList = setDlCurrentDoc 1 (ledDocumentList s) })
  ensureNonEmptyDocList
  gets ledExecFiles >>= traverse_ importMacroCommand


-- | Main loop that handles transitions between normal REPL and visual mode.
mainLoop :: LedEnv -> HupRef -> LedState -> IO LedState
mainLoop env hupRef st
  | ledVisualMode st = do
      -- Run visual mode session (handles all commands internally)
      (result, st') <- enterVisualMode env st
      case result of
        VisualContinue ->
          -- Exit visual mode, return to normal REPL
          mainLoop env hupRef st'
        VisualQuit ->
          -- Quit editor entirely
          pure st'
  | otherwise = do
      -- Run normal REPL
      (shouldContinue, st') <- runLed env st (repl hupRef)
      -- Check if we should enter visual mode
      if ledVisualMode st'
        then mainLoop env hupRef st'
        else if shouldContinue
          then mainLoop env hupRef st'
          else pure st'


-- | Save buffer on SIGHUP: try ed.hup in cwd, then $HOME/ed.hup.
-- Now saves all modified documents.
sighupSave :: LedState -> IO ()
sighupSave st = do
    let dl = ledDocumentList st
    forM_ [1..documentCount dl] $ \idx -> do
      case getDocStateAt idx dl of
        Nothing -> pure ()
        Just ds -> when (LedDocument.lineCount (docDocument ds) > 0 && docChangeFlag ds /= Unchanged) $ do
          let filename = maybe ("ed" <> show idx <> ".hup") toString (getFilenameAt idx dl)
              hupFile = if null filename then "ed" <> show idx <> ".hup" else filename <> ".hup"
          result <- writeDocument hupFile (docDocument ds)
          case result of
            Right _ -> pure ()
            Left _ -> do
              mhome <- lookupEnv "HOME"
              case mhome of
                Just home -> void $ writeDocument (home <> "/" <> hupFile) (docDocument ds)
                Nothing   -> pure ()


repl :: HupRef -> Led Bool
repl hupRef = do
  -- Update SIGHUP handler with current state
  st <- get
  liftIO (writeIORef hupRef (sighupSave st))
  modify (\s -> s { ledCommandError = False })
  shouldContinue <- handleInterrupt (handleSigint >> pure True) $ do
    queue <- gets ledInputQueue
    case queue of
      (x:xs) -> do
        modify (\s -> s { ledInputQueue = xs })
        fullLine <- readBlockContinuation x >>= readSubContinuation >>= readBackslashContinuation
        processInputWithUndo fullLine
      [] -> evaluatePrompt >>= getInputLineLed >>= \case
        Nothing -> handleEof
        Just "\x03" -> handleSigint >> pure True  -- Ctrl+C marker from LedLine
        Just line -> do
          fullLine <- readBlockContinuation (toText line) >>= readSubContinuation >>= readBackslashContinuation
          processInputWithUndo fullLine
  ensureNonEmptyDocList
  -- In non-interactive mode, stop on any error
  cmdErr <- gets ledCommandError
  interactive <- gets ledIsInteractive
  let stop = not shouldContinue || (not interactive && cmdErr)
  pure (not stop)
  where
    handleSigint = do
      modify (\s -> s { ledInputQueue = [] })  -- Clear pending commands
      setErrorText "Interrupt"
      flagError
      outputLine "?"
      printHelpIfActive
    handleEof = do
      interactive <- gets ledIsInteractive
      if interactive then guardChanged >>= \ok -> pure (not ok)
      else pure False

-- | Process input with undo tracking at toplevel.
-- Captures pre-state before processing and commits undo step after.
-- Skip tracking for u/U commands since they manage undo/redo stacks themselves.
processInputWithUndo :: Text -> Led Bool
processInputWithUndo input = do
  depth <- gets ledUndoDepth
  mgr <- gets ledUndoManager
  let stripped = T.strip input
      isUndoRedo = stripped == "u" || stripped == "U"
  -- Capture pre-state at toplevel, unless it's an undo/redo command
  when (depth == 0 && not isUndoRedo) $ do
    dl <- gets ledDocumentList
    liftIO $ LedUndo.capturePreState mgr dl
  -- Process the input
  result <- processInput input
  -- Commit undo step at toplevel, unless it's an undo/redo command
  when (depth == 0 && not isUndoRedo) $ do
    dl <- gets ledDocumentList
    mStep <- liftIO $ LedUndo.commitUndoStep mgr dl
    -- Clear redo stack if there were changes
    when (isJust mStep) $ liftIO $ LedUndo.clearRedoStack mgr
  pure result
