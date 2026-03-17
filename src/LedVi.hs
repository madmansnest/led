{-# LANGUAGE LambdaCase #-}
module LedVi
  ( enterVisualMode
  , VisualModeResult(..)
  ) where

import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Platform.Unix as VtyUnix

import LedCore (LedState(..), UndoManager(..))
import LedInput (Led, LedEnv(..), runLed)
import LedDocument (documentLines, lineCount)
import LedExec (processInput, readBlockContinuation, readSubContinuation, readBackslashContinuation)
import LedGlobal (findMatchingLineNumbers)
import LedState (ensureNonEmptyDocList)
import LedNexus (BufferChangeFlag(..), DocumentState(..), dlCurrentDoc, documentCount, getDocStateAt, setDocStateAt)
import LedParse (Command(..), FullRange(..), DocRange(..), LineRange(..))
import LedResolve (resolveLineRange)
import qualified LedUndo
import LedVi.Line
import LedVi.Mouse
import LedVi.Parse (PartialParse(..), parsePartialWithFns, isParseError)
import LedVi.Types (VisualModeResult(..), InputResult(..), ResultDisplay(..), DisplayZone(..), DisplayLine(..), LineStyle(..), HighlightStyle(..), ScrollDirection(..), applyScroll, canScroll, isRangeVisible, isNearWindowEdge, ContinuationContext(..), ContinuationType(..), GlobalContInfo(..))
import LedVi.Render (wrapDisplayZone, errorAttr, inputAttr)
import LedVi.Preview (generateDisplayForParse, generateDisplayForParseAtView, windowedRenderThreshold, computeGlobalMatches, showGlobalCommandResultsWithMatches)
import LedVi.Result (resultDisplayToZone, textLinesToDisplayZone, generateDisplayZoneForParse, getGlobalCommandInfo)
import LedVi.Completion (CompletionResult(..), performCompletion)


displayAttr :: Vty.Attr
displayAttr = Vty.defAttr `Vty.withForeColor` Vty.white `Vty.withBackColor` Vty.black


enterVisualMode :: LedEnv -> LedState -> IO (VisualModeResult, LedState)
enterVisualMode env st = bracket
  (VtyUnix.mkVty Vty.defaultConfig)
  Vty.shutdown
  $ \vty -> do
      -- Enable mouse mode for scroll wheel support
      let output = Vty.outputIface vty
      Vty.setMode output Vty.Mouse True

      -- Create an IORef to hold the current input state for the input hook
      inputRef <- newIORef emptyInput

      -- Load history from file
      initialHist <- loadHistoryNav (leHistoryFile env)
      histRef <- newIORef initialHist

      -- Reset warned status: ChangedAndWarned -> Changed
      -- This ensures user gets warned again after entering visual mode
      let st' = resetWarnedStatus st { ledVisualMode = True }
          -- Show blank screen on entry (no implicit file display)
          initialResult = ResultInitial
      result <- runVisualSession vty env inputRef histRef st' initialResult

      -- Save history on exit
      finalHist <- readIORef histRef
      saveHistoryNav finalHist

      return result


-- When entering visual mode, the user should be warned again about unsaved changes.
resetWarnedStatus :: LedState -> LedState
resetWarnedStatus st =
  let dl = ledDocumentList st
      n = documentCount dl
      resetDoc idx docList = case getDocStateAt idx docList of
        Nothing -> docList
        Just ds -> case docChangeFlag ds of
          ChangedAndWarned -> setDocStateAt idx (ds { docChangeFlag = Changed }) docList
          _ -> docList
      dl' = foldr resetDoc dl [1..n]
  in st { ledDocumentList = dl' }


runVisualSession :: Vty.Vty -> LedEnv -> IORef InputState -> IORef HistoryNav -> LedState -> ResultDisplay -> IO (VisualModeResult, LedState)
runVisualSession vty env inputRef histRef st resultDisplay = do
  -- First, process any pending commands from the input queue
  -- (e.g., remaining commands after 'vi' was called in a function)
  let queue = ledInputQueue st
  case queue of
    (cmd:rest) -> do
      -- Process pending command
      let st' = st { ledInputQueue = rest }
      (newResult, st'', shouldQuit) <- processLine vty env inputRef st' cmd
      if shouldQuit
        then return (VisualQuit, st'' { ledVisualMode = False })
        else if not (ledVisualMode st'')
          -- Visual mode was toggled off, exit to normal mode
          then return (VisualContinue, st'')
          else runVisualSession vty env inputRef histRef st'' newResult

    [] -> do
      -- No pending commands, normal interactive flow
      -- Render current state
      renderScreenWithResult vty st emptyInput resultDisplay False

      -- Get a line of input
      result <- getInputLine vty st inputRef histRef resultDisplay

      case result of
        InputEOF ->
          -- EOF (Ctrl-D on empty) - quit
          return (VisualQuit, st { ledVisualMode = False })

        InputExitVisual ->
          -- Exit visual mode (double ESC or "vi" command)
          return (VisualContinue, st { ledVisualMode = False })

        InputInterrupt -> do
          -- Interrupt (Ctrl-C) - show error and continue
          runVisualSession vty env inputRef histRef st (ResultText ["?"])

        InputLine line
          | line == "vi" ->
              -- Exit visual mode
              return (VisualContinue, st { ledVisualMode = False })

          | otherwise -> do
              -- Add to history
              modifyIORef histRef (addToHistory line)

              -- Process the line through normal Led infrastructure
              (newResult, st', shouldQuit) <- processLine vty env inputRef st line

              if shouldQuit
                then return (VisualQuit, st' { ledVisualMode = False })
                else if not (ledVisualMode st')
                  -- Visual mode was toggled off (e.g., 'vi' in a function)
                  then return (VisualContinue, st')
                  else runVisualSession vty env inputRef histRef st' newResult


-- resultDisplay: result of previous command (shown when input is empty or on error)
getInputLine :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> ResultDisplay -> IO InputResult
getInputLine vty st inputRef histRef resultDisplay = do
  writeIORef inputRef emptyInput
  -- Reset history navigation position for new input
  modifyIORef histRef (\hn -> hn { hnIndex = -1, hnOriginal = "" })
  -- Create scroll state ref with special marker (-1) to indicate first iteration
  -- -1 means "use display zone's scroll if target visible, otherwise compute"
  scrollRef <- newIORef (-1 :: Int, "" :: Text)
  -- Create result display ref (for multi-doc navigation)
  resultRef <- newIORef resultDisplay
  -- Create mouse state ref (for click-and-drag selection)
  mouseRef <- newIORef emptyMouseState
  -- Create async preview ref (for non-blocking global search)
  asyncRef <- newIORef (Nothing :: CachedGlobalMatches)
  inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False


-- Async state: (Maybe ongoing computation, Maybe cached results)
-- | Cached global command matches: (pattern, docIdx, matches)
-- Pattern is cached to avoid recomputing when only the command changes.
type CachedGlobalMatches = Maybe (Text, Int, [Int])


inputLoop :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> Bool -> IO InputResult
inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef lastWasEsc = do
  input <- readIORef inputRef
  resultDisplay <- readIORef resultRef
  let inputTxt = inputText input

  -- Check if input changed
  (scrollOffset, lastInputTxt) <- readIORef scrollRef
  let inputChanged = inputTxt /= lastInputTxt

  -- Parse input for real-time preview
  let userFns = Set.fromList $ Map.keys (ledDefinedFunctions st)
      pp = parsePartialWithFns userFns inputTxt
      hasError = isParseError pp

  -- Get terminal dimensions for display generation
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)
  let displayHeight = height - 1
      dl = ledDocumentList st
      docIdx = dlCurrentDoc dl

  -- Determine what to show in display zone:
  -- - If input is empty: show previous command result
  -- - If parse error: show previous command result (input line will be red)
  -- - Otherwise: show preview based on partial parse
  -- Then apply soft wrapping for long lines.
  --
  -- For windowed documents: if user has scrolled near buffer edge and input hasn't
  -- changed, regenerate centered on current view position instead of target.
  let isWindowed baseD = dzTotalLines baseD > windowedRenderThreshold && dzWindowStart baseD > 1

      -- Check if we need to regenerate for scroll position
      needsViewRegen baseD = not inputChanged
                          && isWindowed baseD
                          && isNearWindowEdge baseD { dzScrollTop = scrollOffset }

      -- Compute logical view top line for regeneration
      computeViewTop baseD = dzWindowStart baseD + scrollOffset

  -- Handle global commands with match caching
  -- Extract (invert, pattern, command, docRange, lineRange) if this is a global command with closed delimiter
  let mGlobalInfo = getGlobalCommandInfo pp inputTxt

  -- Manage cached matches and compute display
  baseDisplay <- case mGlobalInfo of
    Nothing -> do
      -- Not a global command - clear cache and use sync path
      writeIORef asyncRef Nothing
      pure $ if T.null (T.strip inputTxt) || hasError
        then resultDisplayToZone resultDisplay st width displayHeight
        else generateDisplayZoneForParse pp st width displayHeight

    Just (invert, pat, cmd, _docRange, _lineRange) -> do
      -- Global command with closed delimiter
      -- Check if we can reuse cached matches (same pattern and document)
      cached <- readIORef asyncRef
      matches <- case cached of
        Just (cachedPat, cachedDocIdx, cachedMatches)
          | cachedPat == pat && cachedDocIdx == docIdx ->
            -- Pattern and doc unchanged, reuse cached matches
            pure cachedMatches
        _ -> do
          -- Pattern or doc changed, recompute matches
          newMatches <- computeGlobalMatches invert pat docIdx dl
          writeIORef asyncRef (Just (pat, docIdx, newMatches))
          pure newMatches

      -- Display with command-specific styling (pass global pattern for s// case)
      pure $ showGlobalCommandResultsWithMatches docIdx matches pat cmd dl width displayHeight

  -- Check if we need to regenerate centered on view position
  let finalBase = if needsViewRegen baseDisplay && not hasError && not (T.null (T.strip inputTxt))
        then let viewTop = computeViewTop baseDisplay
             in generateDisplayForParseAtView pp (ledDocumentList st) (ledParamStack st)
                  (ledLastLineRange st) viewTop width displayHeight
        else baseDisplay

      -- Wrap long lines for display and scrolling
      wrappedBase = wrapDisplayZone finalBase

  -- Apply scroll offset:
  -- - If scrollOffset = -1 (first iteration), check if target visible at 0, else use computed
  -- - If input changed and target range is NOT already visible, use preview's scroll
  -- - If input changed but target range IS already visible, keep current scroll
  -- - If input is the same, use the user's manual scroll position (for manual scrolling)
  -- - If we regenerated for view, reset scroll to 0 (view is already centered)
  -- All indices are in visual (wrapped) line units.
  let regenForView = needsViewRegen baseDisplay && not hasError && not (T.null (T.strip inputTxt))
      scrollOffset' = if regenForView
        then 0  -- View-centered regeneration, start at top of new window
        else if scrollOffset < 0
        -- First iteration: determine initial scroll
        then case dzTargetRange wrappedBase of
          Nothing -> dzScrollTop wrappedBase
          Just (targetStart, targetEnd) ->
            if isRangeVisible targetStart targetEnd 0 displayHeight
              then 0  -- Range visible at top, stay at top
              else dzScrollTop wrappedBase  -- Use computed scroll
        else if inputChanged
        then case dzTargetRange wrappedBase of
          Nothing -> dzScrollTop wrappedBase  -- No target, use preview's scroll
          Just (targetStart, targetEnd) ->
            if isRangeVisible targetStart targetEnd scrollOffset displayHeight
              then scrollOffset  -- Range already visible, keep current scroll
              else dzScrollTop wrappedBase  -- Range not visible, scroll to show it
        else scrollOffset  -- Input unchanged, keep user's scroll position
      dz = wrappedBase { dzScrollTop = scrollOffset' }
  writeIORef scrollRef (scrollOffset', inputTxt)

  renderScreenWithZone vty st input dz hasError

  ev <- Vty.nextEvent vty
  case ev of
    Vty.EvKey key mods -> handleKey vty st inputRef histRef scrollRef resultRef mouseRef asyncRef lastWasEsc key mods dz
    Vty.EvMouseDown _ _ Vty.BScrollUp _ -> do
      -- Mouse scroll up - check for document switch at boundary
      handleScrollUp vty st inputRef histRef scrollRef resultRef mouseRef asyncRef scrollOffset' inputTxt dz
    Vty.EvMouseDown _ _ Vty.BScrollDown _ -> do
      -- Mouse scroll down - check for document switch at boundary
      handleScrollDown vty st inputRef histRef scrollRef resultRef mouseRef asyncRef scrollOffset' inputTxt dz
    Vty.EvMouseDown _col row Vty.BLeft _ -> do
      -- Left mouse button pressed - start line selection
      handleMouseDown vty st inputRef histRef scrollRef resultRef mouseRef asyncRef row dz
    Vty.EvMouseUp _col row (Just Vty.BLeft) -> do
      -- Left mouse button released - complete line selection
      handleMouseUp vty st inputRef histRef scrollRef resultRef mouseRef asyncRef row dz
    Vty.EvResize _ _ -> inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef lastWasEsc
    _ -> inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef lastWasEsc


handleScrollUp :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> Int -> Text -> DisplayZone -> IO InputResult
handleScrollUp vty st inputRef histRef scrollRef resultRef mouseRef asyncRef scrollOffset inputTxt _dz = do
  if scrollOffset > 0
    then do
      -- Normal scroll up
      writeIORef scrollRef (scrollOffset - 1, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    else do
      -- At top - try to switch to previous document for multi-doc results
      resultDisplay <- readIORef resultRef
      case resultDisplay of
        ResultAffected ranges idx | idx > 0 -> do
          -- Switch to previous document
          let newIdx = idx - 1
          writeIORef resultRef (ResultAffected ranges newIdx)
          -- Set scroll to bottom of new document
          (width, height) <- Vty.displayBounds (Vty.outputIface vty)
          let newDz = resultDisplayToZone (ResultAffected ranges newIdx) st width (height - 1)
              maxScroll = max 0 (V.length (dzLines newDz) - dzHeight newDz)
          writeIORef scrollRef (maxScroll, inputTxt)
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
        _ ->
          -- Can't scroll further
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False


handleScrollDown :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> Int -> Text -> DisplayZone -> IO InputResult
handleScrollDown vty st inputRef histRef scrollRef resultRef mouseRef asyncRef scrollOffset inputTxt dz = do
  let maxScroll = max 0 (V.length (dzLines dz) - dzHeight dz)
  if scrollOffset < maxScroll
    then do
      -- Normal scroll down
      writeIORef scrollRef (scrollOffset + 1, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    else do
      -- At bottom - try to switch to next document for multi-doc results
      resultDisplay <- readIORef resultRef
      case resultDisplay of
        ResultAffected ranges idx | idx < length ranges - 1 -> do
          -- Switch to next document
          let newIdx = idx + 1
          writeIORef resultRef (ResultAffected ranges newIdx)
          writeIORef scrollRef (0, inputTxt)  -- Start at top of new document
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
        _ ->
          -- Can't scroll further
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False


-- Updates input immediately so single-click works even without mouse-up.
-- For drag: subsequent EvMouseDown events with BLeft update the range.
handleMouseDown :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> Int -> DisplayZone -> IO InputResult
handleMouseDown vty st inputRef histRef scrollRef resultRef mouseRef asyncRef row dz = do
  mouseState <- readIORef mouseRef
  let currentLineNum = getLineNumberAtRow row dz

  case currentLineNum of
    Nothing ->
      -- Clicked on non-line area (header, empty space) - ignore
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    Just lineNum ->
      case msClickStart mouseState of
        Nothing -> do
          -- New click - record start and set input to single line
          writeIORef mouseRef (MouseState (Just lineNum))
          let rangeText = makeRangeText lineNum lineNum
          updateInputWithRange inputRef rangeText
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
        Just startLine -> do
          -- Continuing drag - update range from start to current
          let rangeText = makeRangeText startLine lineNum
          updateInputWithRange inputRef rangeText
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False


handleMouseUp :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> Int -> DisplayZone -> IO InputResult
handleMouseUp vty st inputRef histRef scrollRef resultRef mouseRef asyncRef row dz = do
  mouseState <- readIORef mouseRef
  let endLineNum = getLineNumberAtRow row dz

  -- Clear mouse state (drag finished)
  writeIORef mouseRef emptyMouseState

  case (msClickStart mouseState, endLineNum) of
    (Just startLine, Just endLine) -> do
      -- Valid line selection - finalize range
      let rangeText = makeRangeText startLine endLine
      updateInputWithRange inputRef rangeText
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    (Nothing, Just endLine) -> do
      -- No start recorded (mouse-up without prior mouse-down) - single line
      let rangeText = makeRangeText endLine endLine
      updateInputWithRange inputRef rangeText
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    _ ->
      -- Released on non-line area - just clear state
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False




handleKey :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> Bool -> Vty.Key -> [Vty.Modifier] -> DisplayZone -> IO InputResult
handleKey vty st inputRef histRef scrollRef resultRef mouseRef asyncRef lastWasEsc key mods dz = do
  -- Clear mouse drag state on any key press
  writeIORef mouseRef emptyMouseState
  input <- readIORef inputRef
  let inputTxt = inputText input

  case (key, mods) of
    (Vty.KEnter, []) -> do
      let cmd = inputText input
      writeIORef inputRef emptyInput
      return (InputLine cmd)

    (Vty.KChar 'd', [Vty.MCtrl]) ->
      if T.null (inputText input)
        then return InputEOF
        else do
          writeIORef inputRef (deleteAt input)
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KChar 'c', [Vty.MCtrl]) ->
      -- Ctrl+C = interrupt, not clear
      return InputInterrupt

    (Vty.KEsc, []) ->
      if lastWasEsc
        then return InputExitVisual  -- Double ESC = exit
        else inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef True

    -- Scroll: PageUp = page up
    (Vty.KPageUp, []) -> do
      when (canScroll dz) $ do
        let scrolled = applyScroll ScrollPageUp dz
        writeIORef scrollRef (dzScrollTop scrolled, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Scroll: PageDown = page down
    (Vty.KPageDown, []) -> do
      when (canScroll dz) $ do
        let scrolled = applyScroll ScrollPageDown dz
        writeIORef scrollRef (dzScrollTop scrolled, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Scroll: Ctrl+B = page up (vi style)
    (Vty.KChar 'b', [Vty.MCtrl]) -> do
      when (canScroll dz) $ do
        let scrolled = applyScroll ScrollPageUp dz
        writeIORef scrollRef (dzScrollTop scrolled, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Scroll: Ctrl+F = page down (vi style)
    (Vty.KChar 'f', [Vty.MCtrl]) -> do
      when (canScroll dz) $ do
        let scrolled = applyScroll ScrollPageDown dz
        writeIORef scrollRef (dzScrollTop scrolled, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Scroll: Ctrl+Y = scroll up one line (vi style)
    (Vty.KChar 'y', [Vty.MCtrl]) -> do
      when (canScroll dz) $ do
        let scrolled = applyScroll ScrollLineUp dz
        writeIORef scrollRef (dzScrollTop scrolled, inputTxt)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Ctrl+E = move to end of line (emacs style)
    (Vty.KChar 'e', [Vty.MCtrl]) -> do
      writeIORef inputRef (moveEnd input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Ctrl+W = delete word before cursor (emacs style)
    (Vty.KChar 'w', [Vty.MCtrl]) -> do
      writeIORef inputRef (deleteWord input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KUp, []) -> do
      -- Navigate to older history entry
      hn <- readIORef histRef
      case historyUp input hn of
        Nothing -> inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False  -- No more history
        Just (newInput, newHn) -> do
          writeIORef inputRef newInput
          writeIORef histRef newHn
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KDown, []) -> do
      -- Navigate to newer history entry
      hn <- readIORef histRef
      case historyDown input hn of
        Nothing -> inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False  -- Already at newest
        Just (newInput, newHn) -> do
          writeIORef inputRef newInput
          writeIORef histRef newHn
          inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KBS, []) -> do
      writeIORef inputRef (deleteBack input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KDel, []) -> do
      writeIORef inputRef (deleteAt input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KLeft, []) -> do
      writeIORef inputRef (moveLeft input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KRight, []) -> do
      writeIORef inputRef (moveRight input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KHome, []) -> do
      writeIORef inputRef (moveHome input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KEnd, []) -> do
      writeIORef inputRef (moveEnd input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KChar 'a', [Vty.MCtrl]) -> do
      writeIORef inputRef (moveHome input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KChar 'k', [Vty.MCtrl]) -> do
      writeIORef inputRef (killToEnd input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KChar 'u', [Vty.MCtrl]) -> do
      writeIORef inputRef (killToStart input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    -- Tab completion
    (Vty.KChar '\t', []) -> do
      handleCompletion vty st inputRef histRef scrollRef resultRef mouseRef asyncRef

    (Vty.KChar c, []) -> do
      writeIORef inputRef (insertChar c input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    (Vty.KChar c, [Vty.MShift]) -> do
      writeIORef inputRef (insertChar c input)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False

    _ -> inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False


handleCompletion :: Vty.Vty -> LedState -> IORef InputState -> IORef HistoryNav -> IORef (Int, Text) -> IORef ResultDisplay -> IORef MouseState -> IORef CachedGlobalMatches -> IO InputResult
handleCompletion vty st inputRef histRef scrollRef resultRef mouseRef asyncRef = do
  input <- readIORef inputRef
  result <- performCompletion input
  case result of
    NoCompletions ->
      -- No completions - ring bell (visual feedback via re-render)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    SingleCompletion newInput -> do
      writeIORef inputRef newInput
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    MultipleWithPrefix newInput -> do
      writeIORef inputRef newInput
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False
    MultipleNoPrefix completionLines -> do
      writeIORef resultRef (ResultText completionLines)
      inputLoop vty st inputRef histRef scrollRef resultRef mouseRef asyncRef False


-- This reads from vty, showing a live preview of lines being added.
-- scrollRef is shared across all lines to preserve scroll position.
getVisualLine :: Vty.Vty -> LedState -> IORef InputState -> IORef ContinuationContext -> IORef (Int, Text) -> IO (Maybe String)
getVisualLine vty st inputRef ctxRef scrollRef = do
  writeIORef inputRef emptyInput
  result <- continuationLoop vty st inputRef ctxRef scrollRef
  case result of
    Nothing -> return Nothing
    Just line -> do
      -- Add this line to the continuation context for future preview
      modifyIORef ctxRef (\ctx -> ctx { ccLines = line : ccLines ctx })
      return (Just (toString line))


continuationLoop :: Vty.Vty -> LedState -> IORef InputState -> IORef ContinuationContext -> IORef (Int, Text) -> IO (Maybe Text)
continuationLoop vty st inputRef ctxRef scrollRef = do
  input <- readIORef inputRef
  ctx <- readIORef ctxRef

  -- Get terminal dimensions
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)
  let displayHeight = height - 1

  -- Check if input changed (for scroll preservation)
  let currentInput = inputText input
  (scrollOffset, lastInputTxt) <- readIORef scrollRef
  let inputChanged = currentInput /= lastInputTxt

  -- Generate preview based on continuation context
  let -- Lines accumulated so far plus current input line
      allNewLines = reverse (ccLines ctx) ++ [currentInput]
      mPrevLineRange = ledLastLineRange st
      dl = ledDocumentList st
      dz = case ccGlobalInfo ctx of
        -- Global command: use global preview with matches
        Just gci ->
          let curDoc = dlCurrentDoc dl
              matches = gciMatches gci
              cmdChar = case ccType ctx of
                ContAppend -> "a"
                ContInsert -> "i"
                ContChange -> "c"
                ContNone -> ""
              cmdText = cmdChar <> "\n" <> T.intercalate "\n" allNewLines
          in showGlobalCommandResultsWithMatches curDoc matches (gciPattern gci) cmdText dl width displayHeight
        -- Regular command: use normal preview
        Nothing -> case ccType ctx of
          ContAppend ->
            let (docRange, lineRange) = ccRange ctx
            in generateDisplayForParse (PPAppendText docRange lineRange allNewLines)
                 dl (ledParamStack st) mPrevLineRange width displayHeight
          ContInsert ->
            let (docRange, lineRange) = ccRange ctx
            in generateDisplayForParse (PPInsertText docRange lineRange allNewLines)
                 dl (ledParamStack st) mPrevLineRange width displayHeight
          ContChange ->
            let (docRange, lineRange) = ccRange ctx
            in generateDisplayForParse (PPChangeText docRange lineRange allNewLines)
                 dl (ledParamStack st) mPrevLineRange width displayHeight
          ContNone ->
            -- No context - show empty
            textLinesToDisplayZone [] width displayHeight
      wrappedDz = wrapDisplayZone dz

      -- Preserve scroll if target range is already visible
      -- scrollOffset < 0 means first iteration - use computed scroll or check visibility at 0
      scrollOffset' = if scrollOffset < 0
        then case dzTargetRange wrappedDz of
          Nothing -> dzScrollTop wrappedDz
          Just (targetStart, targetEnd) ->
            if isRangeVisible targetStart targetEnd 0 displayHeight
              then 0  -- Range visible at top, stay at top
              else dzScrollTop wrappedDz  -- Use computed scroll
        else if inputChanged
        then case dzTargetRange wrappedDz of
          Nothing -> dzScrollTop wrappedDz
          Just (targetStart, targetEnd) ->
            if isRangeVisible targetStart targetEnd scrollOffset displayHeight
              then scrollOffset  -- Range already visible, keep current scroll
              else dzScrollTop wrappedDz  -- Range not visible, scroll to show it
        else scrollOffset  -- Input unchanged, keep scroll position
      finalDz = wrappedDz { dzScrollTop = scrollOffset' }

  writeIORef scrollRef (scrollOffset', currentInput)
  renderScreenWithZone vty st input finalDz False

  ev <- Vty.nextEvent vty
  case ev of
    Vty.EvKey Vty.KEnter [] -> do
      let line = inputText input
      writeIORef inputRef emptyInput
      return (Just line)

    Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] ->
      if T.null (inputText input)
        then return Nothing
        else do
          modifyIORef inputRef deleteAt
          continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
      writeIORef inputRef emptyInput
      continuationLoop vty st inputRef ctxRef scrollRef

    -- Ctrl+A = move to beginning of line (emacs style)
    Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl] -> do
      modifyIORef inputRef moveHome
      continuationLoop vty st inputRef ctxRef scrollRef

    -- Ctrl+E = move to end of line (emacs style)
    Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl] -> do
      modifyIORef inputRef moveEnd
      continuationLoop vty st inputRef ctxRef scrollRef

    -- Ctrl+W = delete word before cursor (emacs style)
    Vty.EvKey (Vty.KChar 'w') [Vty.MCtrl] -> do
      modifyIORef inputRef deleteWord
      continuationLoop vty st inputRef ctxRef scrollRef

    -- Ctrl+U = delete to beginning of line (emacs style)
    Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl] -> do
      modifyIORef inputRef killToStart
      continuationLoop vty st inputRef ctxRef scrollRef

    -- Ctrl+K = delete to end of line (emacs style)
    Vty.EvKey (Vty.KChar 'k') [Vty.MCtrl] -> do
      modifyIORef inputRef killToEnd
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey Vty.KBS [] -> do
      modifyIORef inputRef deleteBack
      continuationLoop vty st inputRef ctxRef scrollRef

    -- Tab completion (must be before general KChar)
    Vty.EvKey (Vty.KChar '\t') [] -> do
      handleContinuationCompletion vty st inputRef ctxRef scrollRef

    Vty.EvKey (Vty.KChar c) [] -> do
      modifyIORef inputRef (insertChar c)
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey (Vty.KChar c) [Vty.MShift] -> do
      modifyIORef inputRef (insertChar c)
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey Vty.KLeft [] -> do
      modifyIORef inputRef moveLeft
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey Vty.KRight [] -> do
      modifyIORef inputRef moveRight
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey Vty.KHome [] -> do
      modifyIORef inputRef moveHome
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvKey Vty.KEnd [] -> do
      modifyIORef inputRef moveEnd
      continuationLoop vty st inputRef ctxRef scrollRef

    Vty.EvResize _ _ ->
      continuationLoop vty st inputRef ctxRef scrollRef

    _ -> continuationLoop vty st inputRef ctxRef scrollRef


handleContinuationCompletion :: Vty.Vty -> LedState -> IORef InputState -> IORef ContinuationContext -> IORef (Int, Text) -> IO (Maybe Text)
handleContinuationCompletion vty st inputRef ctxRef scrollRef = do
  input <- readIORef inputRef
  result <- performCompletion input
  case result of
    NoCompletions ->
      continuationLoop vty st inputRef ctxRef scrollRef
    SingleCompletion newInput -> do
      writeIORef inputRef newInput
      continuationLoop vty st inputRef ctxRef scrollRef
    MultipleWithPrefix newInput -> do
      writeIORef inputRef newInput
      continuationLoop vty st inputRef ctxRef scrollRef
    MultipleNoPrefix _ ->
      -- No common prefix - just continue (continuation mode doesn't show completion list)
      continuationLoop vty st inputRef ctxRef scrollRef


-- | Detect continuation type and global info from command text.
-- Returns (contType, docRange, lineRange, globalInfo)
detectContinuationType :: Text -> LedState -> (ContinuationType, DocRange, LineRange, Maybe GlobalContInfo)
detectContinuationType cmdText st =
  let userFns = Set.fromList $ Map.keys (ledDefinedFunctions st)
  in case parsePartialWithFns userFns cmdText of
    PPCommand (Append (FullRange docRange lineRange) _ _) ->
      (ContAppend, docRange, lineRange, Nothing)
    PPCommand (Insert (FullRange docRange lineRange) _ _) ->
      (ContInsert, docRange, lineRange, Nothing)
    PPCommand (Change (FullRange docRange lineRange) _ _) ->
      (ContChange, docRange, lineRange, Nothing)
    -- Global command with block text sub-command
    PPCommand (Global (FullRange docRange lineRange) pat cmdlist) ->
      detectGlobalBlockText False docRange lineRange pat cmdlist st
    PPCommand (GlobalReverse (FullRange docRange lineRange) pat cmdlist) ->
      detectGlobalBlockText True docRange lineRange pat cmdlist st
    -- Also check partial global commands
    PPGlobalCommand docRange lineRange _delim pat cmdlist ->
      let invert = isInvertedGlobal cmdText
      in detectGlobalBlockText invert docRange lineRange pat cmdlist st
    _ -> (ContNone, DocDefault, LineDefault, Nothing)

-- | Check if command text starts with v or V (inverted global)
isInvertedGlobal :: Text -> Bool
isInvertedGlobal t =
  let stripped = T.dropWhile (\c -> c == ',' || c == ';' || c == '.' || c == '$' ||
                                     c == '+' || c == '-' || c == '\'' || c == ':' ||
                                     (c >= '0' && c <= '9')) t
  in case T.uncons stripped of
       Just (c, _) -> c == 'v' || c == 'V'
       _ -> False

-- | Detect if global command has block text sub-command (a/i/c)
detectGlobalBlockText :: Bool -> DocRange -> LineRange -> Text -> Text -> LedState
                      -> (ContinuationType, DocRange, LineRange, Maybe GlobalContInfo)
detectGlobalBlockText invert docRange lineRange pat cmdlist st =
  let subCmd = T.strip cmdlist
  in case T.uncons subCmd of
       Just ('a', rest) | isBlockTextCmd rest -> makeGlobalCont ContAppend
       Just ('i', rest) | isBlockTextCmd rest -> makeGlobalCont ContInsert
       Just ('c', rest) | isBlockTextCmd rest -> makeGlobalCont ContChange
       _ -> (ContNone, DocDefault, LineDefault, Nothing)
  where
    -- Check that after command letter there's only optional suffix (p/n/l) or nothing
    isBlockTextCmd rest = T.null rest || T.all (`elem` ("pnl" :: String)) rest

    makeGlobalCont contType =
      let matches = computeGlobalMatchesForCont invert pat docRange lineRange st
          gci = GlobalContInfo { gciPattern = pat, gciMatches = matches, gciInvert = invert }
      in (contType, docRange, lineRange, Just gci)

-- | Compute matching line numbers for global command continuation preview
computeGlobalMatchesForCont :: Bool -> Text -> DocRange -> LineRange -> LedState -> [Int]
computeGlobalMatchesForCont invert pat docRange lineRange st =
  let dl = ledDocumentList st
      curDoc = dlCurrentDoc dl
      matchSense = not invert  -- g/ matches True, v/ matches False
  in case docRange of
       DocDefault ->
         case getDocStateAt curDoc dl of
           Nothing -> []
           Just docState ->
             let doc = docDocument docState
                 total = lineCount doc
                 curLine = docCurrentLine docState
                 (startLine, endLine) = case lineRange of
                   LineDefault -> if total == 0 then (0, 0) else (1, total)
                   _ -> case resolveLineRange curLine total (docMarks docState) (documentLines doc) lineRange of
                     Left _ -> (1, total)
                     Right (s, e) -> (s, e)
             in case findMatchingLineNumbers matchSense pat startLine endLine doc of
                  Left _ -> []
                  Right matches -> matches
       _ -> []  -- Multi-doc global commands not supported in continuation preview yet


-- Returns (result display, new state, should quit).
processLine :: Vty.Vty -> LedEnv -> IORef InputState -> LedState -> Text -> IO (ResultDisplay, LedState, Bool)
processLine vty env inputRef st lineText = do
  -- Enable output capture, reset command error for this command
  -- (same as non-visual mode does in repl)
  let st1 = st { ledCaptureOutput = Just [], ledCommandError = False }
      wasVisual = ledVisualMode st

  -- Detect if this is a text input command (append/insert/change)
  let (contType, docRange, lineRange, mGlobalInfo) = detectContinuationType lineText st

  -- Create continuation context ref
  ctxRef <- newIORef ContinuationContext
    { ccType = contType
    , ccRange = (docRange, lineRange)
    , ccLines = []
    , ccGlobalInfo = mGlobalInfo
    }

  -- Create scroll ref that persists across all lines of input
  -- Start with -1 marker to indicate first iteration needs computed scroll
  scrollRef <- newIORef (-1 :: Int, "" :: Text)

  -- Create env with visual input hook that uses the context
  let visualEnv = env { leVisualInput = Just (getVisualLine vty st inputRef ctxRef scrollRef) }

  -- Process using normal Led infrastructure
  -- processWithUndo returns:
  --   (True, _)  = continue REPL
  --   (False, _) = quit (from q/Q command) or mode change (vi command)
  --   (_, Just affected) = lines were modified
  ((shouldContinue, mAffected), st2) <- runLed visualEnv st1 $ do
    -- Read continuations (for a, i, c, etc.) - uses visual input hook
    fullLine <- readBlockContinuation lineText >>= readSubContinuation >>= readBackslashContinuation

    -- Process with undo tracking - returns whether to continue and affected range
    result <- processWithUndo fullLine

    -- Ensure non-empty doc list
    ensureNonEmptyDocList

    pure result

  -- Get captured output (split any embedded newlines into separate lines)
  let rawCaptured = reverse $ fromMaybe [] (ledCaptureOutput st2)
      capturedLines = concatMap T.lines rawCaptured
      st3 = st2 { ledCaptureOutput = Nothing }

  -- Determine if this is a quit or mode change:
  -- - If shouldContinue = False and visual mode changed, it's mode change (not quit)
  -- - If shouldContinue = False and visual mode unchanged, it's a quit
  let nowVisual = ledVisualMode st3
      modeChanged = wasVisual /= nowVisual
      shouldQuit = not shouldContinue && not modeChanged

  -- Check for error to determine what to display
  if ledCommandError st3
    then do
      let errMsg = if ledHelpMode st3
                   then fromMaybe "?" (ledLastError st3)
                   else "?"
      return (ResultText [errMsg], st3, shouldQuit)
    else do
      -- If there was captured output, show it as text
      -- Otherwise, if lines were affected, return affected ranges for highlighting
      let result = case (capturedLines, mAffected) of
            (lines', _) | not (null lines') -> ResultText lines'
            (_, affected) | not (null affected) -> ResultAffected affected 0
            _ -> ResultEmpty
      return (result, st3, shouldQuit)


-- Returns (continue flag, list of affected ranges if changes were made).
processWithUndo :: Text -> Led (Bool, [LedUndo.AffectedRange])
processWithUndo input = do
  depth <- gets ledUndoDepth
  mgr <- gets ledUndoManager
  let stripped = T.strip input
      isUndo = stripped == "u"
      isRedo = stripped == "U"
      isUndoRedo = isUndo || isRedo

  -- For undo/redo at top level, peek at the stack to get the step
  mUndoRedoStep <- if depth == 0 && isUndoRedo
    then liftIO $ do
      if isUndo
        then do
          stack <- readIORef (umUndoStack mgr)
          pure (viaNonEmpty head stack)
        else do
          stack <- readIORef (umRedoStack mgr)
          pure (viaNonEmpty head stack)
    else pure Nothing

  when (depth == 0 && not isUndoRedo) $ do
    dl <- gets ledDocumentList
    liftIO $ LedUndo.capturePreState mgr dl

  result <- processInput input

  affected <- if depth == 0
    then case mUndoRedoStep of
      Just step ->
        -- Undo/redo: compute affected ranges from the step
        pure (LedUndo.getUndoAffectedRanges step)
      Nothing -> do
        -- Normal command: commit and get affected ranges
        dl <- gets ledDocumentList
        mStep <- liftIO $ LedUndo.commitUndoStep mgr dl
        case mStep of
          Nothing -> pure []
          Just step -> do
            liftIO $ LedUndo.clearRedoStack mgr
            pure (LedUndo.getAffectedRanges step)
    else pure []

  pure (result, affected)


renderScreenWithResult :: Vty.Vty -> LedState -> InputState -> ResultDisplay -> Bool -> IO ()
renderScreenWithResult vty st input resultDisplay isError = do
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)
  let displayHeight = height - 1
      dz = wrapDisplayZone $ resultDisplayToZone resultDisplay st width displayHeight
  renderScreenWithZone vty st input dz isError


renderScreenWithZone :: Vty.Vty -> LedState -> InputState -> DisplayZone -> Bool -> IO ()
renderScreenWithZone vty st input dz isError = do
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)

  -- Display zone (height - 1 lines, black background)
  let displayHeight = height - 1
      displayImage = renderDisplayZoneFromDZ dz isError width displayHeight

  -- Input line (default background, red on error)
  let prompt = getPrompt st
      inputImage = renderInputLine prompt input width isError

  -- Combine
  let fullImage = displayImage Vty.<-> inputImage

  -- Cursor position (accounting for virtual line wrapping)
  let (_, visibleCursor) = calcVirtualLine prompt input width
      cursorCol = visibleCursor
      cursorRow = height - 1

  let pic = Vty.Picture
        { Vty.picCursor = Vty.Cursor cursorCol cursorRow
        , Vty.picLayers = [fullImage]
        , Vty.picBackground = Vty.Background ' ' Vty.defAttr
        }

  Vty.update vty pic


-- Note: isError is ignored here - only the input line should turn red on error.
--
-- Layout: 8-column gutter (default bg) + content area (black bg)
-- - Lines with dlLineNum: gutter shows line number, content area shows text
-- - Command output with tab: gutter shows text up to tab (n command line numbers), rest in content
-- - Other command output: empty gutter, text in content area
renderDisplayZoneFromDZ :: DisplayZone -> Bool -> Int -> Int -> Vty.Image
renderDisplayZoneFromDZ dz _isError width height =
  let allLines = V.toList (dzLines dz)
      -- Apply scroll offset
      visibleLines = take height $ drop (dzScrollTop dz) allLines
      -- Gutter (8 cols) uses default background with white text
      gutterAttr = Vty.defAttr `Vty.withForeColor` Vty.white
      gutterWidth = 8
      contentWidth = width - gutterWidth
      -- Right-pad for left alignment (like ,n command)
      rightPad n s = s ++ replicate (n - length s) ' '
      -- Render each line
      renderLine dl' = case dlLineNum dl' of
        -- Line with explicit line number: show in gutter (left-aligned like ,n command)
        Just n ->
          let lineNumStr = rightPad gutterWidth (show n)
              content = expandTabs (T.unpack (dlText dl'))
              truncated = take contentWidth content
              lineAttr = styleToAttr (dlStyle dl')
              contentImg = renderWithHighlights lineAttr truncated (dlHighlights dl') contentWidth
          in Vty.string gutterAttr lineNumStr Vty.<|> contentImg
        -- No explicit line number: check for tab (n command output)
        Nothing ->
          let rawText = T.unpack (dlText dl')
          in case break (== '\t') rawText of
            -- Has tab: text before tab goes in gutter (n command), rest in content
            (beforeTab, '\t':afterTab) ->
              let gutterStr = rightPad gutterWidth beforeTab
                  content = expandTabs afterTab
                  truncated = take contentWidth content
                  lineAttr = styleToAttr (dlStyle dl')
                  contentImg = renderWithHighlights lineAttr truncated (dlHighlights dl') contentWidth
              in Vty.string gutterAttr gutterStr Vty.<|> contentImg
            -- No tab: empty gutter, all text in content area
            _ ->
              let content = expandTabs rawText
                  truncated = take contentWidth content
                  lineAttr = styleToAttr (dlStyle dl')
                  contentImg = renderWithHighlights lineAttr truncated (dlHighlights dl') contentWidth
              in Vty.string gutterAttr (replicate gutterWidth ' ') Vty.<|> contentImg

      -- Render text with highlights
      renderWithHighlights baseAttr text highlights maxWidth =
        if null highlights
          then let padding = replicate (maxWidth - length text) ' '
               in Vty.string baseAttr (text ++ padding)
          else
            let sortedHls = sortOn (\(s,_,_) -> s) highlights
                segments = buildSegments baseAttr text sortedHls 0
                imgs = map (\(txt, attr) -> Vty.string attr txt) segments
                totalLen = sum (map (length . fst) segments)
                padding = replicate (maxWidth - totalLen) ' '
            in Vty.horizCat imgs Vty.<|> Vty.string baseAttr padding

      -- Build segments with appropriate attributes
      buildSegments :: Vty.Attr -> String -> [(Int, Int, HighlightStyle)] -> Int -> [(String, Vty.Attr)]
      buildSegments _ "" _ _ = []
      buildSegments bAttr remaining [] _ = [(remaining, bAttr)]
      buildSegments bAttr remaining ((hlStart, hlEnd, hlStyle):rest) pos
        | pos < hlStart =
            -- Text before highlight
            let beforeLen = hlStart - pos
                (before, after) = splitAt beforeLen remaining
            in (before, bAttr) : buildSegments bAttr after ((hlStart, hlEnd, hlStyle):rest) hlStart
        | pos < hlEnd =
            -- Text in highlight
            let hlLen = hlEnd - pos
                (hlText, after) = splitAt hlLen remaining
                hlAttr = highlightToAttr hlStyle
            in (hlText, hlAttr) : buildSegments bAttr after rest hlEnd
        | otherwise =
            -- Move to next highlight
            buildSegments bAttr remaining rest pos
      lineImages = map renderLine visibleLines
      -- Pad with empty lines (8 cols default bg + rest black bg)
      emptyGutter = Vty.string gutterAttr (replicate gutterWidth ' ')
      emptyContent = Vty.string displayAttr (replicate contentWidth ' ')
      emptyLine = emptyGutter Vty.<|> emptyContent
      paddingCount = height - length lineImages
      paddingImages = replicate paddingCount emptyLine
  in Vty.vertCat (lineImages ++ paddingImages)


styleToAttr :: LineStyle -> Vty.Attr
styleToAttr StyleNormal = displayAttr
styleToAttr StyleAdded = Vty.defAttr
  `Vty.withForeColor` Vty.green
  `Vty.withBackColor` Vty.black
styleToAttr StyleDeleted = Vty.defAttr
  `Vty.withForeColor` Vty.ISOColor 245  -- Grey
  `Vty.withBackColor` Vty.black
  `Vty.withStyle` Vty.dim
  `Vty.withStyle` Vty.italic
styleToAttr StyleHeader = Vty.defAttr
  `Vty.withForeColor` Vty.white
  `Vty.withBackColor` Vty.ISOColor 17  -- Dark blue
  `Vty.withStyle` Vty.bold
styleToAttr StyleSelected = displayAttr `Vty.withStyle` Vty.reverseVideo


highlightToAttr :: HighlightStyle -> Vty.Attr
highlightToAttr HLMatch = Vty.defAttr
  `Vty.withForeColor` Vty.ISOColor 245  -- Grey
  `Vty.withBackColor` Vty.black
  `Vty.withStyle` Vty.strikethrough
highlightToAttr HLReplacement = Vty.defAttr
  `Vty.withForeColor` Vty.black
  `Vty.withBackColor` Vty.yellow
  `Vty.withStyle` Vty.italic
highlightToAttr HLSelection = displayAttr `Vty.withStyle` Vty.reverseVideo




expandTabs :: String -> String
expandTabs = go 0
  where
    go _ [] = []
    go col ('\t':rest) =
      let spaces = 8 - (col `mod` 8)
      in replicate spaces ' ' ++ go (col + spaces) rest
    go col (c:rest) = c : go (col + 1) rest


-- Returns (visible text, cursor column within visible text)
calcVirtualLine :: String -> InputState -> Int -> (Text, Int)
calcVirtualLine prompt (InputState before after) width =
  let promptLen = length prompt
      beforeText = T.reverse before
      afterText = after
      fullText = toText prompt <> beforeText <> afterText
      cursorPos = promptLen + T.length beforeText  -- Cursor position in full text
      -- Calculate which virtual line the cursor is on
      virtualLineNum = if width > 0 then cursorPos `div` width else 0
      startOffset = virtualLineNum * width
      -- Extract visible portion
      visibleText = T.take width (T.drop startOffset fullText)
      visibleCursor = cursorPos - startOffset
  in (visibleText, visibleCursor)

renderInputLine :: String -> InputState -> Int -> Bool -> Vty.Image
renderInputLine prompt input width isError =
  let attr = if isError then errorAttr else inputAttr
      (visibleText, _) = calcVirtualLine prompt input width
      textImg = Vty.text' attr visibleText
      textLen = T.length visibleText
      paddingLen = max 0 (width - textLen)
      paddingImg = Vty.string attr (replicate paddingLen ' ')
  in Vty.horizCat [textImg, paddingImg]


getPrompt :: LedState -> String
getPrompt st
  | ledPromptActive st = toString (ledPrompt st)
  | otherwise = ""
