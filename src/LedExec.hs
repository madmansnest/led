module LedExec
  ( processInput, processInputDirect, processInputParsed
  , readBlockContinuation, readSubContinuation, readBackslashContinuation
  , expandExpressions, expandExpressionsRaw, findGlobalCmdlist
  , evaluateExpression, evaluatePrompt
  , invokeFunctionWithParams, executeFunctionBody
  , processFunctionQueue, processFunctionQueueDirect, processFunctionQueueWith
  , executeCommand
  , getCurrentLineDefault, getLineCountDefault, getCurrentLineRange, getCurrentAndNextRange
  , handleLineCommand, handleParamCommand
  , handleDocListCommand, handleManageCommand, handleModifiedCommand
  , handleCrossDocCommand, executeCrossDocLineCommand
  , guardAllChanged
  , importMacroCommand, importFromFile, importFromShell
  ) where

import Data.Char (isAlpha, isDigit)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)

import LedParse (Addr(..), Command(..), DocRange(..), FullRange(..), LineRange(..), ParseResult(..), Suffix(..), TargetAddr(..), parseCommand, getCommandRange, setCommandRange)
import LedResolve (resolveAddr, resolveDocRange, resolveLineRange)
import LedCore (LedState(..))
import LedNexus (BufferChangeFlag(..), DocumentState(..), emptyDocumentState, documentCount, getDocStateAt, setDocStateAt, insertDocAfter, deleteDocRange, dlDocList, dlCurrentDoc, dlDocListState, setDlCurrentDoc, unsavedDocuments, changedDocuments)
import qualified LedDocument
import LedIO (readTextFile, runShellCommand, expandPath, humanisePath)
import LedInput (Led, getLineFromQueueOrInput, readInputText, outputLine, outputStrLn')
import LedGlobal (GlobalHandlers(..), executeGlobal, executeInteractiveGlobal)
import LedState hiding (resolveTargetAddress)
import LedEdit
import LedPrint
import LedReadWrite
import LedSession

processInput :: Text -> Led Bool
processInput input = do
  modify (\s -> s { ledCurrentInputLine = Just (T.strip input) })
  expanded <- if isFnDefinition input then pure input else expandExpressions input
  processInputParsed expanded

processInputDirect :: Text -> Led Bool
processInputDirect input = do
  modify (\s -> s { ledCurrentInputLine = Just (T.strip input) })
  processInputParsed input

processInputParsed :: Text -> Led Bool
processInputParsed input = do
    fnNames <- Set.fromList . Map.keys <$> gets ledDefinedFunctions
    case parseCommand fnNames input of
      Failed err -> handleParseError err
      Incomplete -> handleParseError "Incomplete command"
      Complete cmd -> handleCommandWithPrevRange cmd
  where
    handleParseError err = do
      addressError err
      gets ledIsInteractive

    handleCommandWithPrevRange cmd
      | isEmptyPrint cmd = handleLineCommand defaultNextLine (PrintLines (FullRange DocDefault LineDefault) PrintSuffix)
      | otherwise = case getCommandRange cmd of
          Nothing -> executeCommand cmd
          Just (FullRange docRange lineRange) -> do
            mDocRange <- substitutePrevDocRange docRange
            mLineRange <- substitutePrevLineRange lineRange
            case (mDocRange, mLineRange) of
              (Nothing, _) -> addressError "No previous document range" *> pure True
              (_, Nothing) -> addressError "No previous line range" *> pure True
              (Just docR, Just lineR) -> do
                savePrevRanges docR lineR
                let cmd' = setCommandRange (FullRange docR lineR) cmd
                dispatchCommand docR lineR cmd'

    dispatchCommand docR lineR cmd = case docR of
      DocDefault -> handleLineCommand lineR cmd
      DocAll -> handleDocListCommand lineR cmd
      DocManage -> handleManageCommand lineR cmd
      DocModified -> handleModifiedCommand lineR cmd
      DocParam -> handleParamCommand lineR cmd
      _ -> handleCrossDocCommand docR lineR cmd

    isEmptyPrint (PrintLines (FullRange DocDefault LineDefault) PrintSuffix) = T.null (T.strip input)
    isEmptyPrint _ = False

    defaultNextLine = LineSingle (AddrOffset Current 1)

substitutePrevDocRange :: DocRange -> Led (Maybe DocRange)
substitutePrevDocRange DocPrevious = gets ledLastDocRange
substitutePrevDocRange other = pure (Just other)

substitutePrevLineRange :: LineRange -> Led (Maybe LineRange)
substitutePrevLineRange LinePrevious = gets ledLastLineRange
substitutePrevLineRange other = pure (Just other)

savePrevRanges :: DocRange -> LineRange -> Led ()
savePrevRanges docR lineR = do
  when (docR /= DocDefault) $
    modify (\s -> s { ledLastDocRange = Just docR })
  when (lineR /= LineDefault) $
    modify (\s -> s { ledLastLineRange = Just lineR })

readBlockContinuation :: Text -> Led Text
readBlockContinuation initial =
  let n = countUnmatchedBraces initial
  in if n <= 0
     then pure initial
     else go initial n
  where
    go acc depth = do
      mline <- getLineFromQueueOrInput
      case mline of
        Nothing -> pure acc
        Just line -> do
          let lineText = toText line
              acc' = acc <> "\n" <> lineText
              depth' = depth + countUnmatchedBraces lineText
          if depth' <= 0
            then pure acc'
            else go acc' depth'

readSubContinuation :: Text -> Led Text
readSubContinuation line =
  if needsSubContinuation line
    then do
      mNext <- getLineFromQueueOrInput
      case mNext of
        Nothing -> pure line
        Just next -> readSubContinuation (line <> "\n" <> toText next)
    else pure line

-- | Read backslash continuation lines (for g command and fn delimiter syntax)
readBackslashContinuation :: Text -> Led Text
readBackslashContinuation line =
  if endsWithOddBackslashes line
    then do
      mNext <- getLineFromQueueOrInput
      case mNext of
        Nothing -> pure line
        Just next -> readBackslashContinuation (line <> "\n" <> toText next)
    else pure line
  where
    endsWithOddBackslashes t = odd (T.length (T.takeWhileEnd (== '\\') (T.stripEnd t)))

expandExpressions :: Text -> Led Text
expandExpressions input = case findGlobalCmdlist input of
    Just (before, cmdlist) -> do
      -- Expand expressions only in the part before the command list
      expandedBefore <- expandExpressionsRaw before
      -- Return with unexpanded command list preserved
      pure (expandedBefore <> cmdlist)
    Nothing -> expandExpressionsRaw input

-- | Core expression expansion logic
expandExpressionsRaw :: Text -> Led Text
expandExpressionsRaw input = go "" input
  where
    go acc t = case T.uncons t of
      Nothing -> pure acc
      Just ('\\', r) -> case T.uncons r of
        Just ('{', r') -> go (acc <> "{") r'
        Just ('}', r') -> go (acc <> "}") r'
        Just (c, r')   -> go (acc <> "\\" <> T.singleton c) r'
        Nothing        -> pure (acc <> "\\")
      Just ('{', r) -> do
        case findMatchingBrace r 0 of
          Nothing -> pure (acc <> "{" <> r)
          Just (inner, rest) -> do
            expandedInner <- expandExpressionsRaw inner
            result <- evaluateExpression expandedInner
            go (acc <> result) rest
      Just (c, r) -> go (acc <> T.singleton c) r

-- | Find the command list portion of a global command, returning (before, cmdlist).
-- Global commands: [range][docprefix]g/re/cmdlist or v/re/cmdlist (also G/V)
-- Returns Nothing if not a global command or no command list.
findGlobalCmdlist :: Text -> Maybe (Text, Text)
findGlobalCmdlist input = do
    let stripped = T.stripStart input
    -- Skip optional doc prefix (& or &&) and range
    let (_prefix, afterPrefix) = skipDocPrefixAndRange stripped
    -- Look for g, G, v, or V command
    (cmdChar, afterCmd) <- T.uncons afterPrefix
    guard (cmdChar `elem` ['g', 'G', 'v', 'V'])
    -- Get delimiter
    (delim, afterDelim) <- T.uncons afterCmd
    guard (not (isAlphaNum delim))
    -- Find matching delimiter for the regex
    let (_regex, afterRegex) = T.breakOn (T.singleton delim) afterDelim
    -- Skip the closing delimiter
    rest <- T.stripPrefix (T.singleton delim) afterRegex
    -- If there's a command list, return split point
    if T.null rest
      then Nothing  -- No command list (e.g., just g/re/ or G/re/)
      else do
        -- Calculate split point: everything up to and including second delimiter
        let beforeLen = T.length input - T.length rest
            before = T.take beforeLen input
        Just (before, rest)
  where
    isAlphaNum c = isAlpha c || isDigit c
    skipDocPrefixAndRange t =
      let t1 = case T.stripPrefix "&&" t of
                 Just r -> T.stripStart r
                 Nothing -> case T.stripPrefix "&*" t of
                   Just r -> T.stripStart r
                   Nothing -> case T.stripPrefix "&" t of
                     Just r -> T.stripStart r
                     Nothing -> t
          -- Skip @ prefix
          t2 = case T.uncons t1 of
                 Just ('@', r) -> T.stripStart r
                 _ -> t1
          -- Skip range (addresses, commas, semicolons)
          (_, t3) = skipRange t2
      in (T.take (T.length input - T.length t3) input, T.stripStart t3)
    skipRange t = spanText isRangeChar (toString t)
      where isRangeChar c = isDigit c || c `elem` ['.', '$', '+', '-', ',', ';', '\'', ' ']
    spanText p s = let (a, b) = Prelude.span p s in (a, toText b)

evaluateExpression :: Text -> Led Text
evaluateExpression expr = do
  oldCapture <- gets ledCaptureOutput
  oldCmdError <- gets ledCommandError
  modify (\s -> s { ledCaptureOutput = Just [], ledCommandError = False })
  let lns = T.splitOn "\n" (T.strip expr)
  savedQueue <- gets ledInputQueue
  wasVisual <- gets ledVisualMode
  modify (\s -> s { ledInputQueue = lns })
  void processFunctionQueueDirect
  nowVisual <- gets ledVisualMode
  -- Preserve queue if visual mode changed (remaining commands for new mode)
  modify (\s -> s { ledInputQueue = if wasVisual == nowVisual then savedQueue else ledInputQueue s })
  captured <- gets ledCaptureOutput
  hadError <- gets ledCommandError
  modify (\s -> s { ledCaptureOutput = oldCapture, ledCommandError = oldCmdError })
  -- If error occurred, return empty string (don't substitute error output)
  let result = if hadError then "" else case captured of
        Just acc -> T.intercalate "\n" (reverse acc)
        Nothing  -> ""
  pure result

evaluatePrompt :: Led String
evaluatePrompt = do
  st <- get
  if ledPromptActive st
    then do
      let p = ledPrompt st
      if "{" `T.isInfixOf` p
        then toString <$> expandExpressions p
        else pure (toString p)
    else pure ""

invokeFunctionWithParams :: Text -> Text -> [Text] -> Led Bool
invokeFunctionWithParams name body args = do
  let paramDoc = if null args
        then emptyDocumentState
        else DocumentState
          { docDocument = LedDocument.fromLines args
          , docCurrentLine = length args
          , docMarks = Map.empty
          , docChangeFlag = Unchanged
          }
  pushParamDoc name paramDoc
  modify (\s -> s { ledUndoDepth = ledUndoDepth s + 1 })
  result <- executeFunctionBody body
  modify (\s -> s { ledUndoDepth = ledUndoDepth s - 1 })
  popParamDoc
  pure result

executeFunctionBody :: Text -> Led Bool
executeFunctionBody body = do
  let lns = T.splitOn "\n" body
  modify (\s -> s { ledInputQueue = lns ++ ledInputQueue s })
  processFunctionQueue

processFunctionQueue :: Led Bool
processFunctionQueue = processFunctionQueueWith processInput

processFunctionQueueDirect :: Led Bool
processFunctionQueueDirect = processFunctionQueueWith processInputDirect

processFunctionQueueWith :: (Text -> Led Bool) -> Led Bool
processFunctionQueueWith processor = do
  queue <- gets ledInputQueue
  case queue of
    [] -> pure True
    (x:xs) -> do
      modify (\s -> s { ledInputQueue = xs })
      wasVisual <- gets ledVisualMode
      fullLine <- readBlockContinuation x >>= readSubContinuation >>= readBackslashContinuation
      shouldContinue <- processor fullLine
      if shouldContinue
        then processFunctionQueueWith processor
        else do
          -- If visual mode changed, preserve queue for the new mode to process.
          -- Otherwise (quit command), clear the queue.
          nowVisual <- gets ledVisualMode
          when (wasVisual == nowVisual) $ modify (\s -> s { ledInputQueue = [] })
          pure False

-- | Execute a command and return whether to continue processing.
-- This is the central dispatch point for all commands.
executeCommand :: Command -> Led Bool
executeCommand = \case
    -- Quit commands
    Quit -> guardAllChanged >>= \ok -> pure (not ok)
    QuitAlways -> pure False
    WriteQuit range path -> do
      doc <- getDocument
      withLineRangeOr range (pure (1, LedDocument.lineCount doc)) $ \(s, e) ->
        resolveFilePath path >>= maybe (pure ()) (\p -> writeFileRange p s e)
      guardAllChanged >>= \ok -> pure (not ok)
    WriteQuitShell range cmd' -> do
      doc <- getDocument
      withLineRangeOr range (pure (1, LedDocument.lineCount doc)) $ \(s, e) -> writeToShell cmd' s e
      guardAllChanged >>= \ok -> pure (not ok)

    -- Edit commands (load new file)
    Edit _range path -> guardChanged >>= \ok ->
      if ok then resolveFilePath path >>= maybe (pure ()) editFile >> pure True
      else pure True
    EditAlways _range path -> resolveFilePath path >>= maybe (pure ()) editFileAlways >> pure True
    EditShell _range cmd' -> guardChanged >>= \ok ->
      if ok then editFromShell cmd' >> pure True else pure True
    EditShellAlways _range cmd' -> editFromShell cmd' *> pure True

    -- Global commands
    Global range re cmdlist -> executeGlobal globalHandlers True (frLines range) re cmdlist *> pure True
    GlobalReverse range re cmdlist -> executeGlobal globalHandlers False (frLines range) re cmdlist *> pure True
    GlobalInteractive range re -> executeInteractiveGlobal globalHandlers True (frLines range) re *> pure True
    GlobalReverseInteractive range re -> executeInteractiveGlobal globalHandlers False (frLines range) re *> pure True

    -- Visual mode toggle
    VisualMode -> do
      modify (\s -> s { ledVisualMode = not (ledVisualMode s) })
      pure False

    -- Text modification commands
    Append range txt suf -> do
      withLineAddress range True $ \addr -> do
        let lns = if T.null txt then [] else [txt]
        inputLines <- if T.null txt then readInputText else pure lns
        appendLinesAt addr inputLines *> printSuffix suf
      pure True
    Insert range txt suf -> do
      withLineAddress range True $ \addr -> do
        let lns = if T.null txt then [] else [txt]
        inputLines <- if T.null txt then readInputText else pure lns
        insertLinesAt addr inputLines *> printSuffix suf
      pure True
    Change range txt suf -> do
      withLineRange range $ \(s, e) -> do
        let inlineTxt = if T.null txt then Nothing else Just txt
        changeLinesWithText inlineTxt s e *> printSuffix suf
      pure True
    Delete range suf -> do
      withLineRange range $ \(s, e) -> deleteLines s e *> printSuffix suf
      pure True
    Join range suf -> do
      withLineRangeOr range getCurrentAndNextRange $ \(s, e) -> joinLines s e *> printSuffix suf
      pure True
    Substitute range re repl' flags suf -> do
      withLineRange range $ \(s, e) -> substituteCommand re repl' flags s e *> printSuffix suf
      pure True

    -- Line movement commands
    Transfer range target suf -> do
      withLineRange range $ \(s, e) -> executeTransfer target s e *> printSuffix suf
      pure True
    Move range target suf -> do
      withLineRange range $ \(s, e) -> executeMove target s e *> printSuffix suf
      pure True

    -- Print commands
    PrintLines range suf -> do
      withLineRange range $ \(s, e) -> printRange suf s e
      pure True
    PrintLineNumber range -> do
      withLineAddressOr range getLineCountDefault False $ \addr -> outputLine (show addr)
      pure True

    -- File I/O commands
    Write range path -> do
      doc <- getDocument
      withLineRangeOr range (pure (1, LedDocument.lineCount doc)) $ \(s, e) ->
        resolveFilePath path >>= maybe (pure ()) (\p -> writeFileRange p s e)
      pure True
    WriteShell range cmd' -> do
      doc <- getDocument
      withLineRangeOr range (pure (1, LedDocument.lineCount doc)) $ \(s, e) -> writeToShell cmd' s e
      pure True
    ReadFile range path -> do
      withLineAddressOr range getLineCountDefault True $ \addr -> readFileCommand path addr
      pure True
    ReadShell range cmd' -> do
      withLineAddressOr range getLineCountDefault True $ \addr -> readShellCommand cmd' addr
      pure True

    -- Shell commands
    ShellCommand cmd' -> shellCommand cmd' *> pure True
    ShellFilter range cmd' -> do
      withLineRange range $ \(s, e) -> filterLines s e cmd'
      pure True

    -- Undo/Redo
    Undo _range -> undoCommand *> pure True
    Redo _range -> redoCommand *> pure True

    -- Marks and metadata
    MarkLine range c -> do
      withLineAddress range False $ \addr -> markLine c addr
      pure True
    Filename _range path -> filenameCommand path *> pure True

    -- Session commands
    Help -> printLastError *> pure True
    HelpMode -> toggleHelpMode *> pure True
    TogglePrompt mPrompt -> do
      case mPrompt of
        Nothing -> modify (\s -> s { ledPromptActive = not (ledPromptActive s) })
        Just newPrompt -> modify (\s -> s { ledPrompt = newPrompt, ledPromptActive = True })
      pure True
    Comment -> pure True
    ImportDir -> importDirCommand *> pure True
    ImportMacro path -> do
      wasVisual <- gets ledVisualMode
      importMacroCommand path
      nowVisual <- gets ledVisualMode
      pure (wasVisual == nowVisual)

    -- Function commands
    FnList -> defineFunctionCommand "" [] Nothing *> pure True
    FnQuery name -> defineFunctionCommand name [] Nothing *> pure True
    FnDefine name params body -> defineFunctionCommand name params (Just body) *> pure True
    InvokeFunction name range args suf -> do
      fns <- gets ledDefinedFunctions
      case Map.lookup name fns of
        Nothing -> addressError "Invalid function" *> pure True
        Just (_, body) -> do
          let rangeText = formatLineRange (frLines range)
              allArgs = if T.null rangeText then args else rangeText : args
          wasVisual <- gets ledVisualMode
          _ <- invokeFunctionWithParams name body allArgs
          nowVisual <- gets ledVisualMode
          -- If visual mode changed during function execution, stop processing
          -- to let mainLoop handle the mode transition
          if wasVisual /= nowVisual
            then pure False
            else printSuffix suf *> pure True

formatLineRange :: LineRange -> Text
formatLineRange = \case
  LineDefault -> ""
  LinePrevious -> "%"
  LineSingle addr -> formatAddr addr
  LineFree from to -> formatAddr from <> "," <> formatAddr to
  LineBound from to -> formatAddr from <> ";" <> formatAddr to
  where
    formatAddr = \case
      Current -> "."
      LastLine -> "$"
      Number n -> show n
      Mark c -> "'" <> T.singleton c
      Next _ -> ""
      Prev _ -> ""
      AddrOffset base off -> formatAddr base <> (if off >= 0 then "+" else "") <> show off

withLineRange :: FullRange -> ((Int, Int) -> Led ()) -> Led ()
withLineRange range action = withLineRangeOr range getCurrentLineRange action

withLineRangeOr :: FullRange -> Led (Int, Int) -> ((Int, Int) -> Led ()) -> Led ()
withLineRangeOr (FullRange _ lineRange) defaultRange action = do
  cur <- getCurrentLine
  doc <- getDocument
  let total = LedDocument.lineCount doc
  marks <- getMarks
  let docLines = LedDocument.documentLines doc
  case lineRange of
    LineDefault -> defaultRange >>= action
    _ -> case resolveLineRange cur total marks docLines lineRange of
      Left err -> addressError err
      Right (s, e) -> action (s, e)

withLineAddress :: FullRange -> Bool -> (Int -> Led ()) -> Led ()
withLineAddress range allowZero action = withLineAddressOr range getCurrentLineDefault allowZero action

withLineAddressOr :: FullRange -> Led Int -> Bool -> (Int -> Led ()) -> Led ()
withLineAddressOr (FullRange _ lineRange) defaultAddr allowZero action = do
  cur <- getCurrentLine
  doc <- getDocument
  let total = LedDocument.lineCount doc
  marks <- getMarks
  let docLines = LedDocument.documentLines doc
  case lineRange of
    LineDefault -> defaultAddr >>= action
    LineSingle addr -> case resolveAddr cur total marks docLines addr of
      Left err -> addressError err
      Right n | not allowZero && n < 1 -> addressError "Invalid address"
              | otherwise -> action n
    _ -> addressError "Unexpected range"

getCurrentLineDefault :: Led Int
getCurrentLineDefault = getCurrentLine

getLineCountDefault :: Led Int
getLineCountDefault = LedDocument.lineCount <$> getDocument

getCurrentLineRange :: Led (Int, Int)
getCurrentLineRange = do
  n <- getCurrentLine
  pure (n, n)

getCurrentAndNextRange :: Led (Int, Int)
getCurrentAndNextRange = do
  n <- getCurrentLine
  doc <- getDocument
  pure (n, min (LedDocument.lineCount doc) (n + 1))

handleLineCommand :: LineRange -> Command -> Led Bool
handleLineCommand lineRange cmd =
  -- Special case: ShellCommand with a range acts as a filter
  case cmd of
    ShellCommand cmd' | lineRange /= LineDefault -> do
      withLineRangeOr (FullRange DocDefault lineRange) getCurrentLineRange $ \(s, e) ->
        filterLines s e cmd'
      pure True
    _ -> do
      -- Set the line range on the command and execute
      let cmdWithRange = case getCommandRange cmd of
            Just (FullRange docR _) -> setCommandRange (FullRange docR lineRange) cmd
            Nothing -> cmd
      executeCommand cmdWithRange

handleParamCommand :: LineRange -> Command -> Led Bool
handleParamCommand lineRange cmd = do
  paramStack <- gets ledParamStack
  case paramStack of
    [] -> addressError "No parameter document" *> pure True
    ((paramName, paramDs):restStack) -> case cmd of
      Filename _ Nothing -> outputLine (toString paramName) *> pure True
      Filename _ (Just _) -> paramReject
      _ -> do
        -- Transform CrossDocTarget DocParam to LocalTarget since we're already on the param doc
        let cmd' = localiseParamTargets cmd
        (result, newParamDs) <- withTempDocument paramName paramDs lineRange cmd'
        modify (\s -> s { ledParamStack = (paramName, newParamDs) : restStack })
        pure result
  where
    paramReject = addressError "Invalid command for parameter document" *> pure True
    -- Convert CrossDocTarget DocParam to LocalTarget (they refer to the same doc)
    localiseParamTargets = \case
      Transfer r (CrossDocTarget DocParam addr) s -> Transfer r (LocalTarget addr) s
      Move r (CrossDocTarget DocParam addr) s -> Move r (LocalTarget addr) s
      other -> other

handleDocListCommand :: LineRange -> Command -> Led Bool
handleDocListCommand lineRange cmd = do
  case rejectGlobalOnly cmd of
    Just msg -> addressError msg *> pure True
    Nothing -> case cmd of
      Filename _ (Just _) -> docListReject
      PrintLines _ NumberSuffix -> do
        dl <- gets ledDocumentList
        let origIdx = dlCurrentDoc dl
            total = documentCount dl
            dlState = dlDocListState dl
            doc = docDocument dlState
            cur = docCurrentLine dlState
            marks = docMarks dlState
            docLines = LedDocument.documentLines doc
            resolved = case lineRange of
              LineDefault -> Right (cur, cur)
              _ -> resolveLineRange cur total marks docLines lineRange
        case resolved of
          Left err -> addressError err *> pure True
          Right (s, e) -> do
            let lns = LedDocument.getLines s e doc
            forM_ (zip [s..e] lns) $ \(lineNum, filename) -> do
              let isCurrent = lineNum == origIdx
                  isChanged = case getDocStateAt lineNum dl of
                    Just ds -> docChangeFlag ds /= Unchanged
                    Nothing -> False
                  marker = if isCurrent then ">" else " "
                  changeMark = if isChanged then "*" else ""
              -- Use tab separator so gutter rendering puts number in gutter, filename in content
              outputLine . toString $ show lineNum <> "\t" <> marker <> filename <> changeMark
            -- NOTE: Do NOT update docCurrentLine here, as that would change the
            -- current document index. The doc list's current line is the current
            -- doc index, not a display cursor.
            pure True
      _ -> do
        dlState <- gets (dlDocListState . ledDocumentList)
        (result, newDlState) <- withTempDocument "documents" dlState lineRange cmd
        -- Humanise all filenames in the document list for nicer display
        humanisedDlState <- humaniseDocListState newDlState
        let newLines = LedDocument.documentLines (docDocument humanisedDlState)
        modify (\s -> s { ledDocumentList = (ledDocumentList s) { dlDocListState = humanisedDlState } })
        applyDocListDiff dlState newLines
        pure result
  where
    docListReject = addressError "Invalid command for document list" *> pure True

handleManageCommand :: LineRange -> Command -> Led Bool
handleManageCommand lineRange cmd = do
  case rejectGlobalOnly cmd of
    Just msg -> addressError msg *> pure True
    Nothing -> case cmd of
      Filename _ (Just _) -> manageReject
      _ -> do
        dlState <- gets (dlDocListState . ledDocumentList)
        (result, newDlState) <- withTempDocument "documents" dlState lineRange cmd
        -- Humanise all filenames in the document list for nicer display
        humanisedDlState <- humaniseDocListState newDlState
        let newLines = LedDocument.documentLines (docDocument humanisedDlState)
        modify (\s -> s { ledDocumentList = (ledDocumentList s) { dlDocListState = humanisedDlState } })
        applyManagedDiff dlState newLines
        pure result
  where
    manageReject = addressError "Invalid command for document list" *> pure True

-- | Humanise all filenames (lines) in a document list state
-- Skip empty lines to avoid canonicalizing "" to CWD
humaniseDocListState :: DocumentState -> Led DocumentState
humaniseDocListState ds = do
  let oldLines = LedDocument.documentLines (docDocument ds)
  newLines <- liftIO $ traverse humaniseIfNonEmpty oldLines
  let newDoc = LedDocument.fromLines newLines
  pure ds { docDocument = newDoc }
  where
    humaniseIfNonEmpty line
      | T.null line = pure line
      | otherwise   = toText <$> humanisePath (toString line)

withTempDocument :: Text -> DocumentState -> LineRange -> Command -> Led (Bool, DocumentState)
withTempDocument name docState lineRange cmd = do
  dl <- gets ledDocumentList
  let origIdx = dlCurrentDoc dl
      total = documentCount dl
      tempIdx = total + 1
      tempDl = setDlCurrentDoc tempIdx (insertDocAfter total name docState dl)
  modify (\s -> s { ledDocumentList = tempDl })
  result <- executePrefixedCmd lineRange cmd
  newDl <- gets ledDocumentList
  let newDocState = maybe docState id (getDocStateAt tempIdx newDl)
      cleanedDl = setDlCurrentDoc origIdx (deleteDocRange tempIdx tempIdx newDl)
  modify (\s -> s { ledDocumentList = cleanedDl })
  pure (result, newDocState)

executePrefixedCmd :: LineRange -> Command -> Led Bool
executePrefixedCmd _lineRange c = case c of
  Quit -> pure True
  QuitAlways -> pure True
  Edit _range path -> guardChanged >>= \ok ->
    if ok then resolveFilePath path >>= maybe (pure ()) editFile >> pure True
    else pure True
  EditAlways _range path -> resolveFilePath path >>= maybe (pure ()) editFileAlways >> pure True
  EditShell _range cmd' -> guardChanged >>= \ok ->
    if ok then editFromShell cmd' >> pure True else pure True
  EditShellAlways _range cmd' -> editFromShell cmd' *> pure True
  WriteQuit range path -> do
      doc <- getDocument
      withLineRangeOr range (pure (1, LedDocument.lineCount doc)) $ \(s, e) ->
          resolveFilePath path >>= maybe (pure ()) (\p -> writeFileRange p s e)
      pure True
  WriteQuitShell range cmd' -> do
      doc <- getDocument
      withLineRangeOr range (pure (1, LedDocument.lineCount doc)) $ \(s, e) ->
          writeToShell cmd' s e
      pure True
  Global range re cmdlist -> executeGlobal globalHandlers True (frLines range) re cmdlist *> pure True
  GlobalReverse range re cmdlist -> executeGlobal globalHandlers False (frLines range) re cmdlist *> pure True
  GlobalInteractive range re -> executeInteractiveGlobal globalHandlers True (frLines range) re *> pure True
  GlobalReverseInteractive range re -> executeInteractiveGlobal globalHandlers False (frLines range) re *> pure True
  _ -> executeCommand c *> pure True

handleCrossDocCommand :: DocRange -> LineRange -> Command -> Led Bool
handleCrossDocCommand docRange lineRange cmd = do
  dl <- gets ledDocumentList
  let origDoc = dlCurrentDoc dl
      totalDocs = documentCount dl
      docFilenames = LedDocument.documentLines (dlDocList dl)

  let dlMarks = docMarks (dlDocListState dl)
  case resolveDocRange origDoc totalDocs docFilenames dlMarks docRange of
    Left err -> addressError err *> pure True
    Right (docStart, docEnd) -> do
      let isMultiDoc = docStart /= docEnd
      results <- forM [docStart..docEnd] $ \docIdx -> do
        modify (\s -> s { ledDocumentList = setDlCurrentDoc docIdx (ledDocumentList s)
                       , ledMultiDocPrint = if isMultiDoc then Just docIdx else Nothing })
        executeCrossDocLineCommand origDoc lineRange cmd
      modify (\s -> s { ledDocumentList = setDlCurrentDoc origDoc (ledDocumentList s)
                     , ledMultiDocPrint = Nothing })
      pure (and results)

executeCrossDocLineCommand :: Int -> LineRange -> Command -> Led Bool
executeCrossDocLineCommand origDoc lineRange cmd = case cmd of
  Transfer range (LocalTarget addr) suf -> do
    withLineRangeOr range getCurrentLineRange $ \(s, e) ->
      executeCrossDocTransfer origDoc addr s e *> printSuffix suf
    pure True
  Move range (LocalTarget addr) suf -> do
    withLineRangeOr range getCurrentLineRange $ \(s, e) ->
      executeCrossDocMove origDoc addr s e *> printSuffix suf
    pure True
  Transfer range target suf -> do
    withLineRangeOr range getCurrentLineRange $ \(s, e) ->
      executeTransfer target s e *> printSuffix suf
    pure True
  Move range target suf -> do
    withLineRangeOr range getCurrentLineRange $ \(s, e) ->
      executeMove target s e *> printSuffix suf
    pure True
  _ -> handleLineCommand lineRange cmd

-- | Handle commands with &* prefix (modified documents only).
-- Executes the command on each document with unsaved changes.
handleModifiedCommand :: LineRange -> Command -> Led Bool
handleModifiedCommand lineRange cmd = do
  dl <- gets ledDocumentList
  let origDoc = dlCurrentDoc dl
      modifiedDocs = unsavedDocuments dl
  if null modifiedDocs
    then do
      addressError "No modified documents"
      pure True
    else do
      results <- forM modifiedDocs $ \docIdx -> do
        modify (\s -> s { ledDocumentList = setDlCurrentDoc docIdx (ledDocumentList s) })
        executeCrossDocLineCommand origDoc lineRange cmd
      modify (\s -> s { ledDocumentList = setDlCurrentDoc origDoc (ledDocumentList s) })
      pure (and results)

globalHandlers :: GlobalHandlers
globalHandlers = GlobalHandlers
  { ghExecuteCommand    = executeCommand
  , ghHandleDocList     = handleDocListCommand
  , ghHandleManage      = handleManageCommand
  , ghHandleModified    = handleModifiedCommand
  , ghHandleParam       = handleParamCommand
  , ghHandleCrossDoc    = handleCrossDocCommand
  , ghExpandExpressions = expandExpressions
  }

guardAllChanged :: Led Bool
guardAllChanged = do
  dl <- gets ledDocumentList
  let changed = changedDocuments dl
  let modifiedStates = mapMaybe (\idx -> (\ds -> (idx, ds)) <$> getDocStateAt idx dl) changed
  traverse_ (\(idx, ds) -> modify (\s -> s { ledDocumentList = setDocStateAt idx (ds { docChangeFlag = ChangedAndWarned }) (ledDocumentList s) })) modifiedStates
  case changed of
    [] -> pure True
    _ -> do
      setErrorText ("Warning: "
        <> pluraliseDocument changed
        <> " "
        <> (T.intercalate ", " $ map show changed)
        <> " modified")
      flagError
      outputLine "?"
      printHelpIfActive
      pure False
  where
    pluraliseDocument xs = if (length xs == 1) then "document" else "documents"

importMacroCommand :: FilePath -> Led ()
importMacroCommand path
    | "!" `isPrefixOf` path = importFromShell (drop 1 path)
    | otherwise = (liftIO . expandPath) path >>= importFromFile

importFromFile :: FilePath -> Led ()
importFromFile path = do
    absPath <- liftIO $ makeAbsolute path
    let scriptDir = takeDirectory absPath
    result <- liftIO $ readTextFile absPath
    case result of
      Left err -> addressError err
      Right content -> do
        let lns = T.lines content
            scriptName = takeFileName absPath
            byteCount = B.length (TE.encodeUtf8 content)
        printByteCount byteCount ReadFrom (toText path)
        savedQueue <- gets ledInputQueue
        wasVisual <- gets ledVisualMode
        modify (\s -> s { ledInputQueue = lns
                        , ledImportDirStack = scriptDir : ledImportDirStack s
                        , ledImportFileStack = scriptName : ledImportFileStack s
                        , ledUndoDepth = ledUndoDepth s + 1 })
        void processFunctionQueue
        nowVisual <- gets ledVisualMode
        modify (\s -> s { ledImportDirStack = drop 1 (ledImportDirStack s)
                        , ledImportFileStack = drop 1 (ledImportFileStack s)
                        -- Preserve queue if visual mode changed (remaining commands for new mode)
                        , ledInputQueue = if wasVisual == nowVisual then savedQueue else ledInputQueue s
                        , ledUndoDepth = ledUndoDepth s - 1 })

importFromShell :: String -> Led ()
importFromShell cmd = do
    (_, out, _) <- liftIO $ runShellCommand cmd ""
    let t = toText out
        lns = if null out then [] else T.lines t
        byteCount = B.length (TE.encodeUtf8 t)
    printByteCount byteCount ReadFrom ("!" <> toText cmd)
    savedQueue <- gets ledInputQueue
    wasVisual <- gets ledVisualMode
    modify (\s -> s { ledInputQueue = lns, ledUndoDepth = ledUndoDepth s + 1 })
    void processFunctionQueue
    nowVisual <- gets ledVisualMode
    -- Preserve queue if visual mode changed (remaining commands for new mode)
    modify (\s -> s { ledInputQueue = if wasVisual == nowVisual then savedQueue else ledInputQueue s
                    , ledUndoDepth = ledUndoDepth s - 1 })
    gets ledSilent >>= bool (outputStrLn' "!") (pure ())
