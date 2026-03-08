-- | Global commands (g, G, v, V) for led.
-- This module handles the execution of global and interactive global commands.
--
-- Optimizations:
--   * Commands are parsed once, not per-line
--   * Simple delete (g/re/d) uses batch deletion O(n) instead of O(m*n)
--   * Simple substitute uses single-pass processing
module LedGlobal
  ( -- * Handler record
    GlobalHandlers(..)
    -- * Global commands
  , executeGlobal
  , readGlobalCmdlist
  , executeOnMarkedLines
  , executeOnMarkedLinesWithExpansion
  , executeGlobalCommand
  , executeInteractiveGlobal
  , interactOnMarkedLines
  ) where

import Data.List (minimum)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import LedParse (Command(..), DocRange(..), FullRange(..), LineRange(..), ParseResult(..), Suffix(..), SubstFlags(..), parseCommand, getCommandRange)
import LedResolve (resolveLineRange)
import qualified LedDocument
import qualified LedRegularExpressions as RE
import LedState (addressError, curAndTotal, getDocument, getMarks, setChangeFlag, setCurrentLine, setDocument, splitGlobalCmds)
import LedEdit (substituteLine)
import LedSmartReplace (isSmartReplaceEligible, isAllLower)
import LedPrint (printRange, printSuffix)
import LedSession (adjustMarksDelete)
import LedInput (Led, getLineFromQueueOrInput)
import LedCore (LedState(..))
import LedNexus (BufferChangeFlag(..))

-- ---------------------------------------------------------------------------
-- Handler record
-- ---------------------------------------------------------------------------

-- | Record of handler functions passed from LedExec to break circular dependency.
-- These functions handle different command contexts and are defined in LedExec.
data GlobalHandlers = GlobalHandlers
  { ghExecuteCommand    :: Command -> Led Bool
  , ghHandleDocList     :: LineRange -> Command -> Led Bool
  , ghHandleManage      :: LineRange -> Command -> Led Bool
  , ghHandleModified    :: LineRange -> Command -> Led Bool
  , ghHandleParam       :: LineRange -> Command -> Led Bool
  , ghHandleCrossDoc    :: DocRange -> LineRange -> Command -> Led Bool
  , ghExpandExpressions :: Text -> Led Text  -- ^ Expand {expr} in command text
  }

-- ---------------------------------------------------------------------------
-- Global commands
-- ---------------------------------------------------------------------------

-- | Execute a batch global command (g or v).
executeGlobal :: GlobalHandlers -> Bool -> LineRange -> Text -> Text -> Led ()
executeGlobal handlers matchSense lineRange reText cmdlistFirstLine = do
    (cur, total) <- curAndTotal
    marks <- getMarks
    doc <- getDocument
    let resolved = case lineRange of
          LineDefault -> if total == 0 then Right (0 :: Int, 0 :: Int) else Right (1, total)
          _ -> resolveLineRange cur total marks (LedDocument.documentLines doc) lineRange
    case resolved of
      Left err -> addressError err
      Right (s, e) -> case RE.parseBRE reText of
        Left err -> addressError err
        Right bre -> do
          -- Set last RE so s// can reuse it
          modify (\st -> st { ledLastRE = Just bre })
          -- Read continuation lines for multi-line command list
          fullCmdlist <- readGlobalCmdlist cmdlistFirstLine
          let cmdText = if T.null fullCmdlist then "p" else fullCmdlist
          -- Smart search: case-insensitive when pattern is all lowercase
          let smartSearch = isAllLower reText
              matchFn = if smartSearch then RE.matchesBREInsensitive else RE.matchesBRE
          -- Mark matching/non-matching lines
          let lns = LedDocument.getLines s e doc
              marked = [ lineNum | (lineNum, line) <- zip [s..e] lns
                       , matchFn bre line == matchSense ]
          -- Try batch optimizations first
          if null marked
            then pure ()
            else do
              -- Check if commands contain expressions that need per-line expansion
              let cmds = splitGlobalCmds cmdText
                  hasExpressions = any ("{" `T.isInfixOf`) cmds
              if hasExpressions
                then do
                  -- Must re-expand expressions for each marked line
                  executeOnMarkedLinesWithExpansion handlers cmds marked
                else do
                  -- Parse commands once (optimization #1)
                  fnNames <- Set.fromList . Map.keys <$> gets ledDefinedFunctions
                  let parsedCmds = map (parseCommand fnNames) cmds
                  -- Check for batch-optimizable patterns
                  case detectBatchPattern parsedCmds of
                    Just (BatchDelete suffix) ->
                      executeBatchDelete bre s e marked suffix
                    Just (BatchSubstitute subRe subRepl flags suffix) ->
                      executeBatchSubstitute s e marked subRe subRepl flags suffix
                    Nothing ->
                      -- Fall back to per-line execution with pre-parsed commands
                      executeOnMarkedLinesParsed handlers parsedCmds marked

-- | Pattern detected for batch optimization
data BatchPattern
  = BatchDelete Suffix
  | BatchSubstitute Text Text SubstFlags Suffix

-- | Detect if the command list is a simple batch-optimizable pattern.
-- Currently detects:
--   * Single "d" command (with optional suffix) -> BatchDelete
--   * Single "s/re/repl/flags" command (with default range) -> BatchSubstitute
detectBatchPattern :: [ParseResult] -> Maybe BatchPattern
detectBatchPattern [Complete (Delete (FullRange DocDefault LineDefault) suf)] =
  Just (BatchDelete suf)
detectBatchPattern [Complete (Substitute (FullRange DocDefault LineDefault) re repl flags suf)] =
  Just (BatchSubstitute re repl flags suf)
detectBatchPattern _ = Nothing

-- | Execute batch delete: delete all marked lines in one pass.
-- O(n) instead of O(m*n) where m = number of matches.
executeBatchDelete :: RE.BRE -> Int -> Int -> [Int] -> Suffix -> Led ()
executeBatchDelete _bre _start _end marked suffix = do
  doc <- getDocument
  -- Group consecutive lines into ranges and delete from end to start
  let ranges = groupConsecutive (reverse (sort marked))
      doc' = foldl' (flip deleteRange) doc ranges
      newTotal = LedDocument.lineCount doc'
      -- New current line: first deleted line position, clamped to valid range
      firstDeleted = minimum marked
      newCur = if newTotal == 0 then 0
               else min firstDeleted newTotal
  setDocument doc'
  setCurrentLine newCur
  setChangeFlag Changed
  -- Adjust marks for all deletions
  forM_ (reverse (sort marked)) $ \lineNum ->
    adjustMarksDelete lineNum lineNum
  printSuffix suffix
  where
    -- Delete a range of lines [start, end]
    deleteRange :: (Int, Int) -> LedDocument.Document -> LedDocument.Document
    deleteRange (s, e) = LedDocument.replaceLines s (e - s + 1) []
    -- Group consecutive line numbers into (start, end) ranges
    -- Input must be sorted in descending order
    groupConsecutive :: [Int] -> [(Int, Int)]
    groupConsecutive [] = []
    groupConsecutive (x:xs) = go x x xs
      where
        go s e [] = [(s, e)]
        go s e (y:ys)
          | y == s - 1 = go y e ys  -- consecutive
          | otherwise  = (s, e) : go y y ys

-- | Execute batch substitute: substitute in marked lines only in one pass.
executeBatchSubstitute :: Int -> Int -> [Int] -> Text -> Text -> SubstFlags -> Suffix -> Led ()
executeBatchSubstitute start end marked subRe subRepl flags suffix = do
  -- Parse the substitute RE (may be empty to reuse last)
  lastRE <- gets ledLastRE
  lastRepl <- gets ledLastReplacement
  let mbre = if T.null subRe
             then maybe (Left "No previous regular expression") Right lastRE
             else RE.parseBRE subRe
      mrepl = if subRepl == "%"
              then maybe (Left "No previous substitution") Right lastRepl
              else Right subRepl
  case (mbre, mrepl) of
    (Left err, _) -> addressError err
    (_, Left err) -> addressError err
    (Right bre, Right actualRepl) -> do
      modify (\st -> st { ledLastRE = Just bre, ledLastReplacement = Just actualRepl })
      doc <- getDocument
      -- Determine if smart replace should be used
      let smartMode = isSmartReplaceEligible subRe actualRepl (sfInsensitive flags)
          markedSet = Set.fromList marked
      -- Get lines in range, apply substitution only to marked lines
      let lns = LedDocument.getLines start end doc
          (newLines, lastChanged, anyChanged) = mapLinesWithTracking start bre actualRepl flags smartMode markedSet lns
      if not anyChanged
        then addressError "No match"
        else do
          -- change start to end = replaceLines start (end - start + 1) newLines
          let doc' = LedDocument.replaceLines start (end - start + 1) newLines doc
          setDocument doc'
          when (lastChanged > 0) $ setCurrentLine lastChanged
          setChangeFlag Changed
          printSuffix suffix
  where
    -- Map over lines, applying substitution only to marked lines and tracking changes
    mapLinesWithTracking :: Int -> RE.BRE -> Text -> SubstFlags -> Bool -> Set.Set Int -> [Text] -> ([Text], Int, Bool)
    mapLinesWithTracking startLine bre repl flgs smartMode markedSet lns = go lns startLine [] 0 False
      where
        go [] _ acc lastCh changed = (reverse acc, lastCh, changed)
        go (l:ls) lineNum acc lastCh changed =
          if lineNum `Set.member` markedSet
          then let newLine = substituteLine bre repl (sfGlobal flgs) (sfCount flgs) smartMode l
                   didChange = newLine /= l
               in go ls (lineNum + 1) (newLine : acc) (if didChange then lineNum else lastCh) (changed || didChange)
          else go ls (lineNum + 1) (l : acc) lastCh changed

-- | Read continuation lines for a global command list.
-- If the first line ends with \, read more lines until one doesn't.
readGlobalCmdlist :: Text -> Led Text
readGlobalCmdlist firstLine = go firstLine
  where
    go acc
      | T.null acc = pure acc
      | T.last acc == '\\' = do
          let stripped = T.init acc
          mline <- getLineFromQueueOrInput
          case mline of
            Nothing   -> pure stripped
            Just line -> go (stripped <> "\n" <> toText line)
      | otherwise = pure acc

-- | Execute the command list on each marked line with pre-parsed commands.
-- Commands are already parsed (optimization #1).
executeOnMarkedLinesParsed :: GlobalHandlers -> [ParseResult] -> [Int] -> Led ()
executeOnMarkedLinesParsed _ _ [] = pure ()
executeOnMarkedLinesParsed handlers parsedCmds marks' = do
    forM_ marks' $ \markLine' -> do
      doc <- getDocument
      let total = LedDocument.lineCount doc
      when (markLine' >= 1 && markLine' <= total) $ do
        setCurrentLine markLine'
        forM_ parsedCmds $ \parsed -> case parsed of
          Failed err -> addressError err
          Incomplete -> addressError "Incomplete command"
          Complete cmd -> void $ executeGlobalCommand handlers cmd

-- | Execute the command list on each marked line, expanding expressions fresh each time.
-- Used when commands contain {expr} that should be re-evaluated for each line.
executeOnMarkedLinesWithExpansion :: GlobalHandlers -> [Text] -> [Int] -> Led ()
executeOnMarkedLinesWithExpansion _ _ [] = pure ()
executeOnMarkedLinesWithExpansion handlers cmds marks' = do
    forM_ marks' $ \markLine' -> do
      doc <- getDocument
      let total = LedDocument.lineCount doc
      when (markLine' >= 1 && markLine' <= total) $ do
        setCurrentLine markLine'
        -- Expand expressions and parse fresh for this line
        fnNames <- Set.fromList . Map.keys <$> gets ledDefinedFunctions
        forM_ cmds $ \cmdText -> do
          expanded <- ghExpandExpressions handlers cmdText
          case parseCommand fnNames expanded of
            Failed err -> addressError err
            Incomplete -> addressError "Incomplete command"
            Complete cmd -> void $ executeGlobalCommand handlers cmd

-- | Execute the command list on each marked line (legacy, parses each time).
-- Kept for compatibility but executeOnMarkedLinesParsed is preferred.
executeOnMarkedLines :: GlobalHandlers -> Text -> [Int] -> Led ()
executeOnMarkedLines _ _ [] = pure ()
executeOnMarkedLines handlers cmdText marks' = do
    let cmds = splitGlobalCmds cmdText
    fnNames <- Set.fromList . Map.keys <$> gets ledDefinedFunctions
    let parsedCmds = map (parseCommand fnNames) cmds
    executeOnMarkedLinesParsed handlers parsedCmds marks'

-- | Execute a command from within a global command.
-- Dispatches to the appropriate handler based on command context.
executeGlobalCommand :: GlobalHandlers -> Command -> Led Bool
executeGlobalCommand handlers cmd = case getCommandRange cmd of
  Nothing -> ghExecuteCommand handlers cmd
  Just (FullRange docRange lineRange) -> case docRange of
    DocDefault -> ghExecuteCommand handlers cmd
    DocAll -> ghHandleDocList handlers lineRange cmd
    DocManage -> ghHandleManage handlers lineRange cmd
    DocModified -> ghHandleModified handlers lineRange cmd
    DocParam -> ghHandleParam handlers lineRange cmd
    _ -> ghHandleCrossDoc handlers docRange lineRange cmd

-- | Execute an interactive global command (G or V).
executeInteractiveGlobal :: GlobalHandlers -> Bool -> LineRange -> Text -> Led ()
executeInteractiveGlobal handlers matchSense lineRange reText = do
    (cur, total) <- curAndTotal
    marks <- getMarks
    doc <- getDocument
    let resolved = case lineRange of
          LineDefault -> if total == 0 then Right (0 :: Int, 0 :: Int) else Right (1, total)
          _ -> resolveLineRange cur total marks (LedDocument.documentLines doc) lineRange
    case resolved of
      Left err -> addressError err
      Right (s, e) -> case RE.parseBRE reText of
        Left err -> addressError err
        Right bre -> do
          -- Set last RE so s// can reuse it
          modify (\st -> st { ledLastRE = Just bre })
          -- Smart search: case-insensitive when pattern is all lowercase
          let smartSearch = isAllLower reText
              matchFn = if smartSearch then RE.matchesBREInsensitive else RE.matchesBRE
          let lns = LedDocument.getLines s e doc
              marked = [ lineNum | (lineNum, line) <- zip [s..e] lns
                       , matchFn bre line == matchSense ]
          interactOnMarkedLines handlers marked Nothing

-- | Interactive loop for G/V: for each marked line, print it, read a command, execute.
interactOnMarkedLines :: GlobalHandlers -> [Int] -> Maybe Text -> Led ()
interactOnMarkedLines _ [] _ = pure ()
interactOnMarkedLines handlers (m:ms) lastCmd = do
    doc <- getDocument
    let total = LedDocument.lineCount doc
    if m < 1 || m > total
      then interactOnMarkedLines handlers ms lastCmd
      else do
        setCurrentLine m
        -- Print the current line
        printRange PrintSuffix m m
        -- Read one command
        mline <- getLineFromQueueOrInput
        case mline of
          Nothing -> pure ()  -- EOF, stop
          Just line
            | null line -> interactOnMarkedLines handlers ms lastCmd  -- null command
            | line == "&" -> case lastCmd of
                Nothing  -> interactOnMarkedLines handlers ms lastCmd  -- no previous command
                Just cmdTxt -> execAndContinue cmdTxt ms lastCmd
            | otherwise -> execAndContinue (toText line) ms (Just (toText line))
  where
    execAndContinue cmdTxt ms' prevCmd = do
      fnNames <- Set.fromList . Map.keys <$> gets ledDefinedFunctions
      case parseCommand fnNames cmdTxt of
        Failed err -> addressError err *> interactOnMarkedLines handlers ms' prevCmd
        Incomplete -> addressError "Incomplete command" *> interactOnMarkedLines handlers ms' prevCmd
        Complete cmd -> void (executeGlobalCommand handlers cmd) *> interactOnMarkedLines handlers ms' (Just cmdTxt)
