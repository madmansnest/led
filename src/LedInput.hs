-- | Input/Output abstractions for led.
-- This module provides the Led monad and basic input/output functions.
-- Uses haskeline with vi mode for interactive input, simple stdin for non-interactive.
module LedInput
  ( -- * Core types
    Led
  , LedEnv(..)
  , EditMode(..)
  , HupRef
    -- * Running Led
  , withLedEnv
  , runLed
    -- * Input functions
  , getLineFromQueueOrInput
  , getInputLineRaw
  , getInputLineLed
  , readInputText
    -- * Output functions
  , outputLine
  , outputStrLn'
    -- * Interrupt handling
  , withInterrupt
  , handleInterrupt
    -- * Settings
  , ledSettings
  , ledCompletion
    -- * Completion for visual mode
  , runCompletion
  , Completion(..)
  ) where

import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import System.IO (isEOF)
import System.Directory (listDirectory, doesFileExist, getPermissions, executable)
import System.FilePath ((</>), splitSearchPath)
import Control.Exception (try)

import qualified System.Console.Haskeline as H
import qualified System.Console.Haskeline.Completion as HC
import System.Console.Haskeline.Completion (Completion(..))

import LedCore (LedState(..))


-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

-- | Edit mode preference (read from ~/.haskeline).
data EditMode = EditVi | EditEmacs
  deriving stock (Eq, Show)

-- | Environment for Led operations.
data LedEnv = LedEnv
  { leInteractive :: !Bool
    -- ^ Whether we're in interactive mode
  , leHistoryFile :: !(Maybe FilePath)
    -- ^ Path to history file
  , leVisualInput :: !(Maybe (IO (Maybe String)))
    -- ^ Visual mode input hook: when set, use this instead of stdin
  , leHaskelineSettings :: !(Maybe (H.Settings IO))
    -- ^ Haskeline settings for interactive input (Nothing for non-interactive)
  , leEditMode :: !EditMode
    -- ^ Edit mode preference (Vi or Emacs) for visual mode keybindings
  }

-- | The Led monad: ReaderT over StateT over IO.
type Led = ReaderT LedEnv (StateT LedState IO)

-- | Reference to the SIGHUP handler action.
type HupRef = IORef (IO ())


-- ---------------------------------------------------------------------------
-- Running Led
-- ---------------------------------------------------------------------------

-- | Create a Led environment and run an action.
-- Uses haskeline with vi mode for interactive input, simple stdin for non-interactive.
withLedEnv :: Bool -> Maybe FilePath -> (LedEnv -> IO a) -> IO a
withLedEnv interactive histFile action = do
  -- Read edit mode preference from ~/.haskeline
  editMode <- readEditModePreference
  if interactive
    then do
      -- Interactive mode: create haskeline settings
      let settings = ledSettings histFile
      let env = LedEnv
            { leInteractive = True
            , leHistoryFile = histFile
            , leVisualInput = Nothing
            , leHaskelineSettings = Just settings
            , leEditMode = editMode
            }
      action env
    else do
      -- Non-interactive mode: use simple stdin
      let env = LedEnv
            { leInteractive = False
            , leHistoryFile = histFile
            , leVisualInput = Nothing
            , leHaskelineSettings = Nothing
            , leEditMode = editMode
            }
      action env


-- | Read edit mode preference from ~/.haskeline.
-- Returns EditVi if "editMode: Vi" is found, EditEmacs otherwise (default).
readEditModePreference :: IO EditMode
readEditModePreference = do
  home <- lookupEnv "HOME"
  case home of
    Nothing -> pure EditEmacs  -- Default to Emacs if HOME not set
    Just h -> do
      let haskelineFile = h </> ".haskeline"
      result <- try (readFileBS haskelineFile) :: IO (Either SomeException ByteString)
      case result of
        Left _ -> pure EditEmacs  -- Default if file doesn't exist or can't be read
        Right content -> pure $ parseEditMode (decodeUtf8 content)
  where
    parseEditMode :: Text -> EditMode
    parseEditMode content =
      let lns = T.lines content
          editLines = filter (T.isPrefixOf "editMode:") (map T.strip lns)
      in case editLines of
        (l:_) ->
          let value = T.strip $ T.drop 9 l  -- Drop "editMode:" prefix
          in if T.isPrefixOf "Vi" value || T.isPrefixOf "vi" value
               then EditVi
               else EditEmacs
        [] -> EditEmacs


-- | Run a Led computation.
runLed :: LedEnv -> LedState -> Led a -> IO (a, LedState)
runLed env st action = runStateT (runReaderT action env) st


-- ---------------------------------------------------------------------------
-- Haskeline helpers
-- ---------------------------------------------------------------------------

-- | Run a haskeline input action.
-- Vi mode can be configured via ~/.haskeline with "editMode: Vi"
runHaskelineInput :: H.Settings IO -> H.InputT IO a -> IO a
runHaskelineInput = H.runInputT


-- ---------------------------------------------------------------------------
-- Input functions
-- ---------------------------------------------------------------------------

-- | Get a line of input, checking the queue first.
-- Uses haskeline for interactive input, visual hook for visual mode, stdin for non-interactive.
getLineFromQueueOrInput :: Led (Maybe String)
getLineFromQueueOrInput = do
  queue <- gets ledInputQueue
  case queue of
    (x:xs) -> do
      modify (\s -> s { ledInputQueue = xs })
      pure (Just (toString x))
    [] -> do
      env <- ask
      case leVisualInput env of
        Just getLine' -> liftIO getLine'  -- Visual mode: use hook
        Nothing -> case leHaskelineSettings env of
          Just settings -> liftIO $ runHaskelineInput settings (H.getInputLine "")
          Nothing -> liftIO getLineStdin   -- Non-interactive mode: use stdin


-- | Read a line from the input queue if available, otherwise from haskeline.
-- Used by readInputText for multiline input modes.
getInputLineRaw :: Led (Maybe String)
getInputLineRaw = getLineFromQueueOrInput

-- | Get a line from stdin.
getLineStdin :: IO (Maybe String)
getLineStdin = do
  eof <- isEOF
  if eof
    then pure Nothing
    else Just . toString <$> getLine

-- | Read lines of input text until a line containing only "." is entered.
readInputText :: Led [Text]
readInputText = go []
  where
    go acc = getLineFromQueueOrInput >>= \case
      Nothing    -> pure (reverse acc)
      Just line
        | line == "." -> pure (reverse acc)
        | otherwise   -> go (toText line : acc)


-- | Get a line of input with a prompt.
-- Uses haskeline for interactive input, visual hook for visual mode, stdin for non-interactive.
getInputLineLed :: String -> Led (Maybe String)
getInputLineLed prompt = do
  queue <- gets ledInputQueue
  case queue of
    (x:xs) -> do
      modify (\s -> s { ledInputQueue = xs })
      pure (Just (toString x))
    [] -> do
      env <- ask
      case leVisualInput env of
        Just getLine' -> liftIO getLine'  -- Visual mode: use hook
        Nothing -> case leHaskelineSettings env of
          Just settings -> liftIO $ runHaskelineInput settings (H.getInputLine prompt)
          Nothing -> liftIO $ do          -- Non-interactive mode: prompt and read
            putStr prompt
            hFlush stdout
            getLineStdin


-- ---------------------------------------------------------------------------
-- Output functions
-- ---------------------------------------------------------------------------

-- | Output a line, either to stdout or to capture buffer.
outputLine :: String -> Led ()
outputLine s = gets ledCaptureOutput >>= \case
  Nothing -> liftIO $ putStrLn s
  Just acc ->
    modify (\st -> st { ledCaptureOutput = Just (toText s : acc) })


-- | Direct output (lifted).
outputStrLn' :: String -> Led ()
outputStrLn' = liftIO . putStrLn


-- ---------------------------------------------------------------------------
-- Interrupt handling
-- ---------------------------------------------------------------------------

-- | Handle interrupts during an action.
-- Haskeline handles Ctrl-C via key events, so this is mostly a no-op wrapper.
withInterrupt :: Led a -> Led a
withInterrupt = id


-- | Handle interrupt with recovery.
handleInterrupt :: Led a -> Led a -> Led a
handleInterrupt _handler action = action


-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | Haskeline settings for led.
ledSettings :: Maybe FilePath -> H.Settings IO
ledSettings histFile = H.Settings
  { H.complete       = ledCompletion
  , H.historyFile    = histFile
  , H.autoAddHistory = True
  }


-- | Custom completion function for led.
-- Supports:
-- 1. Filename completion for file commands (e, E, f, r, w, etc.)
-- 2. Shell command completion after !
-- 3. Instant {} expression substitution when tab pressed after }
-- 4. Regex/previous parameter substitution after /
ledCompletion :: HC.CompletionFunc IO
ledCompletion input@(left, _) =
  let line = reverse left
  in case detectCompletionType line of
       CompleteBraceExpr prefix expr ->
         -- Substitute {} expression (for now, just remove the braces as placeholder)
         -- Full evaluation would require LedState access
         pure (reverse prefix, [HC.Completion expr expr True])
       CompleteRegexPrev prefix prevPat ->
         -- Substitute previous regex pattern
         pure (reverse prefix, [HC.Completion prevPat prevPat True])
       CompleteShellCmd ->
         completeShellCommand input
       CompleteFilename ->
         HC.completeFilename input
       CompleteNone ->
         HC.noCompletion input

-- | Types of completion detected
data CompletionType'
  = CompleteBraceExpr String String  -- ^ Prefix before {, content inside braces
  | CompleteRegexPrev String String  -- ^ Prefix before last /, previous pattern to substitute
  | CompleteShellCmd                 -- ^ Complete shell command after !
  | CompleteFilename                 -- ^ Complete filename
  | CompleteNone                     -- ^ No completion

-- | Detect what type of completion is needed based on input.
detectCompletionType :: String -> CompletionType'
detectCompletionType line
  -- Check for {} expression: cursor immediately after }
  | Just (prefix, expr) <- extractBraceExpr line = CompleteBraceExpr prefix expr
  -- Check for regex substitution: after / with matching prefix
  | Just (prefix, prevPat) <- extractRegexPrev line = CompleteRegexPrev prefix prevPat
  -- Check for shell command after !
  | isShellCommand line = CompleteShellCmd
  -- Check for file commands
  | isFileCommand line = CompleteFilename
  | otherwise = CompleteNone

-- | Extract {} expression if cursor is immediately after }.
-- Returns (prefix before {, content inside braces) if found.
extractBraceExpr :: String -> Maybe (String, String)
extractBraceExpr line = case line of
  ('}':rest) -> findMatchingBrace rest 1 []
  _ -> Nothing
  where
    findMatchingBrace :: String -> Int -> String -> Maybe (String, String)
    findMatchingBrace [] _ _ = Nothing  -- Unmatched brace
    findMatchingBrace ('{':rest) 1 acc = Just (rest, acc)  -- Found matching {
    findMatchingBrace ('{':rest) n acc = findMatchingBrace rest (n-1) ('{':acc)
    findMatchingBrace ('}':rest) n acc = findMatchingBrace rest (n + 1) ('}':acc)
    findMatchingBrace (c:rest) n acc = findMatchingBrace rest n (c:acc)

-- | Extract previous regex pattern for substitution.
-- When after /, if text after last / starts with (or is empty and matches)
-- text after second-last /, substitute the second-last pattern.
extractRegexPrev :: String -> Maybe (String, String)
extractRegexPrev line = do
  -- Find positions of all unescaped /
  let slashPositions = findUnescapedSlashes line
  -- Need at least 2 slashes for substitution
  case slashPositions of
    (lastSlashPos : secondLastPos : _) -> do
      -- Text after last / (reversed, so take from start)
      let afterLast = take lastSlashPos line
          -- Text between second-last and last /
          betweenSlashes = take (secondLastPos - lastSlashPos - 1) (drop (lastSlashPos + 1) line)
      -- Check if afterLast is empty or is a prefix of betweenSlashes
      guard (null afterLast || afterLast `isPrefixOf` betweenSlashes)
      -- Return the prefix (everything after the last /) and the pattern to substitute
      let prefix = drop lastSlashPos line  -- includes the /
      Just (prefix, betweenSlashes)
    _ -> Nothing

-- | Find positions of unescaped / characters (0-indexed from start of reversed string).
findUnescapedSlashes :: String -> [Int]
findUnescapedSlashes = go 0 False
  where
    go _ _ [] = []
    go pos True (c:cs)   -- Previous char was backslash, skip this
      | c == '/' = go (pos + 1) False cs
      | otherwise = go (pos + 1) (c == '\\') cs
    go pos False (c:cs)
      | c == '/' = pos : go (pos + 1) False cs
      | c == '\\' = go (pos + 1) True cs
      | otherwise = go (pos + 1) False cs

-- | Check if we're completing a shell command (after !).
isShellCommand :: String -> Bool
isShellCommand line =
  let stripped = dropWhile isSpace line
      -- Skip range prefix
      afterRange = dropWhile (\c -> isDigit c || c `elem` ['.', '$', ',', ';', '+', '-', '/', '?', '\'']) stripped
      afterRangeStripped = dropWhile isSpace afterRange
  in case afterRangeStripped of
       ('!':_) -> True
       ('r':'!':_) -> True  -- read from command
       ('w':'!':_) -> True  -- write to command
       _ -> False

-- | Check if we need filename completion.
isFileCommand :: String -> Bool
isFileCommand line =
  let stripped = dropWhile isSpace line
      -- Skip range prefix
      afterRange = dropWhile (\c -> isDigit c || c `elem` ['.', '$', ',', ';', '+', '-', '/', '?', '\'']) stripped
  in needsFileCompletion afterRange
  where
    needsFileCompletion s = case s of
      ('e':rest)     -> startsWithSpace rest
      ('E':rest)     -> startsWithSpace rest
      ('f':rest)     -> not ("n" `isPrefixOf` rest) && startsWithSpace rest
      ('r':rest)     -> not ("!" `isPrefixOf` dropWhile isSpace rest) && startsWithSpace rest
      ('w':'q':rest) -> startsWithSpace rest
      ('w':rest)     -> not ("!" `isPrefixOf` dropWhile isSpace rest) && startsWithSpace rest
      ('i':'m':rest) -> startsWithSpace rest
      ('&':'a':rest) -> startsWithSpace rest
      ('&':'i':rest) -> startsWithSpace rest
      ('&':'c':rest) -> startsWithSpace rest
      _ -> False
    startsWithSpace [] = False
    startsWithSpace (c:_) = isSpace c

-- | Complete shell commands by looking up executables in PATH.
completeShellCommand :: HC.CompletionFunc IO
completeShellCommand input@(left, _) = do
  let line = reverse left
      -- Find the word being completed (after ! and any preceding words)
      (prefix, wordToComplete) = extractShellWord line
  if null wordToComplete && '!' `notElem` line
    then HC.noCompletion input
    else do
      -- Get completions
      completions <- getShellCompletions wordToComplete
      if null completions
        then HC.noCompletion input
        else do
          -- Return completions with the prefix consumed
          let hcCompletions = map (\c -> HC.Completion c c False) completions
          pure (reverse prefix, hcCompletions)

-- | Extract the word being completed from a shell command line.
-- Returns (prefix to keep, word to complete).
extractShellWord :: String -> (String, String)
extractShellWord line =
  let -- Find position after !
      afterBang = case break (== '!') line of
        (_, '!':bangRest) -> bangRest
        _ -> line
      -- Split into words
      (spaces, afterSpaces) = span isSpace afterBang
      (word, remaining) = break isSpace afterSpaces
  in if null remaining
       then (drop (length word) line, word)  -- Completing first/only word after !
       else -- Completing subsequent word - use filename completion would be better
            -- For now, still complete as command
            let (_, rest2) = span isSpace remaining
                (word2, _) = break isSpace rest2
            in if null rest2
                 then (drop (length spaces + length word) line, "")
                 else (drop (length word2) line, word2)

-- | Get shell command completions from PATH.
getShellCompletions :: String -> IO [String]
getShellCompletions prefix = do
  pathEnv <- lookupEnv "PATH"
  case pathEnv of
    Nothing -> pure []
    Just path -> do
      let dirs = splitSearchPath path
      executables <- concat <$> mapM (getExecutablesInDir prefix) dirs
      pure $ take 50 executables  -- Limit results

-- | Get executables in a directory that match a prefix.
getExecutablesInDir :: String -> FilePath -> IO [String]
getExecutablesInDir prefix dir = do
  result <- try $ listDirectory dir :: IO (Either SomeException [FilePath])
  case result of
    Left _ -> pure []
    Right entries -> do
      let matching = filter (prefix `isPrefixOf`) entries
      filterM (isExecutableFile dir) matching

-- | Check if a file is executable.
isExecutableFile :: FilePath -> FilePath -> IO Bool
isExecutableFile dir name = do
  let fullPath = dir </> name
  exists <- doesFileExist fullPath
  if exists
    then do
      perms <- getPermissions fullPath
      pure $ executable perms
    else pure False


-- ---------------------------------------------------------------------------
-- Completion for visual mode
-- ---------------------------------------------------------------------------

-- | Run completion for visual mode.
-- Takes (text before cursor, text after cursor) and returns completions.
-- Note: text before cursor is in NORMAL order (not reversed like haskeline).
runCompletion :: (Text, Text) -> IO (Text, [Completion])
runCompletion (beforeCursor, afterCursor) = do
  -- Haskeline uses reversed left, normal right
  let haskelineInput = (toString (T.reverse beforeCursor), toString afterCursor)
  (remainingReversed, completions) <- ledCompletion haskelineInput
  -- Convert back: remaining is reversed left, so reverse it
  return (T.reverse (toText remainingReversed), completions)
