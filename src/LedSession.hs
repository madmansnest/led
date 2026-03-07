module LedSession
  ( undoCommand, redoCommand
  , defineFunctionCommand, warnSuffixShadowing
  , importDirCommand
  , markLine, adjustMarksInsert, adjustMarksDelete, clearMarks
  , printLastError, printHelpIfActive, toggleHelpMode
  , countUnmatchedBraces, findMatchingBrace
  , isFnDefinition, needsSubContinuation
  , rejectGlobalOnly
  ) where

import Data.Char (isAlpha, isDigit, isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)

import LedParse (Command(..), isValidFunctionName, systemCommandNames)
import LedCore (LedState(..))
import LedNexus (BufferChangeFlag(..), adjustMarksForInsert, adjustMarksForDelete)
import qualified LedUndo
import LedInput (Led, outputLine)
import LedState

undoCommand :: Led ()
undoCommand = do
    mgr <- gets ledUndoManager
    dl <- gets ledDocumentList
    result <- liftIO $ LedUndo.performUndo mgr dl
    case result of
      Left err -> addressError err
      Right (_step, dl') -> do
        modify (\s -> s { ledDocumentList = dl' })
        setChangeFlag Changed

redoCommand :: Led ()
redoCommand = do
    mgr <- gets ledUndoManager
    dl <- gets ledDocumentList
    result <- liftIO $ LedUndo.performRedo mgr dl
    case result of
      Left err -> addressError err
      Right (_step, dl') -> do
        modify (\s -> s { ledDocumentList = dl' })
        setChangeFlag Changed

defineFunctionCommand :: Text -> [Text] -> Maybe Text -> Led ()
defineFunctionCommand name params mBody
  | T.null name = do
      fns <- gets ledDefinedFunctions
      unless (Map.null fns) $
        outputLine (toString (T.intercalate " " (sort (Map.keys fns))))
  | not (isValidFunctionName name) = invalidFunction
  | isBuiltinCommand name = invalidCommand
  | Nothing <- mBody = do
      fns <- gets ledDefinedFunctions
      case Map.lookup name fns of
        Just (_, existing) -> do
          let lns = [T.pack ("fn " ++ T.unpack name ++ " {")] ++ T.splitOn "\n" existing ++ ["}"]
          forM_ lns $ \l -> outputLine (toString l)
        Nothing -> invalidFunction
  | Just body <- mBody = do
      warnSuffixShadowing name
      modify (\s -> s { ledDefinedFunctions = Map.insert name (params, body) (ledDefinedFunctions s) })
  where
    invalidFunction = addressError "Invalid function"
    isBuiltinCommand n = n `Set.member` systemCommandNames && T.length n >= 2
    invalidCommand = addressError "Invalid command"

warnSuffixShadowing :: Text -> Led ()
warnSuffixShadowing name = do
  let lastChar = T.last name
  when (lastChar == 'p' || lastChar == 'n' || lastChar == 'l') $ do
    let shorter = T.init name
    fns <- gets ledDefinedFunctions
    let shadowsSysCmd = shorter `Set.member` systemCommandNames
        shadowsUserFn = Map.member shorter fns && isValidFunctionName shorter
    when (shadowsSysCmd || shadowsUserFn) $ do
      let target = if shadowsSysCmd
                   then "command '" <> shorter <> "'"
                   else "function '" <> shorter <> "'"
          sufName = case lastChar of
            'p' -> "print"; 'n' -> "number"; _ -> "list"
      outputLine ("Warning: function '" <> toString name <> "' shadows " <> toString target <> " with " <> sufName <> " suffix")

importDirCommand :: Led ()
importDirCommand = do
    stack <- gets ledImportDirStack
    case stack of
      (d:_) -> outputLine d
      []    -> liftIO getCurrentDirectory >>= outputLine

markLine :: Char -> Int -> Led ()
markLine c addr = modifyMarks (Map.insert c addr)

adjustMarksInsert :: Int -> Int -> Led ()
adjustMarksInsert pos count = modifyMarks (adjustMarksForInsert pos count)

adjustMarksDelete :: Int -> Int -> Led ()
adjustMarksDelete start end = modifyMarks (adjustMarksForDelete start end)

clearMarks :: Led ()
clearMarks = modifyMarks (const Map.empty)

printLastError :: Led ()
printLastError = gets ledLastError >>= traverse_ (outputLine . toString)

toggleHelpMode :: Led ()
toggleHelpMode =
  modify (\s -> s { ledHelpMode = not (ledHelpMode s) }) *> gets ledHelpMode >>= bool (pure ()) printLastError

countUnmatchedBraces :: Text -> Int
countUnmatchedBraces = go 0
  where
    go n t = case T.uncons t of
      Nothing -> n
      Just ('\\', r) -> case T.uncons r of
        Just (_, r') -> go n r'
        Nothing -> n
      Just ('{', r) -> go (n + 1) r
      Just ('}', r) -> go (n - 1) r
      Just (_, r) -> go n r

findMatchingBrace :: Text -> Int -> Maybe (Text, Text)
findMatchingBrace t depth = go "" t depth
  where
    go acc rest d = case T.uncons rest of
      Nothing -> Nothing
      Just ('\\', r) -> case T.uncons r of
        Just (c, r') -> go (acc <> "\\" <> T.singleton c) r' d
        Nothing      -> Nothing
      Just ('{', r) -> go (acc <> "{") r (d + 1)
      Just ('}', r)
        | d == 0    -> Just (acc, r)
        | otherwise -> go (acc <> "}") r (d - 1)
      Just (c, r) -> go (acc <> T.singleton c) r d

isFnDefinition :: Text -> Bool
isFnDefinition t =
  let s = T.stripStart t
  in case T.stripPrefix "fn" s of
    Nothing -> False
    Just rest -> T.null rest || maybe False (isSpace . fst) (T.uncons rest)

needsSubContinuation :: Text -> Bool
needsSubContinuation input = checkAfterPrefix (T.stripStart input)
  where
    checkAfterPrefix t = case T.uncons t of
      Just ('@', rest) -> checkAfterRange (T.stripStart rest)
      _ -> checkForCrossDoc t

    checkForCrossDoc t =
      let (_, rest) = skipRange t
          stripped = T.stripStart rest
      in case T.uncons stripped of
        Just (':', afterColon) ->
          checkAfterRange (T.stripStart afterColon)
        _ -> checkAfterRange t

    checkAfterRange t =
      let (_, rest) = skipRange t
          stripped = T.stripStart rest
      in case T.uncons stripped of
        Just ('s', r) -> checkSubDelimited r
        _ -> False

    checkSubDelimited t = case T.uncons t of
      Just (delim, r)
        | delim /= '\\' && delim /= ' ' && delim /= '\n' ->
          let (_, afterRE, reFound) = parseDelimited delim r
          in reFound &&
             let (replText, _, replFound) = parseDelimited delim afterRE
             in not replFound && hasOddTrailingBackslashes replText
      _ -> False

    hasOddTrailingBackslashes t =
      not (T.null t) && odd (T.length (T.takeWhileEnd (== '\\') t))

skipRange :: Text -> (Bool, Text)
skipRange = go False . T.stripStart
  where
    go found t = case T.uncons t of
      Nothing -> (found, t)
      Just (c, r)
        | c == '.' || c == '$' -> go True (T.stripStart r)
        | c == '\'' -> case T.uncons r of
            Just (m, r') | isAlpha m -> go True (T.stripStart r')
            _ -> (found, t)
        | c == '/' -> let (_, r', _) = parseDelimited '/' r in go True (T.stripStart r')
        | c == '?' -> let (_, r', _) = parseDelimited '?' r in go True (T.stripStart r')
        | c == '+' || c == '-' -> maybe (found, t) (go True . T.stripStart) (skipNumber r)
        | isDigit c -> maybe (found, t) (go True . T.stripStart) (skipNumber t)
        | c == ',' || c == ';' -> go True (T.stripStart r)
        | otherwise -> (found, t)

    skipNumber t = case T.uncons t of
      Just (c, r) | c == '+' || c == '-' -> skipDigits r
      _ -> skipDigits t

    skipDigits t = case T.uncons t of
      Just (c, r) | isDigit c -> skipDigits r
      _ -> Just t

parseDelimited :: Char -> Text -> (Text, Text, Bool)
parseDelimited delim = go mempty
  where
    go acc t = case T.uncons t of
      Nothing -> (acc, T.empty, False)
      Just ('\\', r) -> case T.uncons r of
        Just (c, r') -> go (acc <> T.singleton '\\' <> T.singleton c) r'
        Nothing -> (acc <> "\\", T.empty, False)
      Just (c, r)
        | c == delim -> (acc, r, True)
        | otherwise -> go (acc <> T.singleton c) r

rejectGlobalOnly :: Command -> Maybe Text
rejectGlobalOnly = \case
  Help            -> Just "Unexpected address"
  HelpMode        -> Just "Unexpected address"
  TogglePrompt _  -> Just "Unexpected address"
  Quit            -> Just "Unexpected address"
  QuitAlways      -> Just "Unexpected address"
  Undo _          -> Just "Unexpected address"
  Redo _          -> Just "Unexpected address"
  _               -> Nothing
