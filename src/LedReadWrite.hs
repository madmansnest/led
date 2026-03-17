module LedReadWrite
  ( shellCommand
  , runShellAndPrint
  , filterLines
  , readFileCommand
  , readShellCommand
  , writeFileRange
  , writeToShell
  , loadFile
  , editFile
  , editFileAlways
  , editFromShell
  , filenameCommand
  , substitutePercent
  , rememberFilePath
  , resolveFilePath
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import LedCore (LedState(..))
import LedNexus (BufferChangeFlag(..))
import qualified LedDocument
import LedIO (readDocument, writeBytes, runShellCommand, expandPath, humanisePath)
import LedInput (Led, outputLine, outputStrLn')
import LedState
import LedPrint (printByteCount, IODirection(..))

resolveShellExpansion :: Text -> Led (Maybe Text)
resolveShellExpansion rawCmd = do
    lastShell <- gets ledLastShellCommand
    fp <- getFilePath
    (cmd1, expanded1) <- case T.uncons rawCmd of
      Just ('!', rest) -> case lastShell of
        Just prev -> pure (prev <> rest, True)
        Nothing   -> addressError "No previous command" *> pure ("", False)
      _ -> pure (rawCmd, False)
    if T.null cmd1
      then do
        unless expanded1 $ addressError "No command given"
        pure Nothing
      else do
        let (cmd2, expanded2) = substitutePercent cmd1 fp
            anyExpanded = expanded1 || expanded2
        when anyExpanded $ outputStrLn' (toString cmd2)
        modify (\s -> s { ledLastShellCommand = Just cmd2 })
        pure (Just cmd2)

shellCommand :: Text -> Led ()
shellCommand rawCmd = resolveShellExpansion rawCmd >>= \case
    Nothing -> pure ()
    Just cmd -> do
      runShellAndPrint cmd
      gets ledSilent >>= bool (outputStrLn' "!") (pure ())

runShellAndPrint :: Text -> Led ()
runShellAndPrint cmd = do
    (_, out, err) <- liftIO $ runShellCommand (toString cmd) ""
    unless (null err) $ outputLine err
    unless (null out) $ outputLine (stripTrailingNewline out)
  where
    stripTrailingNewline s = case viaNonEmpty last s of
      Just '\n' -> maybe "" toList (viaNonEmpty init s)
      _         -> s

filterLines :: Int -> Int -> Text -> Led ()
filterLines start end rawCmd = resolveShellExpansion rawCmd >>= \case
    Nothing -> pure ()
    Just cmd2 -> do
      doc <- getDocument
      let lns = LedDocument.getLines start end doc
          input = T.unlines lns
      (_, out, err) <- liftIO $ runShellCommand (toString cmd2) (toString input)
      unless (null err) $ outputStrLn' err
      let outLines = if null out then [] else T.lines (toText out)
          -- change s to e = replaceLines s (e - s + 1) newLines
          doc' = LedDocument.replaceLines start (end - start + 1) outLines doc
          newCur = if null outLines then (if start > 1 then start - 1 else 0)
                   else start + length outLines - 1
      setDocument doc'
      setCurrentLine newCur
      setChangeFlag Changed
      adjustMarksDelete start end
      unless (null outLines) $ adjustMarksInsert (start - 1) (length outLines)

readFileCommand :: Maybe FilePath -> Int -> Led ()
readFileCommand path addr = do
    mfp <- case path of
      Just p  -> Just <$> (liftIO . expandPath) p
      Nothing -> getFilePath >>= maybe noCurrentFile (pure . Just)
    case mfp of
      Nothing -> pure ()
      Just fp -> do
        result <- liftIO (readDocument fp)
        case result of
          Left err -> addressError err
          Right (newDoc, n) -> do
            let lns = LedDocument.documentLines newDoc
            unless (null lns) $ do
              doc <- getDocument
              -- append after addr = replaceLines (addr + 1) 0 lines
              let doc' = LedDocument.replaceLines (addr + 1) 0 lns doc
                  newLine = addr + length lns
              setDocument doc'
              setCurrentLine newLine
              setChangeFlag Changed
              adjustMarksInsert addr (length lns)
            humanFp <- liftIO (humanisePath fp)
            rememberFilePath humanFp
            printByteCount n ReadFrom (toText humanFp)
  where
    noCurrentFile = addressError "No current filename" *> pure Nothing

readShellCommand :: Text -> Int -> Led ()
readShellCommand cmd addr = do
    (_, out, _) <- liftIO $ runShellCommand (toString cmd) ""
    let t = toText out
        bs = B.length (TE.encodeUtf8 t)
        lns = if T.null t then [] else T.lines t
    unless (null lns) $ do
      doc <- getDocument
      -- append after addr = replaceLines (addr + 1) 0 lines
      let doc' = LedDocument.replaceLines (addr + 1) 0 lns doc
          newLine = addr + length lns
      setDocument doc'
      setCurrentLine newLine
      setChangeFlag Changed
      adjustMarksInsert addr (length lns)
    printByteCount bs ReadFrom ("!" <> cmd)
    gets ledSilent >>= bool (outputStrLn' "!") (pure ())

writeFileRange :: FilePath -> Int -> Int -> Led ()
writeFileRange path start end = do
  doc <- getDocument
  let total = LedDocument.lineCount doc
      lns = LedDocument.getLines start end doc
      content = T.unlines lns
      bs = TE.encodeUtf8 content
  result <- liftIO $ writeBytes path bs
  case result of
    Left err -> addressError err
    Right n  -> do
      humanPath <- liftIO (humanisePath path)
      rememberFilePath humanPath
      when (start == 1 && end == total) $ setChangeFlag Unchanged
      printByteCount n WriteTo (toText humanPath)

writeToShell :: Text -> Int -> Int -> Led ()
writeToShell cmd start end = do
    doc <- getDocument
    let lns = LedDocument.getLines start end doc
        input = T.unlines lns
        bs = B.length (TE.encodeUtf8 input)
    (_, out, err) <- liftIO $ runShellCommand (toString cmd) (toString input)
    unless (null err) $ outputLine err
    unless (null out) $ outputLine (stripTrailingNewline out)
    printByteCount bs WriteTo ("!" <> cmd)
  where
    stripTrailingNewline s = case viaNonEmpty last s of
      Just '\n' -> maybe "" toList (viaNonEmpty init s)
      _         -> s

loadFile :: FilePath -> Led ()
loadFile path =
  liftIO (readDocument path) >>= either
    (\err -> do
      setErrorText err
      gets ledSilent >>= bool (outputStrLn' (toString err)) (pure ()))
    (\(doc, n) -> do
      setDocument doc
      humanPath <- liftIO (humanisePath path)
      setFilePath humanPath
      setChangeFlag Unchanged
      setCurrentLine (LedDocument.lineCount doc)
      printByteCount n ReadFrom (toText humanPath))

editFile :: FilePath -> Led ()
editFile path = do
  clearMarks
  setDocument LedDocument.emptyDocument
  setCurrentLine 0
  humanPath <- liftIO (humanisePath path)
  setFilePath humanPath
  loadFile path

editFileAlways :: FilePath -> Led ()
editFileAlways = editFile

editFromShell :: Text -> Led ()
editFromShell cmd = do
    clearMarks
    (_, out, _) <- liftIO $ runShellCommand (toString cmd) ""
    let t = toText out
        bs = B.length (TE.encodeUtf8 t)
        doc = if T.null t then LedDocument.emptyDocument else LedDocument.fromText t
        total = LedDocument.lineCount doc
    setDocument doc
    setCurrentLine total
    setChangeFlag Unchanged
    printByteCount bs ReadFrom ("!" <> cmd)
    gets ledSilent >>= bool (outputStrLn' "!") (pure ())

filenameCommand :: Maybe FilePath -> Led ()
filenameCommand path = do
  case path of
    Just p -> (liftIO . humanisePath) p >>= setFilePath
    Nothing -> pure ()
  getFilePath >>= traverse_ (outputLine . toList)

substitutePercent :: Text -> Maybe FilePath -> (Text, Bool)
substitutePercent cmd mfp = go cmd "" False
  where
    go t acc expanded = case T.uncons t of
      Nothing -> (acc, expanded)
      Just ('\\', r) -> case T.uncons r of
        Just ('%', r') -> go r' (acc <> "%") expanded
        Just (c, r')   -> go r' (acc <> "\\" <> T.singleton c) expanded
        Nothing        -> (acc <> "\\", expanded)
      Just ('%', r) -> case mfp of
        Just fp -> go r (acc <> toText fp) True
        Nothing -> go r (acc <> "%") expanded
      Just (c, r) -> go r (acc <> T.singleton c) expanded

rememberFilePath :: FilePath -> Led ()
rememberFilePath path = do
  fp <- getFilePath
  when (isNothing fp) $ do
    humanPath <- liftIO (humanisePath path)
    setFilePath humanPath

resolveFilePath :: Maybe FilePath -> Led (Maybe FilePath)
resolveFilePath (Just p) = Just <$> (liftIO . expandPath) p
resolveFilePath Nothing  = getFilePath >>= maybe noFile (fmap Just . liftIO . expandPath)
  where
    noFile = addressError "No current filename" *> pure Nothing