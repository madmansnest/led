module LedState
  ( getDocument, getCurrentLine, getMarks, getChangeFlag, setChangeFlag
  , getFilePath, setFilePath
  , modifyDocState, setDocument, setCurrentLine, modifyMarks
  , pushParamDoc, popParamDoc
  , addressError, setErrorText, augmentErrorWithContext, flagError
  , printHelpIfActive
  , guardChanged
  , ensureNonEmptyDocList
  , resolveTargetAddress
  , removeDocStates, insertDocState, openDocState, syncDlCurrentDoc
  , applyDocListDiff, applyManagedDiff
  , curAndTotal
  , splitGlobalCmds
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import LedParse (Addr)
import LedResolve (resolveAddr)
import LedCore (LedState(..))
import LedNexus (BufferChangeFlag(..), DocumentState(..), DocumentList(..), getCurrentDocState, modifyCurrentDocState, setCurrentFilename, getCurrentFilename, documentCount, singletonDocumentList, emptyDocumentState, getDocStateAt, setDocStateAt, setDlCurrentDoc, dlDocListState)
import qualified LedDocument
import LedManageFiles (computeManageDiff, checkManageWarnings, hasWarnings, formatManageWarnings, executeManageActions, ManageDiff(..))
import LedIO (readDocument, expandPath, humanisePath)
import LedInput (Led, outputLine, outputStrLn')

getDocument :: Led LedDocument.Document
getDocument = do
  dl <- gets ledDocumentList
  pure $ maybe LedDocument.emptyDocument docDocument (getCurrentDocState dl)

getCurrentLine :: Led Int
getCurrentLine = do
  dl <- gets ledDocumentList
  pure $ maybe 0 docCurrentLine (getCurrentDocState dl)

getMarks :: Led (Map.Map Char Int)
getMarks = do
  dl <- gets ledDocumentList
  pure $ maybe Map.empty docMarks (getCurrentDocState dl)

getChangeFlag :: Led BufferChangeFlag
getChangeFlag = do
  dl <- gets ledDocumentList
  pure $ maybe Unchanged docChangeFlag (getCurrentDocState dl)

setChangeFlag :: BufferChangeFlag -> Led ()
setChangeFlag flag = modifyDocState (\ds -> ds { docChangeFlag = flag })

getFilePath :: Led (Maybe FilePath)
getFilePath = do
  dl <- gets ledDocumentList
  pure $ fmap toString (getCurrentFilename dl)

setFilePath :: FilePath -> Led ()
setFilePath path = modify (\s -> s { ledDocumentList = setCurrentFilename (toText path) (ledDocumentList s) })

modifyDocState :: (DocumentState -> DocumentState) -> Led ()
modifyDocState f = modify (\s -> s { ledDocumentList = modifyCurrentDocState f (ledDocumentList s) })

setDocument :: LedDocument.Document -> Led ()
setDocument doc = modifyDocState (\ds -> ds { docDocument = doc })

setCurrentLine :: Int -> Led ()
setCurrentLine n = modifyDocState (\ds -> ds { docCurrentLine = n })

modifyMarks :: (Map.Map Char Int -> Map.Map Char Int) -> Led ()
modifyMarks f = modifyDocState (\ds -> ds { docMarks = f (docMarks ds) })

pushParamDoc :: Text -> DocumentState -> Led ()
pushParamDoc name ds = modify (\s -> s { ledParamStack = (name, ds) : ledParamStack s })

popParamDoc :: Led ()
popParamDoc = modify (\s -> s { ledParamStack = drop 1 (ledParamStack s) })

addressError :: Text -> Led ()
addressError msg = do
  setErrorText msg
  flagError
  outputLine "?"
  printHelpIfActive

setErrorText :: Text -> Led ()
setErrorText msg = do
  augmented <- augmentErrorWithContext msg
  modify (\s -> s { ledLastError = Just augmented })

augmentErrorWithContext :: Text -> Led Text
augmentErrorWithContext msg = do
  paramStack <- gets ledParamStack
  mLine <- gets ledCurrentInputLine
  scriptStack <- gets ledImportFileStack
  let fnName = case paramStack of
        ((name, _):_) | name /= "main" -> Just name
        _ -> Nothing
      scriptName = case scriptStack of
        (s:_) -> Just (toText s)
        _ -> Nothing
  pure $ case (fnName, mLine, scriptName) of
    (Just fn, Just line, Just sc) -> msg <> " in fn **" <> fn <> "** line >> " <> line <> " << in " <> sc
    (Just fn, Just line, Nothing) -> msg <> " in fn **" <> fn <> "** line >> " <> line <> " <<"
    (Just fn, Nothing, Just sc)   -> msg <> " in fn **" <> fn <> "** in " <> sc
    (Just fn, Nothing, Nothing)   -> msg <> " in fn **" <> fn <> "**"
    (Nothing, _, Just sc)         -> msg <> " in " <> sc
    _                             -> msg

flagError :: Led ()
flagError = modify (\s -> s { ledErrorOccurred = True, ledCommandError = True })

printHelpIfActive :: Led ()
printHelpIfActive = gets ledHelpMode >>= bool (pure ()) printLastError
  where
    printLastError = gets ledLastError >>= traverse_ (outputLine . toString)

guardChanged :: Led Bool
guardChanged = getChangeFlag >>= \case
    Changed -> warnUnsaved >> pure False
    _       -> pure True
  where
    warnUnsaved = do
      setErrorText "Warning: buffer modified"
      flagError
      setChangeFlag ChangedAndWarned
      outputLine "?"
      printHelpIfActive

ensureNonEmptyDocList :: Led ()
ensureNonEmptyDocList = do
  dl <- gets ledDocumentList
  when (documentCount dl == 0) $
    modify (\s -> s { ledDocumentList = singletonDocumentList "" emptyDocumentState })

resolveTargetAddress :: Addr -> Led (Maybe Int)
resolveTargetAddress addr = do
    cur <- getCurrentLine
    doc <- getDocument
    let total = LedDocument.lineCount doc
    marks <- getMarks
    case resolveAddr cur total marks (LedDocument.documentLines doc) addr of
      Left err -> addressError err *> pure Nothing
      Right n
        | n < 0     -> addressError "Invalid address" *> pure Nothing
        | otherwise -> pure (Just n)

removeDocStates :: Int -> Int -> Led ()
removeDocStates start end = do
  dl <- gets ledDocumentList
  let docs = dlDocuments dl
      count = documentCount dl
      actualEnd = min end count
  when (start >= 1 && start <= count) $ do
    let (before, rest) = Seq.splitAt (start - 1) docs
        after = Seq.drop (actualEnd - start + 1) rest
        newDocs = before <> after
    modify (\s -> s { ledDocumentList = dl { dlDocuments = newDocs } })

insertDocState :: Int -> DocumentState -> Led ()
insertDocState pos ds = do
  dl <- gets ledDocumentList
  let docs = dlDocuments dl
      (before, after) = Seq.splitAt pos docs
      newDocs = before <> Seq.singleton ds <> after
  modify (\s -> s { ledDocumentList = dl { dlDocuments = newDocs } })

openDocState :: FilePath -> Led DocumentState
openDocState rawPath
  | null rawPath = pure emptyDocumentState
  | otherwise = do
      path <- liftIO (expandPath rawPath)
      result <- liftIO (readDocument path)
      case result of
        Left err -> do
          unless ("does not exist" `T.isInfixOf` err || "No such file" `T.isInfixOf` err) $ do
            setErrorText err
            gets ledSilent >>= bool (outputStrLn' (toString err)) (pure ())
          pure emptyDocumentState
        Right (doc, n) -> do
          humanPath <- liftIO (humanisePath path)
          printByteCount' n humanPath
          pure DocumentState
            { docDocument = doc
            , docCurrentLine = LedDocument.lineCount doc
            , docMarks = Map.empty
            , docChangeFlag = Unchanged
            }
  where
    printByteCount' n p = gets ledSilent >>= bool (outputLine . toString $ show n <> " <- " <> toText p) (pure ())

syncDlCurrentDoc :: Led ()
syncDlCurrentDoc = do
  dl <- gets ledDocumentList
  let bufferCurLine = docCurrentLine (dlDocListState dl)
      total = documentCount dl
      newCur | total == 0 = 0
             | bufferCurLine < 1 = 1
             | bufferCurLine > total = total
             | otherwise = bufferCurLine
  modify (\s -> s { ledDocumentList = setDlCurrentDoc newCur (ledDocumentList s) })

applyDocListDiff :: DocumentState -> [Text] -> Led ()
applyDocListDiff origDlState newLines = do
  let oldLines = LedDocument.documentLines (docDocument origDlState)
  if oldLines == newLines then pure ()
  else do
      dl <- gets ledDocumentList
      let oldCount = length oldLines
          newCount = length newLines
          prefix = length (takeWhile id (zipWith (==) oldLines newLines))
          oldRev = reverse (drop prefix oldLines)
          newRev = reverse (drop prefix newLines)
          suffix = length (takeWhile id (zipWith (==) oldRev newRev))
          deletedCount = oldCount - prefix - suffix
          insertedLines = take (newCount - prefix - suffix) (drop prefix newLines)
          deleteStart = prefix + 1
          deleteEnd = prefix + deletedCount
      aborted <- if deletedCount > 0
        then do
          let hasUnsaved = any (\idx ->
                maybe False (\ds -> docChangeFlag ds == Changed) (getDocStateAt idx dl))
                [deleteStart..deleteEnd]
          if hasUnsaved
            then do
              setErrorText "Warning: document modified"
              flagError
              forM_ [deleteStart..deleteEnd] $ \idx ->
                case getDocStateAt idx dl of
                  Just ds | docChangeFlag ds == Changed ->
                    modify (\s -> s { ledDocumentList = setDocStateAt idx (ds { docChangeFlag = ChangedAndWarned }) (ledDocumentList s) })
                  _ -> pure ()
              modify (\s -> let dl' = ledDocumentList s
                            in s { ledDocumentList = dl' { dlDocListState = origDlState } })
              outputLine "?"
              printHelpIfActive
              pure True
            else do
              removeDocStates deleteStart deleteEnd
              pure False
        else pure False
      unless aborted $ do
        when (not (null insertedLines)) $ do
          let insertPos = prefix
          forM_ (zip [0..] insertedLines) $ \(i, filename) -> do
            ds <- openDocState (toString filename)
            insertDocState (insertPos + i) ds
        syncDlCurrentDoc

applyManagedDiff :: DocumentState -> [Text] -> Led ()
applyManagedDiff origDlState newLines = do
  let oldLines = LedDocument.documentLines (docDocument origDlState)
  if oldLines == newLines then pure ()
  else do
      let diff = computeManageDiff oldLines newLines
      warnings <- liftIO (checkManageWarnings expandPath diff)
      warned <- gets ledManageWarned
      if hasWarnings warnings && not warned
        then do
          setErrorText ("Warning: " <> formatManageWarnings warnings)
          flagError
          modify (\s -> s { ledManageWarned = True })
          modify (\s -> let dl = ledDocumentList s
                        in s { ledDocumentList = dl { dlDocListState = origDlState } })
          outputLine "?"
          printHelpIfActive
        else do
          dl <- gets ledDocumentList
          let renamedStates = [ (idx, ds)
                              | (idx, _, _) <- mdRenames diff
                              , Just ds <- [getDocStateAt idx dl]
                              ]
          errors <- liftIO (executeManageActions expandPath diff)
          forM_ errors $ \err -> addressError err
          when (mdDeleteEnd diff >= mdDeleteStart diff) $
            removeDocStates (mdDeleteStart diff) (mdDeleteEnd diff)
          let insertPos = mdInsertPos diff
          forM_ (zip [0..] renamedStates) $ \(i, (_idx, ds)) ->
            insertDocState (insertPos + i) ds
          let createOffset = length renamedStates
          forM_ (zip [0..] (mdCreates diff)) $ \(i, filename) -> do
            ds <- openDocState (toString filename)
            insertDocState (insertPos + createOffset + i) ds
          syncDlCurrentDoc
          modify (\s -> s { ledManageWarned = False })

curAndTotal :: Led (Int, Int)
curAndTotal = do
  cur <- getCurrentLine
  doc <- getDocument
  pure (cur, LedDocument.lineCount doc)

splitGlobalCmds :: Text -> [Text]
splitGlobalCmds = go "" []
  where
    go acc cmds t = case T.uncons t of
      Nothing -> reverse (acc : cmds)
      Just ('\n', r) -> go "" (acc : cmds) r
      Just (c, r) -> go (acc <> T.singleton c) cmds r
