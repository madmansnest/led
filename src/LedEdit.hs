module LedEdit
  ( appendLinesAt, insertLinesAt, changeLinesWithText, deleteLines, joinLines
  , transferLines, moveLines
  , executeTransfer, executeMove
  , transferToDocuments, moveToDocuments, insertIntoDocument
  , executeCrossDocTransfer, executeCrossDocMove
  , transferToParamDoc, moveToParamDoc, insertIntoParamDoc
  , substituteCommand
  , substituteLine, substituteFirst, substituteAll, substituteNth, rebuildLine, substituteLinesLoop
  ) where

import qualified Data.Text as T

import LedCore (LedState(..))
import LedInput (Led, readInputText)
import LedNexus (BufferChangeFlag(..), DocumentState(..), documentCount, getDocStateAt, setDocStateAt, dlDocList, dlCurrentDoc, dlDocListState, insertLinesIntoDocState)
import LedParse (Addr, DocRange(..), SubstFlags(..), TargetAddr(..))
import LedResolve (resolveDocRange)
import LedSession (adjustMarksDelete, adjustMarksInsert)
import LedSmartReplace (isSmartReplaceEligible, smartReplace)
import LedState
import qualified LedDocument
import qualified LedRegularExpressions as RE

commitChange :: LedDocument.Document -> Int -> Led ()
commitChange doc newLine = do
  setDocument doc
  setCurrentLine newLine
  setChangeFlag Changed

withValidRange :: Int -> Int -> Led () -> Led ()
withValidRange start end action
  | start == 0 || end == 0 = addressError "Invalid address"
  | otherwise = action

withDocRange :: DocRange -> (Int -> Led ()) -> Led ()
withDocRange docRange action = do
  dl <- gets ledDocumentList
  let curDoc = dlCurrentDoc dl
      total = documentCount dl
      docFilenames = LedDocument.documentLines (dlDocList dl)
      dlMarks = docMarks (dlDocListState dl)
  case resolveDocRange curDoc total docFilenames dlMarks docRange of
    Left err -> addressError err
    Right (docStart, docEnd) -> forM_ [docStart..docEnd] action

appendLinesAt :: Int -> [Text] -> Led ()
appendLinesAt _ [] = pure ()
appendLinesAt addr ls = do
  doc <- getDocument
  let doc' = LedDocument.replaceLines (addr + 1) 0 ls doc
  commitChange doc' (addr + length ls)
  adjustMarksInsert addr (length ls)

insertLinesAt :: Int -> [Text] -> Led ()
insertLinesAt _ [] = pure ()
insertLinesAt addr ls = do
  let pos = max 1 addr
  doc <- getDocument
  let doc' = LedDocument.replaceLines pos 0 ls doc
  commitChange doc' (pos + length ls - 1)
  adjustMarksInsert (pos - 1) (length ls)

changeLinesWithText :: Maybe Text -> Int -> Int -> Led ()
changeLinesWithText inlineTxt start end = withValidRange start end $ do
  inputLines <- maybe readInputText (pure . (: [])) inlineTxt
  doc <- getDocument
  let doc' = LedDocument.replaceLines start (end - start + 1) inputLines doc
      total = LedDocument.lineCount doc'
      newLine
        | not (null inputLines) = start + length inputLines - 1
        | start - 1 > 0 && start - 1 <= total = start - 1
        | total > 0 = total
        | otherwise = 0
  commitChange doc' newLine
  adjustMarksDelete start end
  unless (null inputLines) $ adjustMarksInsert (start - 1) (length inputLines)

deleteLines :: Int -> Int -> Led ()
deleteLines start end = withValidRange start end $ do
  doc <- getDocument
  let doc' = LedDocument.replaceLines start (end - start + 1) [] doc
      total = LedDocument.lineCount doc'
  commitChange doc' (min start total)
  adjustMarksDelete start end

joinLines :: Int -> Int -> Led ()
joinLines start end = withValidRange start end $
  when (start < end) $ do
    doc <- getDocument
    let lns = LedDocument.getLines start end doc
        doc' = LedDocument.replaceLines start (end - start + 1) [T.concat lns] doc
    commitChange doc' start
    adjustMarksDelete (start + 1) end

transferLines :: Int -> Int -> Int -> Led ()
transferLines start end dest = do
  doc <- getDocument
  let srcLines = LedDocument.getLines start end doc
      doc' = LedDocument.replaceLines (dest + 1) 0 srcLines doc
  commitChange doc' (dest + length srcLines)
  adjustMarksInsert dest (length srcLines)

moveLines :: Int -> Int -> Int -> Led ()
moveLines start end dest
  | dest >= start && dest <= end = addressError "Invalid address"
  | otherwise = do
      doc <- getDocument
      let srcLines = LedDocument.getLines start end doc
          count = end - start + 1
          adjDest = if dest > end then dest - count else dest
          doc' = LedDocument.replaceLines (adjDest + 1) 0 srcLines
               $ LedDocument.replaceLines start count [] doc
      commitChange doc' (adjDest + length srcLines)
      adjustMarksDelete start end
      adjustMarksInsert adjDest count

deleteSourceLines :: Int -> Int -> Led ()
deleteSourceLines start end = do
  doc <- getDocument
  let doc' = LedDocument.replaceLines start (end - start + 1) [] doc
      total = LedDocument.lineCount doc'
  commitChange doc' (min start total)
  adjustMarksDelete start end

executeTransfer :: TargetAddr -> Int -> Int -> Led ()
executeTransfer target srcStart srcEnd = case target of
  LocalTarget addr ->
    resolveTargetAddress addr >>= traverse_ (transferLines srcStart srcEnd)
  CrossDocTarget docRange lineAddr ->
    transferToDocuments docRange lineAddr srcStart srcEnd
  ParamTarget _depth lineAddr ->
    transferToParamDoc lineAddr srcStart srcEnd

executeMove :: TargetAddr -> Int -> Int -> Led ()
executeMove target srcStart srcEnd = case target of
  LocalTarget addr ->
    resolveTargetAddress addr >>= traverse_ (moveLines srcStart srcEnd)
  CrossDocTarget docRange lineAddr ->
    moveToDocuments docRange lineAddr srcStart srcEnd
  ParamTarget _depth lineAddr ->
    moveToParamDoc lineAddr srcStart srcEnd

transferToDocuments :: DocRange -> Addr -> Int -> Int -> Led ()
transferToDocuments docRange lineAddr srcStart srcEnd = do
  srcLines <- LedDocument.getLines srcStart srcEnd <$> getDocument
  withDocRange docRange $ \docIdx ->
    insertIntoDocument docIdx lineAddr srcLines

moveToDocuments :: DocRange -> Addr -> Int -> Int -> Led ()
moveToDocuments docRange lineAddr srcStart srcEnd = do
  srcLines <- LedDocument.getLines srcStart srcEnd <$> getDocument
  withDocRange docRange $ \docIdx ->
    insertIntoDocument docIdx lineAddr srcLines
  deleteSourceLines srcStart srcEnd

insertIntoDocument :: Int -> Addr -> [Text] -> Led ()
insertIntoDocument docIdx lineAddr srcLines = do
  dl <- gets ledDocumentList
  case getDocStateAt docIdx dl of
    Nothing -> addressError "Invalid document"
    Just ds -> case insertLinesIntoDocState lineAddr srcLines ds of
      Left err -> addressError err
      Right ds' -> modify $ \s ->
        s { ledDocumentList = setDocStateAt docIdx ds' (ledDocumentList s) }

executeCrossDocTransfer :: Int -> Addr -> Int -> Int -> Led ()
executeCrossDocTransfer origDoc addr srcStart srcEnd = do
  srcLines <- LedDocument.getLines srcStart srcEnd <$> getDocument
  insertIntoDocument origDoc addr srcLines

executeCrossDocMove :: Int -> Addr -> Int -> Int -> Led ()
executeCrossDocMove origDoc addr srcStart srcEnd = do
  executeCrossDocTransfer origDoc addr srcStart srcEnd
  unlessError $ deleteSourceLines srcStart srcEnd
  where
    unlessError action = do
      hasError <- gets ledCommandError
      unless hasError action

transferToParamDoc :: Addr -> Int -> Int -> Led ()
transferToParamDoc lineAddr srcStart srcEnd = do
  srcLines <- LedDocument.getLines srcStart srcEnd <$> getDocument
  insertIntoParamDoc lineAddr srcLines
  setCurrentLine srcEnd

moveToParamDoc :: Addr -> Int -> Int -> Led ()
moveToParamDoc lineAddr srcStart srcEnd = do
  srcLines <- LedDocument.getLines srcStart srcEnd <$> getDocument
  insertIntoParamDoc lineAddr srcLines
  deleteSourceLines srcStart srcEnd

insertIntoParamDoc :: Addr -> [Text] -> Led ()
insertIntoParamDoc lineAddr srcLines = do
  paramStack <- gets ledParamStack
  case paramStack of
    [] -> addressError "No parameter document"
    ((name, ds):rest) -> case insertLinesIntoDocState lineAddr srcLines ds of
      Left err -> addressError err
      Right ds' -> modify $ \s -> s { ledParamStack = (name, ds') : rest }

substituteCommand :: Text -> Text -> SubstFlags -> Int -> Int -> Led ()
substituteCommand reText repl' flags start end = do
  lastRE <- gets ledLastRE
  lastRepl <- gets ledLastReplacement
  let mbre = if T.null reText
             then maybe (Left "No previous regular expression") Right lastRE
             else RE.parseBRE reText
      mrepl = if repl' == "%"
              then maybe (Left "No previous substitution") Right lastRepl
              else Right repl'
  case (mbre, mrepl) of
    (Left err, _) -> addressError err
    (_, Left err) -> addressError err
    (Right bre, Right actualRepl) -> do
      modify $ \s -> s { ledLastRE = Just bre, ledLastReplacement = Just actualRepl }
      doc <- getDocument
      let lns = LedDocument.getLines start end doc
          smartMode = isSmartReplaceEligible reText actualRepl (sfInsensitive flags)
          (anyChanged, newLines, lastChanged) =
            substituteLinesLoop bre actualRepl (sfGlobal flags) (sfCount flags) smartMode start lns
      if not anyChanged
        then addressError "No match"
        else do
          let doc' = LedDocument.replaceLines start (end - start + 1) newLines doc
          commitChange doc' lastChanged

substituteLine :: RE.BRE -> Text -> Bool -> Int -> Bool -> Text -> Text
substituteLine bre rpl isGlobal cnt smartMode line
  | isGlobal  = substituteAll bre rpl smartMode line
  | cnt > 0   = substituteNth bre rpl cnt smartMode line
  | otherwise = substituteFirst bre rpl smartMode line

substituteFirst :: RE.BRE -> Text -> Bool -> Text -> Text
substituteFirst bre rpl smartMode line =
  let mMatch = if smartMode then RE.matchBREInsensitive bre line else RE.matchBRE bre line
  in case mMatch of
    Nothing -> line
    Just m  ->
      let baseRepl = RE.buildReplacement rpl m
          finalRepl = if smartMode then smartReplace (RE.matchText m) baseRepl else baseRepl
      in T.take (RE.matchStart m) line <> finalRepl <> T.drop (RE.matchEnd m) line

substituteAll :: RE.BRE -> Text -> Bool -> Text -> Text
substituteAll bre rpl smartMode line =
  let matches = if smartMode then RE.matchAllBREInsensitive bre line else RE.matchAllBRE bre line
  in if null matches then line else rebuildLine line rpl smartMode matches

rebuildLine :: Text -> Text -> Bool -> [RE.Match] -> Text
rebuildLine line rpl smartMode = go 0
  where
    go pos [] = T.drop pos line
    go pos (m:ms) =
      let baseRepl = RE.buildReplacement rpl m
          finalRepl = if smartMode then smartReplace (RE.matchText m) baseRepl else baseRepl
      in T.take (RE.matchStart m - pos) (T.drop pos line)
         <> finalRepl
         <> go (RE.matchEnd m) ms

substituteNth :: RE.BRE -> Text -> Int -> Bool -> Text -> Text
substituteNth bre rpl n smartMode line = go line 0 1
  where
    matchFn = if smartMode then RE.matchBREInsensitive bre else RE.matchBRE bre
    go t offset occurrence = case matchFn t of
      Nothing -> line
      Just m
        | occurrence == n ->
            let baseRepl = RE.buildReplacement rpl m
                finalRepl = if smartMode then smartReplace (RE.matchText m) baseRepl else baseRepl
            in T.take (offset + RE.matchStart m) line
               <> finalRepl
               <> T.drop (offset + RE.matchEnd m) line
        | otherwise ->
            let nextStart = RE.matchEnd m
                nextStart' = if nextStart == 0 then 1 else nextStart
            in go (T.drop nextStart' t) (offset + nextStart') (occurrence + 1)

substituteLinesLoop :: RE.BRE -> Text -> Bool -> Int -> Bool -> Int -> [Text] -> (Bool, [Text], Int)
substituteLinesLoop bre repl' isGlobal cnt smartMode startLine lns = go lns startLine False [] startLine
  where
    go [] _ changed acc lastCh = (changed, reverse acc, lastCh)
    go (l:ls) lineNum changed acc lastCh =
      let newLine = substituteLine bre repl' isGlobal cnt smartMode l
          didChange = newLine /= l
          splitLines = T.splitOn "\n" newLine
          numNew = length splitLines
          newLastCh = if didChange then lineNum + numNew - 1 else lastCh
          nextLineNum = lineNum + numNew
      in go ls nextLineNum (changed || didChange) (reverse splitLines ++ acc) newLastCh
