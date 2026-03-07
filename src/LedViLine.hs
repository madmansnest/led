module LedViLine
  ( InputState(..)
  , emptyInput
  , inputText
  , inputFromText

  , insertChar
  , deleteBack
  , deleteAt
  , deleteWord
  , moveLeft
  , moveRight
  , moveHome
  , moveEnd
  , killToEnd
  , killToStart

  , HistoryNav(..)
  , emptyHistoryNav
  , loadHistoryNav
  , saveHistoryNav
  , addToHistory
  , historyUp
  , historyDown
  ) where

import qualified Data.Text as T
import System.IO.Error (catchIOError)

-- Text before cursor is stored reversed for efficient cursor operations.
data InputState = InputState !Text !Text
  deriving stock (Eq, Show)

emptyInput :: InputState
emptyInput = InputState "" ""

inputText :: InputState -> Text
inputText (InputState before after) = T.reverse before <> after

inputFromText :: Text -> InputState
inputFromText t = InputState (T.reverse t) ""



insertChar :: Char -> InputState -> InputState
insertChar c (InputState before after) = InputState (T.cons c before) after

deleteBack :: InputState -> InputState
deleteBack (InputState before after) = case T.uncons before of
  Nothing -> InputState before after
  Just (_, rest) -> InputState rest after

deleteAt :: InputState -> InputState
deleteAt (InputState before after) = case T.uncons after of
  Nothing -> InputState before after
  Just (_, rest) -> InputState before rest

-- Deletes backward through whitespace, then through non-whitespace.
deleteWord :: InputState -> InputState
deleteWord (InputState before after) =
  let -- Skip trailing whitespace first
      afterSpaces = T.dropWhile isSpace before
      -- Then delete the word (non-space characters)
      afterWord = T.dropWhile (not . isSpace) afterSpaces
  in InputState afterWord after
  where
    isSpace c = c == ' ' || c == '\t'

moveLeft :: InputState -> InputState
moveLeft (InputState before after) = case T.uncons before of
  Nothing -> InputState before after
  Just (c, rest) -> InputState rest (T.cons c after)

moveRight :: InputState -> InputState
moveRight (InputState before after) = case T.uncons after of
  Nothing -> InputState before after
  Just (c, rest) -> InputState (T.cons c before) rest

moveHome :: InputState -> InputState
moveHome is = InputState "" (inputText is)

moveEnd :: InputState -> InputState
moveEnd is = InputState (T.reverse $ inputText is) ""

killToEnd :: InputState -> InputState
killToEnd (InputState before _) = InputState before ""

killToStart :: InputState -> InputState
killToStart (InputState _ after) = InputState "" after


data HistoryNav = HistoryNav
  { hnHistory  :: ![Text]    -- ^ Command history (newest first)
  , hnIndex    :: !Int       -- ^ Current position (-1 = new input, 0+ = history index)
  , hnOriginal :: !Text      -- ^ Original text when user started navigating
  , hnFilePath :: !(Maybe FilePath)  -- ^ Path to history file
  } deriving stock (Eq, Show)

emptyHistoryNav :: HistoryNav
emptyHistoryNav = HistoryNav [] (-1) "" Nothing

loadHistoryNav :: Maybe FilePath -> IO HistoryNav
loadHistoryNav Nothing = return emptyHistoryNav
loadHistoryNav (Just path) = catchIOError doLoad handleErr
  where
    doLoad = do
      contents <- readFileBS path
      let lns = T.lines (decodeUtf8 contents)
      -- File stores oldest first, we want newest first
      return $ HistoryNav (reverse lns) (-1) "" (Just path)
    handleErr _ = return $ emptyHistoryNav { hnFilePath = Just path }

saveHistoryNav :: HistoryNav -> IO ()
saveHistoryNav hn = case hnFilePath hn of
  Nothing -> return ()
  Just path -> catchIOError doSave handleErr
    where
      doSave = writeFileBS path $ encodeUtf8 $ T.unlines $ reverse $ take 1000 $ hnHistory hn
      handleErr _ = return ()

addToHistory :: Text -> HistoryNav -> HistoryNav
addToHistory cmd hn
  | T.null (T.strip cmd) = hn  -- Skip empty commands
  | Just cmd == viaNonEmpty head (hnHistory hn) = hn  -- Skip consecutive duplicates
  | otherwise = hn { hnHistory = cmd : hnHistory hn, hnIndex = -1, hnOriginal = "" }

indexList :: [a] -> Int -> Maybe a
indexList xs i = viaNonEmpty head (drop i xs)

historyUp :: InputState -> HistoryNav -> Maybe (InputState, HistoryNav)
historyUp currentInput hn =
  let hist = hnHistory hn
      curIdx = hnIndex hn
      nextIdx = curIdx + 1
  in case indexList hist nextIdx of
       Nothing -> Nothing  -- At oldest entry
       Just entry ->
         let newHn = if curIdx == -1
                     then hn { hnIndex = nextIdx, hnOriginal = inputText currentInput }
                     else hn { hnIndex = nextIdx }
             newInput = inputFromText entry
         in Just (newInput, newHn)

historyDown :: InputState -> HistoryNav -> Maybe (InputState, HistoryNav)
historyDown _currentInput hn =
  let curIdx = hnIndex hn
  in if curIdx < 0
     then Nothing  -- Already at new input
     else if curIdx == 0
          then Just (inputFromText (hnOriginal hn), hn { hnIndex = -1, hnOriginal = "" })
          else case indexList (hnHistory hn) (curIdx - 1) of
                 Nothing -> Nothing
                 Just entry -> Just (inputFromText entry, hn { hnIndex = curIdx - 1 })


