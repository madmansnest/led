module LedQueue
  ( getQueue
  , setQueue
  , prependToQueue
  , clearQueue
  , popQueue
  , saveAndRestoreQueue
  ) where

import LedCore (LedState(..))
import LedInput (Led)

getQueue :: Led [Text]
getQueue = gets ledInputQueue

setQueue :: [Text] -> Led ()
setQueue lns = modify (\s -> s { ledInputQueue = lns })

prependToQueue :: [Text] -> Led ()
prependToQueue lns = modify (\s -> s { ledInputQueue = lns ++ ledInputQueue s })

clearQueue :: Led ()
clearQueue = setQueue []

popQueue :: Led (Maybe Text)
popQueue = do
  queue <- getQueue
  case queue of
    [] -> pure Nothing
    (x:xs) -> setQueue xs *> pure (Just x)

saveAndRestoreQueue :: Led a -> Led a
saveAndRestoreQueue action = do
  savedQueue <- getQueue
  wasVisual <- gets ledVisualMode
  result <- action
  nowVisual <- gets ledVisualMode
  when (wasVisual == nowVisual) $ setQueue savedQueue
  pure result
