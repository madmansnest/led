module LedVi.Completion
  ( CompletionResult(..)
  , performCompletion
  , findCommonPrefix
  ) where

import qualified Data.Text as T

import LedInput (runCompletion, Completion(..))
import LedVi.Line (InputState(..))


data CompletionResult
  = NoCompletions
    -- ^ No completions found
  | SingleCompletion !InputState
    -- ^ Single completion, apply it
  | MultipleWithPrefix !InputState
    -- ^ Multiple completions, applied common prefix
  | MultipleNoPrefix ![Text]
    -- ^ Multiple completions with no common prefix, show them
  deriving stock (Eq, Show)


-- Takes the input state and returns the completion result.
performCompletion :: InputState -> IO CompletionResult
performCompletion (InputState before after) = do
  -- Note: before is stored reversed, so reverse it for completion
  let beforeNormal = T.reverse before

  -- Run completion
  (remaining, completions) <- runCompletion (beforeNormal, after)

  case completions of
    [] ->
      -- No completions found
      pure NoCompletions

    [c] -> do
      -- Single completion - apply it
      let newBeforeNormal = remaining <> toText (replacement c)
          newBefore = T.reverse newBeforeNormal
          newInput = InputState newBefore after
      pure (SingleCompletion newInput)

    (c:cs) -> do
      -- Multiple completions - find common prefix and apply it
      let allReplacements = map replacement (c:cs)
          commonPfx = findCommonPrefix allReplacements
      if null commonPfx
        then do
          -- No common prefix - return completions to show
          let completionLines = map (toText . replacement) (c:cs)
          pure (MultipleNoPrefix completionLines)
        else do
          -- Apply common prefix
          let newBeforeNormal = remaining <> toText commonPfx
              newBefore = T.reverse newBeforeNormal
              newInput = InputState newBefore after
          pure (MultipleWithPrefix newInput)


findCommonPrefix :: [String] -> String
findCommonPrefix [] = []
findCommonPrefix [x] = x
findCommonPrefix (x:xs) = foldr commonPrefix' x xs
  where
    -- Find common prefix of two strings
    commonPrefix' :: String -> String -> String
    commonPrefix' [] _ = []
    commonPrefix' _ [] = []
    commonPrefix' (a:as) (b:bs)
      | a == b = a : commonPrefix' as bs
      | otherwise = []
