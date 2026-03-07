module LedSmartReplace
  ( isSmartReplaceEligible
  , smartReplace
  , isAllLower
  , isAllUpper
  ) where

import Data.Char (isLetter, isUpper, isLower, toUpper, toLower)
import qualified Data.Text as T

-- | Check if smart replace should be triggered.
-- Requires: search pattern all lowercase, replacement all lowercase, no i flag.
isSmartReplaceEligible :: Text -> Text -> Bool -> Bool
isSmartReplaceEligible searchPat replacement hasIFlag =
  not hasIFlag && isAllLower searchPat && isAllLower replacement

-- | Check if all letters in the text are lowercase.
-- Non-letter characters are ignored.
isAllLower :: Text -> Bool
isAllLower = T.all (\c -> not (isLetter c) || isLower c)

-- | Check if all letters in the text are uppercase.
-- Non-letter characters are ignored.
isAllUpper :: Text -> Bool
isAllUpper = T.all (\c -> not (isLetter c) || isUpper c)

-- | Apply case pattern from matched text to replacement.
-- If matched is all uppercase, make replacement all uppercase.
-- Otherwise, apply case character-by-character.
smartReplace :: Text -> Text -> Text
smartReplace matched replacement
  | isAllUpper matched && hasLetters matched = T.toUpper replacement
  | otherwise = applyCharByChar matched replacement

-- | Check if text contains at least one letter.
hasLetters :: Text -> Bool
hasLetters = T.any isLetter

-- | Apply case character-by-character from source to target.
-- Extra characters in target default to lowercase.
applyCharByChar :: Text -> Text -> Text
applyCharByChar src tgt = T.pack $ go (T.unpack src) (T.unpack tgt)
  where
    go :: String -> String -> String
    go _ [] = []
    go [] (t:ts) = toLower t : go [] ts
    go (s:ss) (t:ts)
      | isLetter s && isLetter t =
          (if isUpper s then toUpper t else toLower t) : go ss ts
      | isLetter t = t : go (s:ss) ts  -- s is not a letter, keep looking for case source
      | isLetter s = t : go ss ts      -- t is not a letter, consume case source
      | otherwise = t : go ss ts       -- neither is a letter
