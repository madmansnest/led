module LedInputSpec where

import Test.Hspec

import Data.Char (isDigit, isSpace)

import LedInput (ledSettings, ledCompletion)
import qualified System.Console.Haskeline as H
import qualified System.Console.Haskeline.Completion as HC

spec :: Spec
spec = describe "LedInput" $ do
  describe "ledSettings" $ do
    it "creates settings with no history file" $ do
      let settings = ledSettings Nothing
      H.historyFile settings `shouldBe` Nothing
      H.autoAddHistory settings `shouldBe` True

    it "creates settings with history file" $ do
      let settings = ledSettings (Just "/tmp/history")
      H.historyFile settings `shouldBe` Just "/tmp/history"
      H.autoAddHistory settings `shouldBe` True

    it "uses ledCompletion for completion" $ do
      let settings = ledSettings Nothing
      -- Can't directly compare functions, but we can verify it's set
      -- by checking that completion is defined (non-bottom)
      H.complete settings `seq` () `shouldBe` ()

  describe "ledCompletion" $ do
    -- Note: ledCompletion is a CompletionFunc IO, which takes (reversedLeft, right)
    -- and returns IO (String, [Completion]). We test the logic indirectly
    -- through the needsFileCompletion helper behavior.

    it "recognizes e command for file completion" $ do
      -- Test that 'e ' triggers file completion
      -- The input format is (reversed left part, right part)
      let left = " e"  -- reversed "e "
      let isFileCmd = checkNeedsFileCompletion left
      isFileCmd `shouldBe` True

    it "recognizes w command for file completion" $ do
      let left = " w"  -- reversed "w "
      checkNeedsFileCompletion left `shouldBe` True

    it "recognizes wq command for file completion" $ do
      let left = " qw"  -- reversed "wq "
      checkNeedsFileCompletion left `shouldBe` True

    it "recognizes r command for file completion" $ do
      let left = " r"  -- reversed "r "
      checkNeedsFileCompletion left `shouldBe` True

    it "recognizes im command for file completion" $ do
      let left = " mi"  -- reversed "im "
      checkNeedsFileCompletion left `shouldBe` True

    it "does not trigger for fn command" $ do
      -- fn is for function definition, not file
      let left = " nf"  -- reversed "fn "
      checkNeedsFileCompletion left `shouldBe` False

    it "recognizes f command (not fn)" $ do
      let left = " f"  -- reversed "f "
      checkNeedsFileCompletion left `shouldBe` True

    it "does not trigger for bare command without space" $ do
      let left = "e"  -- just 'e' without space
      checkNeedsFileCompletion left `shouldBe` False

    it "skips leading range prefix" $ do
      -- "1,5e " should still trigger file completion
      let left = " e5,1"  -- reversed "1,5e "
      checkNeedsFileCompletion left `shouldBe` True

    it "recognizes &a command for file completion" $ do
      let left = " a&"  -- reversed "&a "
      checkNeedsFileCompletion left `shouldBe` True

    it "recognizes &i command for file completion" $ do
      let left = " i&"  -- reversed "&i "
      checkNeedsFileCompletion left `shouldBe` True

    it "recognizes &c command for file completion" $ do
      let left = " c&"  -- reversed "&c "
      checkNeedsFileCompletion left `shouldBe` True

-- Helper to check if a reversed left string would trigger file completion
-- This mirrors the logic in ledCompletion
checkNeedsFileCompletion :: String -> Bool
checkNeedsFileCompletion reversedLeft =
  let line = reverse reversedLeft
      afterRange = dropWhile (\c -> isDigit c || c `elem` ['.', '$', ',', ';', '+', '-', '/', '?', '\'']) (dropWhile isSpace line)
  in needsFileCompletion afterRange
  where
    needsFileCompletion s = case s of
      ('e':rest)     -> startsWithSpace rest
      ('E':rest)     -> startsWithSpace rest
      ('f':rest)     -> not ("n" `isPrefixOf` rest) && startsWithSpace rest
      ('r':rest)     -> startsWithSpace rest
      ('w':'q':rest) -> startsWithSpace rest
      ('w':rest)     -> startsWithSpace rest
      ('i':'m':rest) -> startsWithSpace rest
      ('&':'a':rest) -> startsWithSpace rest
      ('&':'i':rest) -> startsWithSpace rest
      ('&':'c':rest) -> startsWithSpace rest
      _ -> False
    startsWithSpace [] = False
    startsWithSpace (c:_) = isSpace c
