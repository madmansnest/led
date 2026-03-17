module LedViLineSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import LedVi.Line


spec :: Spec
spec = do
  describe "LedViLine" $ do
    describe "InputState" $ do
      describe "emptyInput" $ do
        it "creates empty input state" $ do
          inputText emptyInput `shouldBe` ""

      describe "inputFromText" $ do
        it "creates input with cursor at end" $ do
          inputText (inputFromText "hello") `shouldBe` "hello"

      describe "insertChar" $ do
        it "inserts character at cursor" $ do
          let input = inputFromText "hllo"
              moved = moveLeft $ moveLeft $ moveLeft input  -- cursor after 'h'
              result = insertChar 'e' moved
          inputText result `shouldBe` "hello"

    describe "Line Editing" $ do
      describe "deleteBack" $ do
        it "deletes character before cursor" $ do
          let input = inputFromText "hello"
          inputText (deleteBack input) `shouldBe` "hell"

        it "does nothing at start of line" $ do
          let input = moveHome (inputFromText "hello")
          inputText (deleteBack input) `shouldBe` "hello"

      describe "deleteAt" $ do
        it "deletes character at cursor" $ do
          let input = moveHome (inputFromText "hello")
          inputText (deleteAt input) `shouldBe` "ello"

        it "does nothing at end of line" $ do
          let input = inputFromText "hello"
          inputText (deleteAt input) `shouldBe` "hello"

      describe "deleteWord" $ do
        it "deletes word before cursor" $ do
          let input = inputFromText "hello world"
          inputText (deleteWord input) `shouldBe` "hello "

        it "deletes word and trailing spaces" $ do
          let input = inputFromText "hello   "
          inputText (deleteWord input) `shouldBe` ""

        it "deletes through spaces then word" $ do
          let input = inputFromText "one two   "
          inputText (deleteWord input) `shouldBe` "one "

        it "handles single word" $ do
          let input = inputFromText "hello"
          inputText (deleteWord input) `shouldBe` ""

        it "does nothing on empty input" $ do
          inputText (deleteWord emptyInput) `shouldBe` ""

      describe "moveLeft" $ do
        it "moves cursor left" $ do
          let input = inputFromText "ab"
              moved = moveLeft input
          inputText (insertChar 'X' moved) `shouldBe` "aXb"

      describe "moveRight" $ do
        it "moves cursor right" $ do
          let input = moveHome (inputFromText "ab")
              moved = moveRight input
          inputText (insertChar 'X' moved) `shouldBe` "aXb"

      describe "moveHome" $ do
        it "moves cursor to start" $ do
          let input = moveHome (inputFromText "hello")
          inputText (insertChar 'X' input) `shouldBe` "Xhello"

      describe "moveEnd" $ do
        it "moves cursor to end" $ do
          let input = moveEnd (moveHome (inputFromText "hello"))
          inputText (insertChar 'X' input) `shouldBe` "helloX"

      describe "killToEnd" $ do
        it "deletes from cursor to end" $ do
          let input = moveLeft $ moveLeft $ inputFromText "hello"
          inputText (killToEnd input) `shouldBe` "hel"

      describe "killToStart" $ do
        it "deletes from start to cursor" $ do
          let input = moveLeft $ moveLeft $ inputFromText "hello"
          inputText (killToStart input) `shouldBe` "lo"

    describe "History Navigation" $ do
      describe "addToHistory" $ do
        it "adds command to history" $ do
          let hn = addToHistory "test" emptyHistoryNav
          hnHistory hn `shouldBe` ["test"]

        it "skips empty commands" $ do
          let hn = addToHistory "  " emptyHistoryNav
          hnHistory hn `shouldBe` []

        it "skips consecutive duplicates" $ do
          let hn1 = addToHistory "test" emptyHistoryNav
              hn2 = addToHistory "test" hn1
          hnHistory hn2 `shouldBe` ["test"]

        it "allows non-consecutive duplicates" $ do
          let hn1 = addToHistory "test" emptyHistoryNav
              hn2 = addToHistory "other" hn1
              hn3 = addToHistory "test" hn2
          hnHistory hn3 `shouldBe` ["test", "other", "test"]

      describe "historyUp" $ do
        it "navigates to older entry" $ do
          let hn = addToHistory "second" $ addToHistory "first" emptyHistoryNav
          case historyUp emptyInput hn of
            Just (newInput, _) -> inputText newInput `shouldBe` "second"
            Nothing -> expectationFailure "Expected Just"

        it "returns Nothing at oldest" $ do
          historyUp emptyInput emptyHistoryNav `shouldBe` Nothing

      describe "historyDown" $ do
        it "returns Nothing at newest" $ do
          historyDown emptyInput emptyHistoryNav `shouldBe` Nothing
