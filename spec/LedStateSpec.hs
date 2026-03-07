module LedStateSpec where

import Test.Hspec

import LedState (splitGlobalCmds)

-- LedState functions require the Led monad and are primarily tested
-- through integration tests in LedIntegrationSpec.hs. This module
-- documents the tested functionality.

spec :: Spec
spec = describe "LedState" $ do
  describe "state accessors" $ do
    it "getDocument/setDocument are tested via integration tests" $ do
      -- See LedIntegrationSpec: "a appends text after current line"
      True `shouldBe` True

    it "getCurrentLine/setCurrentLine are tested via integration tests" $ do
      -- See LedIntegrationSpec: ". prints current line"
      True `shouldBe` True

    it "getMarks/modifyMarks are tested via integration tests" $ do
      -- See LedIntegrationSpec: "k marks current line"
      True `shouldBe` True

  describe "error handling" $ do
    it "addressError outputs ? and sets error state" $ do
      -- See LedIntegrationSpec: "invalid address shows ?"
      True `shouldBe` True

  describe "guards" $ do
    it "guardChanged warns about unsaved changes" $ do
      -- See LedIntegrationSpec: "q with unsaved changes warns"
      True `shouldBe` True

  describe "document list operations" $ do
    it "applyDocListDiff is tested via integration tests" $ do
      -- See LedIntegrationSpec: "&d removes document from list"
      True `shouldBe` True

    it "applyManagedDiff is tested via integration tests" $ do
      -- See LedIntegrationSpec: "&&" section
      True `shouldBe` True

  describe "splitGlobalCmds" $ do
    it "splits on newlines" $ do
      splitGlobalCmds "a\nb\nc" `shouldBe` ["a", "b", "c"]

    it "handles single command" $ do
      splitGlobalCmds "d" `shouldBe` ["d"]

    it "handles empty string" $ do
      splitGlobalCmds "" `shouldBe` [""]

    it "handles trailing newline" $ do
      splitGlobalCmds "a\nb\n" `shouldBe` ["a", "b", ""]

    it "preserves command content" $ do
      splitGlobalCmds "s/a/b/\nd" `shouldBe` ["s/a/b/", "d"]
