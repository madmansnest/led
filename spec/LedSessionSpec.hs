module LedSessionSpec where

import Test.Hspec

import LedSession (countUnmatchedBraces, findMatchingBrace, isFnDefinition, rejectGlobalOnly)
import LedExec (findGlobalCmdlist)
import LedParse (Command(..))

spec :: Spec
spec = describe "LedSession" $ do
  describe "countUnmatchedBraces" $ do
    it "returns 0 for balanced braces" $ do
      countUnmatchedBraces "{}" `shouldBe` 0
      countUnmatchedBraces "{{}}" `shouldBe` 0
      countUnmatchedBraces "a{b}c" `shouldBe` 0

    it "returns positive for unmatched opening braces" $ do
      countUnmatchedBraces "{" `shouldBe` 1
      countUnmatchedBraces "{{" `shouldBe` 2
      countUnmatchedBraces "{a{b" `shouldBe` 2

    it "returns negative for unmatched closing braces" $ do
      countUnmatchedBraces "}" `shouldBe` (-1)
      countUnmatchedBraces "}}" `shouldBe` (-2)

    it "ignores escaped braces" $ do
      countUnmatchedBraces "\\{" `shouldBe` 0
      countUnmatchedBraces "\\}" `shouldBe` 0
      countUnmatchedBraces "{\\}" `shouldBe` 1

  describe "findMatchingBrace" $ do
    it "finds matching brace at depth 0" $ do
      findMatchingBrace "abc}" 0 `shouldBe` Just ("abc", "")
      findMatchingBrace "abc}def" 0 `shouldBe` Just ("abc", "def")

    it "handles nested braces" $ do
      findMatchingBrace "a{b}c}" 0 `shouldBe` Just ("a{b}c", "")
      findMatchingBrace "{a}b}" 0 `shouldBe` Just ("{a}b", "")

    it "returns Nothing if no matching brace" $ do
      findMatchingBrace "abc" 0 `shouldBe` Nothing
      findMatchingBrace "{abc" 1 `shouldBe` Nothing

    it "handles escaped braces" $ do
      findMatchingBrace "a\\}b}" 0 `shouldBe` Just ("a\\}b", "")

  describe "isFnDefinition" $ do
    it "returns True for fn commands" $ do
      isFnDefinition "fn" `shouldBe` True
      isFnDefinition "fn foo" `shouldBe` True
      isFnDefinition "fn foo { body }" `shouldBe` True
      isFnDefinition "  fn bar" `shouldBe` True

    it "returns False for non-fn commands" $ do
      isFnDefinition "fnx" `shouldBe` False
      isFnDefinition "afn" `shouldBe` False
      isFnDefinition "p" `shouldBe` False

  describe "rejectGlobalOnly" $ do
    it "rejects session commands" $ do
      rejectGlobalOnly Help `shouldBe` Just "Unexpected address"
      rejectGlobalOnly HelpMode `shouldBe` Just "Unexpected address"
      rejectGlobalOnly Quit `shouldBe` Just "Unexpected address"
      rejectGlobalOnly QuitAlways `shouldBe` Just "Unexpected address"

    it "allows other commands" $ do
      rejectGlobalOnly Comment `shouldBe` Nothing
      rejectGlobalOnly FnList `shouldBe` Nothing

  describe "findGlobalCmdlist" $ do
    it "detects simple g/re/cmdlist" $ do
      findGlobalCmdlist "g/./{=}n" `shouldBe` Just ("g/./", "{=}n")
      findGlobalCmdlist "g/foo/s/a/b/" `shouldBe` Just ("g/foo/", "s/a/b/")

    it "detects v command" $ do
      findGlobalCmdlist "v/pat/cmd" `shouldBe` Just ("v/pat/", "cmd")

    it "detects G and V commands" $ do
      findGlobalCmdlist "G/pat/" `shouldBe` Nothing  -- No cmdlist for interactive
      findGlobalCmdlist "V/pat/" `shouldBe` Nothing

    it "returns Nothing for non-global commands" $ do
      findGlobalCmdlist "s/foo/bar/" `shouldBe` Nothing
      findGlobalCmdlist "1,5p" `shouldBe` Nothing
      findGlobalCmdlist ".n" `shouldBe` Nothing

    it "handles address prefix" $ do
      findGlobalCmdlist "1,5g/re/cmd" `shouldBe` Just ("1,5g/re/", "cmd")
      findGlobalCmdlist ".g/re/p" `shouldBe` Just (".g/re/", "p")

    it "handles doc prefix" $ do
      findGlobalCmdlist "&g/re/cmd" `shouldBe` Just ("&g/re/", "cmd")
      findGlobalCmdlist "&&g/re/cmd" `shouldBe` Just ("&&g/re/", "cmd")
