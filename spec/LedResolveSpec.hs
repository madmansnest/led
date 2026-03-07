module LedResolveSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import LedParse (Addr(..), DocRange(..), LineRange(..))
import LedResolve

spec :: Spec
spec = do
  describe "resolveAddr" $ do
    describe "basic addresses" $ do
      it "resolves Current (.) to current line" $ do
        resolveAddr 5 10 Map.empty [] Current `shouldBe` Right 5

      it "resolves LastLine ($) to total lines" $ do
        resolveAddr 5 10 Map.empty [] LastLine `shouldBe` Right 10

      it "resolves Number to the given line" $ do
        resolveAddr 5 10 Map.empty [] (Number 3) `shouldBe` Right 3

      it "rejects Number out of bounds (negative)" $ do
        resolveAddr 5 10 Map.empty [] (Number (-1)) `shouldBe` Left "Invalid address"

      it "rejects Number out of bounds (too large)" $ do
        resolveAddr 5 10 Map.empty [] (Number 15) `shouldBe` Left "Invalid address"

      it "allows Number 0 (for append)" $ do
        resolveAddr 5 10 Map.empty [] (Number 0) `shouldBe` Right 0

    describe "marks" $ do
      it "resolves Mark to marked line" $ do
        let marks = Map.singleton 'a' 7
        resolveAddr 5 10 marks [] (Mark 'a') `shouldBe` Right 7

      it "fails for unset mark" $ do
        resolveAddr 5 10 Map.empty [] (Mark 'a') `shouldBe` Left "Mark not set"

      it "fails for mark referencing deleted line" $ do
        let marks = Map.singleton 'a' 15
        resolveAddr 5 10 marks [] (Mark 'a') `shouldBe` Left "Mark references deleted line"

    describe "offsets" $ do
      it "resolves positive offset from current" $ do
        resolveAddr 5 10 Map.empty [] (AddrOffset Current 2) `shouldBe` Right 7

      it "resolves negative offset from current" $ do
        resolveAddr 5 10 Map.empty [] (AddrOffset Current (-2)) `shouldBe` Right 3

      it "resolves offset from number" $ do
        resolveAddr 5 10 Map.empty [] (AddrOffset (Number 3) 2) `shouldBe` Right 5

      it "rejects offset out of bounds" $ do
        resolveAddr 5 10 Map.empty [] (AddrOffset Current 10) `shouldBe` Left "Invalid address"

    describe "search addresses" $ do
      it "finds forward match" $ do
        let lines' = ["aaa", "bbb", "ccc", "ddd"]
        resolveAddr 1 4 Map.empty lines' (Next "ccc") `shouldBe` Right 3

      it "wraps forward search" $ do
        let lines' = ["aaa", "bbb", "ccc", "ddd"]
        resolveAddr 4 4 Map.empty lines' (Next "aaa") `shouldBe` Right 1

      it "finds backward match" $ do
        let lines' = ["aaa", "bbb", "ccc", "ddd"]
        resolveAddr 4 4 Map.empty lines' (Prev "bbb") `shouldBe` Right 2

      it "wraps backward search" $ do
        let lines' = ["aaa", "bbb", "ccc", "ddd"]
        resolveAddr 1 4 Map.empty lines' (Prev "ddd") `shouldBe` Right 4

      it "fails when no match found" $ do
        let lines' = ["aaa", "bbb", "ccc"]
        resolveAddr 1 3 Map.empty lines' (Next "zzz") `shouldBe` Left "No match"

      it "supports regex patterns" $ do
        let lines' = ["foo", "bar123", "baz"]
        resolveAddr 1 3 Map.empty lines' (Next "bar[0-9]*") `shouldBe` Right 2

  describe "resolveLineRange" $ do
    it "resolves single address to range" $ do
      resolveLineRange 5 10 Map.empty [] (LineSingle (Number 3)) `shouldBe` Right (3, 3)

    it "resolves comma range" $ do
      resolveLineRange 5 10 Map.empty [] (LineFree (Number 2) (Number 5)) `shouldBe` Right (2, 5)

    it "resolves semicolon range with context update" $ do
      -- In 3;+2 the second address is relative to 3, not current line
      resolveLineRange 1 10 Map.empty [] (LineBound (Number 3) (AddrOffset Current 2)) `shouldBe` Right (3, 5)

    it "rejects inverted range" $ do
      resolveLineRange 5 10 Map.empty [] (LineFree (Number 7) (Number 3)) `shouldBe` Left "Invalid address"

    it "rejects LineDefault" $ do
      resolveLineRange 5 10 Map.empty [] LineDefault `shouldBe` Left "Invalid address"

    it "rejects LinePrevious (must be substituted first)" $ do
      resolveLineRange 5 10 Map.empty [] LinePrevious `shouldBe` Left "No previous line range"

  describe "resolveLineAddr" $ do
    it "resolves single address" $ do
      resolveLineAddr 5 10 Map.empty [] False (LineSingle (Number 3)) `shouldBe` Right 3

    it "allows zero when allowZero is True" $ do
      resolveLineAddr 5 10 Map.empty [] True (LineSingle (Number 0)) `shouldBe` Right 0

    it "rejects zero when allowZero is False" $ do
      resolveLineAddr 5 10 Map.empty [] False (LineSingle (Number 0)) `shouldBe` Left "Invalid address"

    it "rejects range (expects single address)" $ do
      resolveLineAddr 5 10 Map.empty [] False (LineFree (Number 1) (Number 3)) `shouldBe` Left "Invalid address"

  describe "resolveDocAddr" $ do
    it "resolves Current to current doc" $ do
      resolveDocAddr 2 5 [] Map.empty Current `shouldBe` Right 2

    it "resolves LastLine to total docs" $ do
      resolveDocAddr 2 5 [] Map.empty LastLine `shouldBe` Right 5

    it "resolves Number to doc number" $ do
      resolveDocAddr 2 5 [] Map.empty (Number 3) `shouldBe` Right 3

    it "searches doc filenames with Next" $ do
      let filenames = ["file1.txt", "file2.txt", "test.txt"]
      resolveDocAddr 1 3 filenames Map.empty (Next "test") `shouldBe` Right 3

  describe "resolveDocRange" $ do
    it "resolves single doc address" $ do
      resolveDocRange 2 5 [] Map.empty (DocSingle (Number 3)) `shouldBe` Right (3, 3)

    it "resolves doc comma range" $ do
      resolveDocRange 2 5 [] Map.empty (DocFree (Number 1) (Number 3)) `shouldBe` Right (1, 3)

    it "resolves DocAll to full range" $ do
      resolveDocRange 2 5 [] Map.empty DocAll `shouldBe` Right (1, 5)

    it "resolves DocAll on empty to (0, 0)" $ do
      resolveDocRange 0 0 [] Map.empty DocAll `shouldBe` Right (0, 0)

    it "resolves DocManage like DocAll" $ do
      resolveDocRange 2 5 [] Map.empty DocManage `shouldBe` Right (1, 5)

    it "rejects DocParam" $ do
      resolveDocRange 2 5 [] Map.empty DocParam `shouldBe` Left "Invalid document range"

  describe "searchForward" $ do
    it "finds next matching line" $ do
      let lines' = ["alpha", "beta", "gamma", "delta"]
      searchForward "gamma" 1 4 lines' `shouldBe` Right 3

    it "wraps around to find match" $ do
      let lines' = ["alpha", "beta", "gamma", "delta"]
      searchForward "alpha" 3 4 lines' `shouldBe` Right 1

    it "finds match on current line when it's the only match" $ do
      let lines' = ["alpha", "beta", "gamma"]
      searchForward "alpha" 1 3 lines' `shouldBe` Right 1

    it "returns error on no match" $ do
      let lines' = ["alpha", "beta", "gamma"]
      searchForward "omega" 1 3 lines' `shouldBe` Left "No match"

    it "returns error on empty list" $ do
      searchForward "anything" 0 0 [] `shouldBe` Left "No match"

  describe "searchBackward" $ do
    it "finds previous matching line" $ do
      let lines' = ["alpha", "beta", "gamma", "delta"]
      searchBackward "beta" 4 4 lines' `shouldBe` Right 2

    it "wraps around to find match" $ do
      let lines' = ["alpha", "beta", "gamma", "delta"]
      searchBackward "delta" 2 4 lines' `shouldBe` Right 4

    it "returns error on no match" $ do
      let lines' = ["alpha", "beta", "gamma"]
      searchBackward "omega" 2 3 lines' `shouldBe` Left "No match"
