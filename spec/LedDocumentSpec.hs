module LedDocumentSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Text as T

import LedDocument (emptyDocument, fromText, fromLines, documentText, documentLines, replaceLines, getLines, lineCount)

spec :: Spec
spec = describe "LedDocument" $ do
  describe "emptyDocument" $ do
    it "has empty text" $
      documentText emptyDocument `shouldBe` ""

    it "has zero lines" $
      lineCount emptyDocument `shouldBe` 0

  describe "fromText / toText" $ do
    it "round-trips text" $
      documentText (fromText "hello\nworld\n") `shouldBe` "hello\nworld\n"

    prop "round-trips arbitrary text" $ \(PrintableString s) ->
      let t = T.pack s
      in documentText (fromText t) === t

  describe "lineCount" $ do
    it "counts lines in multi-line text" $
      lineCount (fromText "a\nb\nc\n") `shouldBe` 3

    it "counts unterminated final line as a line" $
      lineCount (fromText "a\nb") `shouldBe` 2

    it "single line with newline" $
      lineCount (fromText "hello\n") `shouldBe` 1

  describe "fromLines / documentLines" $ do
    it "fromLines creates document from line list" $
      documentText (fromLines ["hello", "world"]) `shouldBe` "hello\nworld\n"

    it "documentLines splits into lines" $
      documentLines (fromText "a\nb\nc\n") `shouldBe` ["a", "b", "c"]

    it "documentLines handles unterminated final line" $
      documentLines (fromText "a\nb") `shouldBe` ["a", "b"]

    it "documentLines of empty document is empty" $
      documentLines emptyDocument `shouldBe` []

    it "fromLines empty list gives empty document" $
      documentText (fromLines []) `shouldBe` ""

    prop "fromLines . documentLines round-trips newline-terminated text" $ \(NonEmpty xs) ->
      let ls = map (T.pack . getPrintableString) xs
          doc = fromLines ls
      in documentLines doc === ls

  describe "replaceLines" $ do
    -- append after n = replaceLines (n + 1) 0 newLines
    it "inserts after line 0 (prepend)" $
      let doc = fromText "a\nb\n"
          doc' = replaceLines 1 0 ["x"] doc
      in documentLines doc' `shouldBe` ["x", "a", "b"]

    it "inserts after last line (append)" $
      let doc = fromText "a\nb\n"
          doc' = replaceLines 3 0 ["x"] doc
      in documentLines doc' `shouldBe` ["a", "b", "x"]

    it "inserts in the middle" $
      let doc = fromText "a\nb\nc\n"
          doc' = replaceLines 2 0 ["x", "y"] doc
      in documentLines doc' `shouldBe` ["a", "x", "y", "b", "c"]

    it "inserts into empty document" $
      let doc' = replaceLines 1 0 ["hello"] emptyDocument
      in documentLines doc' `shouldBe` ["hello"]

    it "inserts multiple lines" $
      let doc = fromText "a\n"
          doc' = replaceLines 2 0 ["b", "c", "d"] doc
      in documentLines doc' `shouldBe` ["a", "b", "c", "d"]

    -- delete s to e = replaceLines s (e - s + 1) []
    it "deletes a single line" $
      let doc = fromText "a\nb\nc\n"
          doc' = replaceLines 2 1 [] doc
      in documentLines doc' `shouldBe` ["a", "c"]

    it "deletes first line" $
      let doc = fromText "a\nb\nc\n"
          doc' = replaceLines 1 1 [] doc
      in documentLines doc' `shouldBe` ["b", "c"]

    it "deletes last line" $
      let doc = fromText "a\nb\nc\n"
          doc' = replaceLines 3 1 [] doc
      in documentLines doc' `shouldBe` ["a", "b"]

    it "deletes a range of lines" $
      let doc = fromText "a\nb\nc\nd\n"
          doc' = replaceLines 2 2 [] doc
      in documentLines doc' `shouldBe` ["a", "d"]

    it "deletes all lines" $
      let doc = fromText "a\nb\n"
          doc' = replaceLines 1 2 [] doc
      in documentLines doc' `shouldBe` []

  describe "getLines" $ do
    it "gets a single line" $
      let doc = fromText "a\nb\nc\n"
      in getLines 2 2 doc `shouldBe` ["b"]

    it "gets a range of lines" $
      let doc = fromText "a\nb\nc\nd\n"
      in getLines 2 3 doc `shouldBe` ["b", "c"]

    it "gets all lines" $
      let doc = fromText "a\nb\nc\n"
      in getLines 1 3 doc `shouldBe` ["a", "b", "c"]

    it "gets first line" $
      let doc = fromText "a\nb\nc\n"
      in getLines 1 1 doc `shouldBe` ["a"]

    it "gets last line" $
      let doc = fromText "a\nb\nc\n"
      in getLines 3 3 doc `shouldBe` ["c"]

  describe "replaceLines (join pattern)" $ do
    -- join s to e = replaceLines s (e - s + 1) [T.concat (getLines s e doc)]
    let joinLines' s e doc =
          let lns = getLines s e doc
              joined = T.concat lns
          in replaceLines s (e - s + 1) [joined] doc

    it "joins two adjacent lines" $
      let doc = fromText "a\nb\nc\n"
          doc' = joinLines' 1 2 doc
      in documentLines doc' `shouldBe` ["ab", "c"]

    it "joins three lines" $
      let doc = fromText "a\nb\nc\nd\n"
          doc' = joinLines' 1 3 doc
      in documentLines doc' `shouldBe` ["abc", "d"]

    it "joins all lines" $
      let doc = fromText "a\nb\nc\n"
          doc' = joinLines' 1 3 doc
      in documentLines doc' `shouldBe` ["abc"]

    it "joins a single line (no-op)" $
      let doc = fromText "a\nb\nc\n"
          doc' = joinLines' 2 2 doc
      in documentLines doc' `shouldBe` ["a", "b", "c"]

    it "joins middle lines" $
      let doc = fromText "a\nb\nc\nd\ne\n"
          doc' = joinLines' 2 4 doc
      in documentLines doc' `shouldBe` ["a", "bcd", "e"]

    it "preserves line count correctly" $
      let doc = fromText "a\nb\nc\nd\n"
          doc' = joinLines' 2 3 doc
      in lineCount doc' `shouldBe` 3
