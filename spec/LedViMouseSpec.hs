module LedViMouseSpec (spec) where

import Test.Hspec

import LedVi.Mouse


spec :: Spec
spec = do
  describe "LedViMouse" $ do
    describe "MouseState" $ do
      describe "emptyMouseState" $ do
        it "has no click start" $ do
          msClickStart emptyMouseState `shouldBe` Nothing

    describe "makeRangeText" $ do
      it "creates single line range" $ do
        makeRangeText 5 5 `shouldBe` "5"

      it "creates ascending range" $ do
        makeRangeText 3 7 `shouldBe` "3,7"

      it "normalizes descending range" $ do
        makeRangeText 7 3 `shouldBe` "3,7"

    describe "splitRangeAndCommand" $ do
      it "splits range from command" $ do
        splitRangeAndCommand "1,5p" `shouldBe` ("1,5", "p")

      it "handles command only" $ do
        splitRangeAndCommand "p" `shouldBe` ("", "p")

      it "handles range only" $ do
        splitRangeAndCommand "1,$" `shouldBe` ("1,$", "")

      it "handles special range characters" $ do
        -- Note: regex patterns like /foo/ are not fully parsed as ranges
        -- Only simple range chars are recognized: digits, comma, semicolon, etc.
        splitRangeAndCommand "1,$d" `shouldBe` ("1,$", "d")
        splitRangeAndCommand ".+5p" `shouldBe` (".+5", "p")
