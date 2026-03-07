module LedPrintSpec where

import Test.Hspec

import LedPrint (formatLine, listFormat)
import LedParse (Suffix(..))

spec :: Spec
spec = describe "LedPrint" $ do
  describe "formatLine" $ do
    it "returns line unchanged for NoSuffix" $ do
      formatLine NoSuffix Nothing 5 "hello" `shouldBe` "hello"

    it "returns line unchanged for PrintSuffix" $ do
      formatLine PrintSuffix Nothing 5 "hello" `shouldBe` "hello"

    it "prepends line number for NumberSuffix" $ do
      formatLine NumberSuffix Nothing 5 "hello" `shouldBe` "5\thello"
      formatLine NumberSuffix Nothing 42 "test" `shouldBe` "42\ttest"

    it "prepends docnum:linenum for NumberSuffix in multi-doc mode" $ do
      formatLine NumberSuffix (Just 2) 5 "hello" `shouldBe` "2:5\thello"
      formatLine NumberSuffix (Just 1) 42 "test" `shouldBe` "1:42\ttest"

    it "formats in list mode for ListSuffix" $ do
      formatLine ListSuffix Nothing 1 "hello" `shouldBe` "hello$"

  describe "listFormat" $ do
    it "appends $ to mark end of line" $ do
      listFormat "hello" `shouldBe` "hello$"

    it "escapes backslash" $ do
      listFormat "a\\b" `shouldBe` "a\\\\b$"

    it "escapes dollar sign" $ do
      listFormat "a$b" `shouldBe` "a\\$b$"

    it "escapes tab" $ do
      listFormat "a\tb" `shouldBe` "a\\tb$"

    it "escapes control characters as octal" $ do
      listFormat "a\x01\&b" `shouldBe` "a\\001b$"
      listFormat "a\x1f\&b" `shouldBe` "a\\037b$"

    it "escapes DEL character" $ do
      listFormat "a\DELb" `shouldBe` "a\\177b$"
