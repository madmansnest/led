module LedReadWriteSpec where

import Test.Hspec

import LedReadWrite

spec :: Spec
spec = describe "LedReadWrite" $ do
  describe "substitutePercent" $ do
    it "substitutes % with filename" $ do
      substitutePercent "cat %" (Just "file.txt") `shouldBe` ("cat file.txt", True)

    it "leaves % unchanged when no filename" $ do
      substitutePercent "cat %" Nothing `shouldBe` ("cat %", False)

    it "does not substitute escaped \\%" $ do
      substitutePercent "echo \\%" (Just "file.txt") `shouldBe` ("echo %", False)

    it "handles multiple % substitutions" $ do
      substitutePercent "diff % %.bak" (Just "f") `shouldBe` ("diff f f.bak", True)

    it "returns expansion flag" $ do
      snd (substitutePercent "echo hi" (Just "f")) `shouldBe` False
      snd (substitutePercent "echo %" (Just "f")) `shouldBe` True
