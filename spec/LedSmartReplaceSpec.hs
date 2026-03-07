module LedSmartReplaceSpec where

import Test.Hspec

import LedSmartReplace (isSmartReplaceEligible, smartReplace, isAllLower, isAllUpper)

spec :: Spec
spec = describe "LedSmartReplace" $ do
  describe "isAllLower" $ do
    it "returns True for all lowercase" $ do
      isAllLower "hello" `shouldBe` True

    it "returns True for lowercase with non-letters" $ do
      isAllLower "hello world 123!" `shouldBe` True

    it "returns False for mixed case" $ do
      isAllLower "Hello" `shouldBe` False

    it "returns True for empty string" $ do
      isAllLower "" `shouldBe` True

    it "returns True for numbers only" $ do
      isAllLower "123" `shouldBe` True

    it "returns True for Cyrillic lowercase" $ do
      isAllLower "техническое обслуживани" `shouldBe` True

    it "returns False for Cyrillic mixed case" $ do
      isAllLower "Техническое обслуживани" `shouldBe` False

    it "returns True for Cyrillic lowercase single word" $ do
      isAllLower "техобслуживани" `shouldBe` True

  describe "isAllUpper" $ do
    it "returns True for all uppercase" $ do
      isAllUpper "HELLO" `shouldBe` True

    it "returns True for uppercase with non-letters" $ do
      isAllUpper "HELLO WORLD 123!" `shouldBe` True

    it "returns False for mixed case" $ do
      isAllUpper "Hello" `shouldBe` False

    it "returns True for empty string" $ do
      isAllUpper "" `shouldBe` True

    it "returns True for Cyrillic uppercase" $ do
      isAllUpper "ТЕХНИЧЕСКОЕ ОБСЛУЖИВАНИЕ" `shouldBe` True

    it "returns False for Cyrillic mixed case" $ do
      isAllUpper "Техническое обслуживание" `shouldBe` False

  describe "isSmartReplaceEligible" $ do
    it "returns True when search and replacement are lowercase without i flag" $ do
      isSmartReplaceEligible "cat" "dog" False `shouldBe` True

    it "returns False when search has uppercase" $ do
      isSmartReplaceEligible "Cat" "dog" False `shouldBe` False

    it "returns False when replacement has uppercase" $ do
      isSmartReplaceEligible "cat" "Dog" False `shouldBe` False

    it "returns False when i flag is set" $ do
      isSmartReplaceEligible "cat" "dog" True `shouldBe` False

  describe "smartReplace" $ do
    it "preserves all-caps" $ do
      smartReplace "CAT" "dog" `shouldBe` "DOG"

    it "preserves all-caps with non-letters" $ do
      smartReplace "MY CAT" "his dog" `shouldBe` "HIS DOG"

    it "preserves title case (first upper)" $ do
      smartReplace "Cat" "dog" `shouldBe` "Dog"

    it "preserves mixed case char-by-char" $ do
      smartReplace "CaT" "dog" `shouldBe` "DoG"

    it "handles longer replacement (all caps)" $ do
      smartReplace "CAT" "mouse" `shouldBe` "MOUSE"

    it "handles longer replacement (mixed case)" $ do
      smartReplace "Ab" "xyz" `shouldBe` "Xyz"

    it "handles shorter replacement (all caps)" $ do
      smartReplace "CAT" "ox" `shouldBe` "OX"

    it "handles shorter replacement (mixed case)" $ do
      smartReplace "CaT" "ab" `shouldBe` "Ab"

    it "preserves lowercase" $ do
      smartReplace "cat" "dog" `shouldBe` "dog"

    it "handles non-letter characters in match" $ do
      smartReplace "MY CAT" "his ferret" `shouldBe` "HIS FERRET"

    it "handles unicode all-caps" $ do
      smartReplace "ДА" "нет" `shouldBe` "НЕТ"

    it "handles Cyrillic title case" $ do
      smartReplace "Техническое" "техобслуживани" `shouldBe` "Техобслуживани"

    it "handles Cyrillic phrase with title case" $ do
      smartReplace "Техническое обслуживани" "техобслуживани" `shouldBe` "Техобслуживани"

    it "handles Cyrillic lowercase" $ do
      smartReplace "техническое" "техобслуживани" `shouldBe` "техобслуживани"

  describe "isSmartReplaceEligible with Unicode" $ do
    it "returns True for Cyrillic all lowercase" $ do
      isSmartReplaceEligible "техническое обслуживани" "техобслуживани" False `shouldBe` True

    it "returns False for Cyrillic mixed case search" $ do
      isSmartReplaceEligible "Техническое обслуживани" "техобслуживани" False `shouldBe` False
