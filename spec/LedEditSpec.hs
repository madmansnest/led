module LedEditSpec where

import Test.Hspec
import qualified Data.Text as T

import LedEdit (substituteLine, substituteFirst, substituteAll, substituteNth, substituteLinesLoop)
import qualified LedRegularExpressions as RE

spec :: Spec
spec = describe "LedEdit" $ do
  describe "substituteLine" $ do
    it "substitutes first match by default" $ do
      let Right bre = RE.parseBRE "a"
      substituteLine bre "X" False 0 False "banana" `shouldBe` "bXnana"

    it "substitutes all matches with global flag" $ do
      let Right bre = RE.parseBRE "a"
      substituteLine bre "X" True 0 False "banana" `shouldBe` "bXnXnX"

    it "substitutes nth occurrence" $ do
      let Right bre = RE.parseBRE "a"
      substituteLine bre "X" False 2 False "banana" `shouldBe` "banXna"

  describe "substituteFirst" $ do
    it "replaces first match only" $ do
      let Right bre = RE.parseBRE "o"
      substituteFirst bre "0" False "foo bar foo" `shouldBe` "f0o bar foo"

    it "returns unchanged if no match" $ do
      let Right bre = RE.parseBRE "z"
      substituteFirst bre "Z" False "foo bar" `shouldBe` "foo bar"

  describe "substituteAll" $ do
    it "replaces all occurrences" $ do
      let Right bre = RE.parseBRE "o"
      substituteAll bre "0" False "foo bar foo" `shouldBe` "f00 bar f00"

  describe "substituteNth" $ do
    it "replaces 1st occurrence" $ do
      let Right bre = RE.parseBRE "a"
      substituteNth bre "X" 1 False "abracadabra" `shouldBe` "Xbracadabra"

    it "replaces 3rd occurrence" $ do
      let Right bre = RE.parseBRE "a"
      substituteNth bre "X" 3 False "abracadabra" `shouldBe` "abracXdabra"

    it "returns unchanged if nth doesn't exist" $ do
      let Right bre = RE.parseBRE "a"
      substituteNth bre "X" 10 False "abracadabra" `shouldBe` "abracadabra"

  describe "substituteLinesLoop" $ do
    it "processes multiple lines" $ do
      let Right bre = RE.parseBRE "x"
          lns = ["axb", "cxd", "efg"]
          (changed, result, lastLine) = substituteLinesLoop bre "Y" False 0 False 1 lns
      changed `shouldBe` True
      result `shouldBe` ["aYb", "cYd", "efg"]
      lastLine `shouldBe` 2

    it "handles newlines in replacement" $ do
      let Right bre = RE.parseBRE "x"
          lns = ["axb"]
          (changed, result, _) = substituteLinesLoop bre "1\n2" False 0 False 1 lns
      changed `shouldBe` True
      result `shouldBe` ["a1", "2b"]
