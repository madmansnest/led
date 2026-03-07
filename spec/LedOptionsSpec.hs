module LedOptionsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Options.Applicative (execParserPure, defaultPrefs, getParseResult)

import LedOptions (Options(..), opts)

parse :: [String] -> Maybe Options
parse = getParseResult . execParserPure defaultPrefs opts

spec :: Spec
spec = describe "LedOptions" $ do
  describe "option parsing" $ do
    it "parses no arguments to defaults" $
      parse [] `shouldBe` Just (Options Nothing False [] [])

    it "parses -s flag" $
      parse ["-s"] `shouldBe` Just (Options Nothing True [] [])

    it "parses -p with prompt string" $
      parse ["-p", "*"] `shouldBe` Just (Options (Just "*") False [] [])

    it "parses a file operand" $
      parse ["foo.txt"] `shouldBe` Just (Options Nothing False [] ["foo.txt"])

    it "parses all options together" $
      parse ["-p", "* ", "-s", "file.txt"]
        `shouldBe` Just (Options (Just "* ") True [] ["file.txt"])

    it "parses -s before -p" $
      parse ["-s", "-p", ":", "buf"]
        `shouldBe` Just (Options (Just ":") True [] ["buf"])

    it "parses multiple files" $
      parse ["file1.txt", "file2.txt", "file3.txt"]
        `shouldBe` Just (Options Nothing False [] ["file1.txt", "file2.txt", "file3.txt"])

    it "rejects unknown flags" $
      parse ["-x"] `shouldBe` Nothing

    it "rejects -p without argument" $
      parse ["-p"] `shouldBe` Nothing

    it "parses -e with exec file" $
      parse ["-e", "script.led"] `shouldBe` Just (Options Nothing False ["script.led"] [])

    it "parses multiple -e flags" $
      parse ["-e", "a.led", "-e", "b.led"]
        `shouldBe` Just (Options Nothing False ["a.led", "b.led"] [])

    it "parses -e with other options and files" $
      parse ["-s", "-e", "script.led", "file.txt"]
        `shouldBe` Just (Options Nothing True ["script.led"] ["file.txt"])

    it "rejects -e without argument" $
      parse ["-e"] `shouldBe` Nothing

  describe "properties" $ do
    prop "file operand round-trips" $ \(PrintableString f) ->
      let f' = filter (`notElem` ['-', ' ', '\NUL']) f
      in not (null f') ==>
        fmap optFiles (parse [f']) === Just [f']

    prop "-s is always parsed correctly" $ \b ->
      let args = ["-s" | b]
      in fmap optSilent (parse args) === Just b
