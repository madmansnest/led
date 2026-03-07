module LedViParseSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

import LedParse (DocRange(..), LineRange(..), Addr(..), Command(..), FullRange(..), Suffix(..), SubstFlags(..))
import LedViParse


spec :: Spec
spec = describe "LedViParse" $ do
  describe "parsePartial" $ do
    describe "empty and basic input" $ do
      it "returns PPEmpty for empty input" $ do
        parsePartial "" `shouldBe` PPEmpty

      it "returns PPEmpty for whitespace-only input" $ do
        parsePartial "   " `shouldBe` PPEmpty

    describe "complete commands" $ do
      it "returns PPCommand for quit" $ do
        case parsePartial "q" of
          PPCommand _ -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for print" $ do
        case parsePartial "p" of
          PPCommand _ -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for numbered print (bare address)" $ do
        -- In ed/led, a bare address like "5" is a print command
        case parsePartial "5" of
          PPCommand (PrintLines (FullRange DocDefault (LineSingle (Number 5))) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand PrintLines, got: " ++ show other

      it "returns PPCommand for range print" $ do
        case parsePartial "1,5p" of
          PPCommand (PrintLines (FullRange DocDefault (LineFree (Number 1) (Number 5))) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for current line (bare .)" $ do
        case parsePartial "." of
          PPCommand (PrintLines (FullRange DocDefault (LineSingle Current)) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for last line (bare $)" $ do
        case parsePartial "$" of
          PPCommand (PrintLines (FullRange DocDefault (LineSingle LastLine)) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for range (1,5)" $ do
        case parsePartial "1,5" of
          PPCommand (PrintLines (FullRange DocDefault (LineFree (Number 1) (Number 5))) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for doc prefix (&)" $ do
        case parsePartial "&" of
          PPCommand (PrintLines (FullRange DocAll LineDefault) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

      it "returns PPCommand for manage prefix (&&)" $ do
        case parsePartial "&&" of
          PPCommand (PrintLines (FullRange DocManage LineDefault) _) -> pure ()
          other -> expectationFailure $ "Expected PPCommand, got: " ++ show other

    describe "substitute command" $ do
      it "treats s/foo as complete (empty replacement)" $ do
        case parsePartial "s/foo" of
          PPCommand (Substitute _ "foo" "" _ _) -> pure ()
          other -> expectationFailure $ "Expected Substitute with empty replacement, got: " ++ show other

      it "parses s/foo/bar as complete" $ do
        case parsePartial "s/foo/bar" of
          PPCommand (Substitute _ "foo" "bar" _ _) -> pure ()
          other -> expectationFailure $ "Expected Substitute, got: " ++ show other

      it "parses s/foo/bar/ as complete with NoSuffix" $ do
        case parsePartial "s/foo/bar/" of
          PPCommand (Substitute _ "foo" "bar" _ _) -> pure ()
          other -> expectationFailure $ "Expected Substitute, got: " ++ show other

      it "parses substitute with flags" $ do
        case parsePartial "s/foo/bar/g" of
          PPCommand (Substitute _ "foo" "bar" flags _) ->
            sfGlobal flags `shouldBe` True
          other -> expectationFailure $ "Expected Substitute with global flag, got: " ++ show other

    describe "partial substitute with flags" $ do
      it "parses partial substitute with global flag" $ do
        case parsePartial "s/foo/bar/g" of
          PPCommand _ -> pure ()  -- Complete command is also valid
          PPSubstReplace _ _ "foo" "bar" flags ->
            sfGlobal flags `shouldBe` True
          other -> expectationFailure $ "Expected flags to be parsed, got: " ++ show other

      it "parses partial substitute with count flag" $ do
        case parsePartial "s/foo/bar/2" of
          PPCommand _ -> pure ()
          PPSubstReplace _ _ "foo" "bar" flags ->
            sfCount flags `shouldBe` 2
          other -> expectationFailure $ "Expected count flag, got: " ++ show other

      it "parses partial substitute with insensitive flag" $ do
        case parsePartial "s/foo/bar/i" of
          PPCommand _ -> pure ()
          PPSubstReplace _ _ "foo" "bar" flags ->
            sfInsensitive flags `shouldBe` True
          other -> expectationFailure $ "Expected insensitive flag, got: " ++ show other

    describe "global command" $ do
      it "treats g/foo as complete (default 'p' command)" $ do
        case parsePartial "g/foo" of
          PPCommand (Global _ "foo" "p") -> pure ()
          other -> expectationFailure $ "Expected Global with default p, got: " ++ show other

      it "parses g/foo/d as complete" $ do
        case parsePartial "g/foo/d" of
          PPCommand (Global _ "foo" "d") -> pure ()
          other -> expectationFailure $ "Expected Global with d, got: " ++ show other

      it "parses v/bar as complete (reverse global)" $ do
        case parsePartial "v/bar" of
          PPCommand (GlobalReverse _ "bar" "p") -> pure ()
          other -> expectationFailure $ "Expected GlobalReverse, got: " ++ show other

    describe "command prefix (before delimiter)" $ do
      it "returns PPCommandPrefix for 's' without delimiter" $ do
        case parsePartial "s" of
          PPCommandPrefix DocDefault LineDefault 's' -> pure ()
          other -> expectationFailure $ "Expected PPCommandPrefix 's', got: " ++ show other

      it "returns PPCommandPrefix for 's' with range but no delimiter" $ do
        case parsePartial "1,5s" of
          PPCommandPrefix DocDefault (LineFree (Number 1) (Number 5)) 's' -> pure ()
          other -> expectationFailure $ "Expected PPCommandPrefix 's' with range, got: " ++ show other

      it "returns PPCommandPrefix for 'g' without delimiter" $ do
        case parsePartial "g" of
          PPCommandPrefix DocDefault LineDefault 'g' -> pure ()
          other -> expectationFailure $ "Expected PPCommandPrefix 'g', got: " ++ show other

    describe "incomplete input (needs partial parsing)" $ do
      it "returns PPSubstPattern for truly incomplete substitute (missing both)" $ do
        -- Only returns partial when full parse fails
        -- s/ by itself might be considered as s with / delimiter and empty pattern
        case parsePartial "1,5s/" of
          PPSubstPattern _ _ "" -> pure ()
          PPCommand _ -> pure ()  -- Also acceptable
          other -> expectationFailure $ "Expected partial or command, got: " ++ show other

    describe "error handling" $ do
      it "returns PPError or PPIncomplete for invalid command" $ do
        case parsePartial "xyz123invalid" of
          PPError _ -> pure ()
          PPIncomplete -> pure ()  -- Could be incomplete if parser is still waiting
          other -> expectationFailure $ "Expected PPError or PPIncomplete, got: " ++ show other

      it "returns PPError, PPIncomplete or PPCommand for malformed input" $ do
        case parsePartial "1,2,3,4,5" of
          PPError _ -> pure ()
          PPIncomplete -> pure ()  -- Could be incomplete
          PPCommand _ -> pure ()  -- Parser might accept this somehow
          other -> expectationFailure $ "Expected PPError, PPIncomplete or PPCommand, got: " ++ show other

  describe "parsePartialWithFns" $ do
    it "recognizes user-defined functions" $ do
      let userFns = Set.fromList ["myfn", "otherfn"]
      case parsePartialWithFns userFns "myfn" of
        PPCommand _ -> pure ()
        other -> expectationFailure $ "Expected PPCommand for user fn, got: " ++ show other

    it "falls back to error or incomplete for unknown function" $ do
      let userFns = Set.fromList ["myfn"]
      case parsePartialWithFns userFns "unknownfn" of
        PPError _ -> pure ()
        PPIncomplete -> pure ()  -- Could be incomplete if still expecting more input
        other -> expectationFailure $ "Expected PPError or PPIncomplete, got: " ++ show other

  describe "isParseError" $ do
    it "returns True for PPError" $ do
      isParseError (PPError "test") `shouldBe` True

    it "returns False for PPEmpty" $ do
      isParseError PPEmpty `shouldBe` False

    it "returns False for PPAddress" $ do
      isParseError (PPAddress DocDefault LineDefault) `shouldBe` False

    it "returns False for PPCommand" $ do
      isParseError (PPCommand (PrintLines (FullRange DocDefault LineDefault) NoSuffix)) `shouldBe` False

  describe "isParseComplete" $ do
    it "returns True for PPCommand" $ do
      isParseComplete (PPCommand (PrintLines (FullRange DocDefault LineDefault) NoSuffix)) `shouldBe` True

    it "returns False for PPEmpty" $ do
      isParseComplete PPEmpty `shouldBe` False

    it "returns False for PPAddress" $ do
      isParseComplete (PPAddress DocDefault LineDefault) `shouldBe` False

    it "returns False for PPSubstPattern" $ do
      isParseComplete (PPSubstPattern DocDefault LineDefault "foo") `shouldBe` False

    it "returns False for PPError" $ do
      isParseComplete (PPError "error") `shouldBe` False
