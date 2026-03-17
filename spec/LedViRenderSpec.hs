module LedViRenderSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Vector as V

import LedVi.Types (DisplayZone(..), DisplayLine(..), LineStyle(..), emptyDisplayZone)
import LedVi.Render (wrapDisplayZone, gutterWidth)
-- Note: tests use modules from LedVi.* hierarchy


-- Helper to create a simple display line
mkDisplayLine :: Maybe Int -> Text -> DisplayLine
mkDisplayLine lineNum txt = DisplayLine
  { dlLineNum = lineNum
  , dlSourceLine = lineNum
  , dlDocIdx = 1
  , dlText = txt
  , dlStyle = StyleNormal
  , dlHighlights = []
  }


spec :: Spec
spec = do
  describe "LedViRender" $ do
    describe "gutterWidth" $ do
      it "is 8 columns" $ do
        gutterWidth `shouldBe` 8

    describe "wrapDisplayZone" $ do
      it "does not wrap short lines" $ do
        let width = 80  -- 72 content width after gutter
            dl = mkDisplayLine (Just 1) "Short line"
            dz = (emptyDisplayZone width 24) { dzLines = V.fromList [dl] }
            wrapped = wrapDisplayZone dz
        V.length (dzLines wrapped) `shouldBe` 1
        dlLineNum (V.head (dzLines wrapped)) `shouldBe` Just 1

      it "wraps long lines into multiple visual lines" $ do
        let width = 20  -- 12 content width after gutter
            longText = "This is a very long line that should wrap"
            dl = mkDisplayLine (Just 1) longText
            dz = (emptyDisplayZone width 24) { dzLines = V.fromList [dl] }
            wrapped = wrapDisplayZone dz
        -- Line should wrap into multiple visual lines
        V.length (dzLines wrapped) `shouldSatisfy` (> 1)

      it "preserves line number only on first visual line" $ do
        let width = 20  -- 12 content width after gutter
            longText = "This is a very long line that should wrap"
            dl = mkDisplayLine (Just 5) longText
            dz = (emptyDisplayZone width 24) { dzLines = V.fromList [dl] }
            wrapped = wrapDisplayZone dz
            wrappedLines = V.toList (dzLines wrapped)
        -- First line should have line number
        dlLineNum (fromMaybe (error "empty") (viaNonEmpty head wrappedLines)) `shouldBe` Just 5
        -- Continuation lines should have Nothing
        all (\l -> dlLineNum l == Nothing) (drop 1 wrappedLines) `shouldBe` True

      it "preserves source line number on all visual lines" $ do
        let width = 20  -- 12 content width after gutter
            longText = "This is a very long line that should wrap"
            dl = mkDisplayLine (Just 5) longText
            dz = (emptyDisplayZone width 24) { dzLines = V.fromList [dl] }
            wrapped = wrapDisplayZone dz
            wrappedLines = V.toList (dzLines wrapped)
        -- All lines should have the same source line number
        all (\l -> dlSourceLine l == Just 5) wrappedLines `shouldBe` True

      it "wraps at word boundaries when possible" $ do
        let width = 20  -- 12 content width after gutter
            -- "hello world" fits, but "hello world foo" doesn't
            text = "hello world foo bar"
            dl = mkDisplayLine (Just 1) text
            dz = (emptyDisplayZone width 24) { dzLines = V.fromList [dl] }
            wrapped = wrapDisplayZone dz
            firstLine = V.head (dzLines wrapped)
        -- First chunk should end with a space (word boundary)
        dlText firstLine `shouldSatisfy` (\t -> " " `T.isSuffixOf` t || T.length t <= 12)

      it "preserves style across wrapped lines" $ do
        let width = 20
            dl = (mkDisplayLine (Just 1) "Long line that wraps") { dlStyle = StyleSelected }
            dz = (emptyDisplayZone width 24) { dzLines = V.fromList [dl] }
            wrapped = wrapDisplayZone dz
            wrappedLines = V.toList (dzLines wrapped)
        -- All lines should have the same style
        all (\l -> dlStyle l == StyleSelected) wrappedLines `shouldBe` True

      it "adjusts scroll position for wrapped lines" $ do
        let width = 20
            -- Create 3 short lines
            dl1 = mkDisplayLine (Just 1) "Line 1"
            dl2 = mkDisplayLine (Just 2) "Very long line that will wrap to multiple visual lines"
            dl3 = mkDisplayLine (Just 3) "Line 3"
            dz = (emptyDisplayZone width 24)
              { dzLines = V.fromList [dl1, dl2, dl3]
              , dzScrollTop = 1  -- Scroll to logical line 2
              }
            wrapped = wrapDisplayZone dz
        -- Scroll position should be adjusted to visual index
        -- Line 1 takes 1 visual line, so scroll to line 2 becomes scroll to visual line 1
        dzScrollTop wrapped `shouldBe` 1

      it "handles empty display zone" $ do
        let dz = emptyDisplayZone 80 24
            wrapped = wrapDisplayZone dz
        V.null (dzLines wrapped) `shouldBe` True
