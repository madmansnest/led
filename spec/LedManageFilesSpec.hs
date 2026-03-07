module LedManageFilesSpec where

import Test.Hspec
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import LedManageFiles (ManageDiff(..), checkManageWarnings, executeManageActions)
import LedDiff (ManageWarnings(..))

spec :: Spec
spec = describe "LedManageFiles" $ do
  describe "checkManageWarnings" $ do
    it "detects existing files for creates" $ withSystemTempDirectory "led-test" $ \tmpDir -> do
      let existingFile = tmpDir </> "existing.txt"
      writeFile existingFile ("" :: String)
      let diff = ManageDiff { mdCreates = [toText existingFile], mdDeletes = [], mdRenames = []
                            , mdDeleteStart = 1, mdDeleteEnd = 0, mdInsertPos = 0 }
      warnings <- checkManageWarnings pure diff
      mwExistingFiles warnings `shouldBe` [toText existingFile]

    it "no warning for non-existing create targets" $ withSystemTempDirectory "led-test" $ \tmpDir -> do
      let newFile = tmpDir </> "new.txt"
      let diff = ManageDiff { mdCreates = [toText newFile], mdDeletes = [], mdRenames = []
                            , mdDeleteStart = 1, mdDeleteEnd = 0, mdInsertPos = 0 }
      warnings <- checkManageWarnings pure diff
      mwExistingFiles warnings `shouldBe` []

    it "single rename produces no mass rename warning" $ do
      let diff = ManageDiff { mdCreates = [], mdDeletes = [], mdRenames = [(1, "a", "b")]
                            , mdDeleteStart = 1, mdDeleteEnd = 1, mdInsertPos = 0 }
      warnings <- checkManageWarnings pure diff
      mwMassRenames warnings `shouldBe` []

    it "two renames produce mass rename warning" $ do
      let diff = ManageDiff { mdCreates = [], mdDeletes = []
                            , mdRenames = [(1, "a", "b"), (2, "c", "d")]
                            , mdDeleteStart = 1, mdDeleteEnd = 2, mdInsertPos = 0 }
      warnings <- checkManageWarnings pure diff
      mwMassRenames warnings `shouldBe` [("a", "b"), ("c", "d")]

  describe "executeManageActions" $ do
    it "creates empty files" $ withSystemTempDirectory "led-test" $ \tmpDir -> do
      let newFile = tmpDir </> "new.txt"
      let diff = ManageDiff { mdCreates = [toText newFile], mdDeletes = [], mdRenames = []
                            , mdDeleteStart = 1, mdDeleteEnd = 0, mdInsertPos = 0 }
      errors <- executeManageActions pure diff
      errors `shouldBe` []
      doesFileExist newFile `shouldReturn` True

    it "deletes files" $ withSystemTempDirectory "led-test" $ \tmpDir -> do
      let file = tmpDir </> "to-delete.txt"
      writeFile file ("content" :: String)
      let diff = ManageDiff { mdCreates = [], mdDeletes = [(1, toText file)], mdRenames = []
                            , mdDeleteStart = 1, mdDeleteEnd = 1, mdInsertPos = 0 }
      errors <- executeManageActions pure diff
      errors `shouldBe` []
      doesFileExist file `shouldReturn` False

    it "renames files" $ withSystemTempDirectory "led-test" $ \tmpDir -> do
      let oldFile = tmpDir </> "old.txt"
          newFile = tmpDir </> "new.txt"
      writeFile oldFile ("content" :: String)
      let diff = ManageDiff { mdCreates = [], mdDeletes = []
                            , mdRenames = [(1, toText oldFile, toText newFile)]
                            , mdDeleteStart = 1, mdDeleteEnd = 1, mdInsertPos = 0 }
      errors <- executeManageActions pure diff
      errors `shouldBe` []
      doesFileExist oldFile `shouldReturn` False
      doesFileExist newFile `shouldReturn` True

    it "returns errors for non-existent deletes" $ withSystemTempDirectory "led-test" $ \tmpDir -> do
      let file = tmpDir </> "nonexistent.txt"
      let diff = ManageDiff { mdCreates = [], mdDeletes = [(1, toText file)], mdRenames = []
                            , mdDeleteStart = 1, mdDeleteEnd = 1, mdInsertPos = 0 }
      errors <- executeManageActions pure diff
      length errors `shouldBe` 1
