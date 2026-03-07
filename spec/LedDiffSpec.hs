module LedDiffSpec where

import Test.Hspec

import LedDiff (ManageDiff(..), ManageWarnings(..), computeManageDiff, hasWarnings, formatManageWarnings)

spec :: Spec
spec = describe "LedDiff" $ do
  describe "computeManageDiff" $ do
    it "returns empty diff for identical lists" $ do
      let diff = computeManageDiff ["a", "b", "c"] ["a", "b", "c"]
      mdCreates diff `shouldBe` []
      mdDeletes diff `shouldBe` []
      mdRenames diff `shouldBe` []

    it "detects pure insertion at end" $ do
      let diff = computeManageDiff ["a", "b"] ["a", "b", "c"]
      mdCreates diff `shouldBe` ["c"]
      mdDeletes diff `shouldBe` []
      mdRenames diff `shouldBe` []

    it "detects pure insertion at beginning" $ do
      let diff = computeManageDiff ["b", "c"] ["a", "b", "c"]
      mdCreates diff `shouldBe` ["a"]
      mdDeletes diff `shouldBe` []
      mdRenames diff `shouldBe` []

    it "detects pure deletion" $ do
      let diff = computeManageDiff ["a", "b", "c"] ["a", "c"]
      mdDeletes diff `shouldBe` [(2, "b")]
      mdCreates diff `shouldBe` []
      mdRenames diff `shouldBe` []

    it "detects single rename" $ do
      let diff = computeManageDiff ["a", "b", "c"] ["a", "B", "c"]
      mdRenames diff `shouldBe` [(2, "b", "B")]
      mdCreates diff `shouldBe` []
      mdDeletes diff `shouldBe` []

    it "detects multiple renames" $ do
      let diff = computeManageDiff ["a", "b", "c"] ["a", "X", "Y"]
      mdRenames diff `shouldBe` [(2, "b", "X"), (3, "c", "Y")]
      mdCreates diff `shouldBe` []
      mdDeletes diff `shouldBe` []

    it "detects mixed rename and create" $ do
      let diff = computeManageDiff ["a", "b"] ["a", "B", "c"]
      mdRenames diff `shouldBe` [(2, "b", "B")]
      mdCreates diff `shouldBe` ["c"]
      mdDeletes diff `shouldBe` []

    it "detects mixed rename and delete" $ do
      let diff = computeManageDiff ["a", "b", "c"] ["a", "B"]
      mdRenames diff `shouldBe` [(2, "b", "B")]
      mdDeletes diff `shouldBe` [(3, "c")]
      mdCreates diff `shouldBe` []

    it "handles complete replacement" $ do
      let diff = computeManageDiff ["a"] ["b"]
      mdRenames diff `shouldBe` [(1, "a", "b")]
      mdCreates diff `shouldBe` []
      mdDeletes diff `shouldBe` []

    it "handles deletion of all" $ do
      let diff = computeManageDiff ["a", "b"] []
      mdDeletes diff `shouldBe` [(1, "a"), (2, "b")]
      mdCreates diff `shouldBe` []
      mdRenames diff `shouldBe` []

    it "handles insertion into empty" $ do
      let diff = computeManageDiff [] ["a", "b"]
      mdCreates diff `shouldBe` ["a", "b"]
      mdDeletes diff `shouldBe` []
      mdRenames diff `shouldBe` []

    it "sets correct deleteStart/deleteEnd for middle deletion" $ do
      let diff = computeManageDiff ["a", "b", "c", "d"] ["a", "d"]
      mdDeleteStart diff `shouldBe` 2
      mdDeleteEnd diff `shouldBe` 3

    it "sets correct insertPos for middle insertion" $ do
      let diff = computeManageDiff ["a", "c"] ["a", "b", "c"]
      mdInsertPos diff `shouldBe` 1

    it "unchanged line in middle of changed region is treated as rename to itself" $ do
      -- If old=["a","b","c"] new=["a","b","d"], the common prefix is ["a","b"]
      -- and the changed region is just the last element
      let diff = computeManageDiff ["a", "b", "c"] ["a", "b", "d"]
      mdRenames diff `shouldBe` [(3, "c", "d")]

  describe "hasWarnings" $ do
    it "returns True for file deletion" $ do
      let warnings = ManageWarnings { mwExistingFiles = [], mwFilesToDelete = ["file.txt"]
                                    , mwMassRenames = [], mwRenameTargetExists = [] }
      hasWarnings warnings `shouldBe` True

    it "returns False when no warnings" $ do
      let warnings = ManageWarnings { mwExistingFiles = [], mwFilesToDelete = []
                                    , mwMassRenames = [], mwRenameTargetExists = [] }
      hasWarnings warnings `shouldBe` False

    it "returns True for mass renames (2+)" $ do
      let warnings = ManageWarnings { mwExistingFiles = [], mwFilesToDelete = []
                                    , mwMassRenames = [("a", "b"), ("c", "d")]
                                    , mwRenameTargetExists = [] }
      hasWarnings warnings `shouldBe` True

    it "returns True for existing file creation" $ do
      let warnings = ManageWarnings { mwExistingFiles = ["existing.txt"], mwFilesToDelete = []
                                    , mwMassRenames = [], mwRenameTargetExists = [] }
      hasWarnings warnings `shouldBe` True

  describe "formatManageWarnings" $ do
    it "formats delete warning" $ do
      let warnings = ManageWarnings { mwExistingFiles = [], mwFilesToDelete = ["a.txt", "b.txt"]
                                    , mwMassRenames = [], mwRenameTargetExists = [] }
      formatManageWarnings warnings `shouldBe` "will delete: a.txt, b.txt"

    it "formats combined warning" $ do
      let warnings = ManageWarnings { mwExistingFiles = ["c.txt"], mwFilesToDelete = ["a.txt"]
                                    , mwMassRenames = [("x", "y"), ("p", "q")]
                                    , mwRenameTargetExists = [] }
      formatManageWarnings warnings `shouldBe`
        "will delete: a.txt; already exists: c.txt; will rename: x -> y, p -> q"

    it "formats empty warnings" $ do
      let warnings = ManageWarnings { mwExistingFiles = [], mwFilesToDelete = []
                                    , mwMassRenames = [], mwRenameTargetExists = [] }
      formatManageWarnings warnings `shouldBe` ""
