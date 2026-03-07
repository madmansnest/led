module LedNexusSpec where

import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import LedNexus
import LedDocument (emptyDocument, fromText, documentLines, lineCount)

spec :: Spec
spec = describe "LedNexus" $ do
  describe "emptyDocumentState" $ do
    it "has empty document" $
      docDocument emptyDocumentState `shouldBe` emptyDocument

    it "has current line 0" $
      docCurrentLine emptyDocumentState `shouldBe` 0

    it "has no marks" $
      docMarks emptyDocumentState `shouldBe` Map.empty

    it "is unchanged" $
      docChangeFlag emptyDocumentState `shouldBe` Unchanged

  describe "emptyDocumentList" $ do
    it "has empty document" $
      dlDocList emptyDocumentList `shouldBe` emptyDocument

    it "has empty documents seq" $
      dlDocuments emptyDocumentList `shouldBe` Seq.empty

    it "has current doc 0" $
      dlCurrentDoc emptyDocumentList `shouldBe` 0

    it "has document count 0" $
      documentCount emptyDocumentList `shouldBe` 0

  describe "singletonDocumentList" $ do
    let ds = emptyDocumentState { docCurrentLine = 5 }
        dl = singletonDocumentList "test.txt" ds

    it "has one document" $
      documentCount dl `shouldBe` 1

    it "has current doc 1" $
      dlCurrentDoc dl `shouldBe` 1

    it "stores the filename" $
      getFilenameAt 1 dl `shouldBe` Just "test.txt"

    it "stores the document state" $
      fmap docCurrentLine (getDocStateAt 1 dl) `shouldBe` Just 5

  describe "insertDocAfter" $ do
    let ds1 = emptyDocumentState
        ds2 = emptyDocumentState { docCurrentLine = 10 }
        dl0 = emptyDocumentList

    it "inserts into empty list" $ do
      let dl = insertDocAfter 0 "first.txt" ds1 dl0
      documentCount dl `shouldBe` 1
      getFilenameAt 1 dl `shouldBe` Just "first.txt"

    it "inserts at end" $ do
      let dl1 = insertDocAfter 0 "first.txt" ds1 dl0
          dl2 = insertDocAfter 1 "second.txt" ds2 dl1
      documentCount dl2 `shouldBe` 2
      getFilenameAt 1 dl2 `shouldBe` Just "first.txt"
      getFilenameAt 2 dl2 `shouldBe` Just "second.txt"

    it "inserts at beginning" $ do
      let dl1 = insertDocAfter 0 "first.txt" ds1 dl0
          dl2 = insertDocAfter 0 "new.txt" ds2 dl1
      documentCount dl2 `shouldBe` 2
      getFilenameAt 1 dl2 `shouldBe` Just "new.txt"
      getFilenameAt 2 dl2 `shouldBe` Just "first.txt"

    it "inserts in middle" $ do
      let dl1 = insertDocAfter 0 "first.txt" ds1 dl0
          dl2 = insertDocAfter 1 "second.txt" ds1 dl1
          dl3 = insertDocAfter 1 "middle.txt" ds2 dl2
      documentCount dl3 `shouldBe` 3
      getFilenameAt 1 dl3 `shouldBe` Just "first.txt"
      getFilenameAt 2 dl3 `shouldBe` Just "middle.txt"
      getFilenameAt 3 dl3 `shouldBe` Just "second.txt"

  describe "deleteDocRange" $ do
    let ds = emptyDocumentState
        dl = insertDocAfter 2 "third.txt" ds
           $ insertDocAfter 1 "second.txt" ds
           $ insertDocAfter 0 "first.txt" ds emptyDocumentList

    it "deletes single document" $ do
      let dl' = deleteDocRange 2 2 dl
      documentCount dl' `shouldBe` 2
      getFilenameAt 1 dl' `shouldBe` Just "first.txt"
      getFilenameAt 2 dl' `shouldBe` Just "third.txt"

    it "deletes first document" $ do
      let dl' = deleteDocRange 1 1 dl
      documentCount dl' `shouldBe` 2
      getFilenameAt 1 dl' `shouldBe` Just "second.txt"
      getFilenameAt 2 dl' `shouldBe` Just "third.txt"

    it "deletes last document" $ do
      let dl' = deleteDocRange 3 3 dl
      documentCount dl' `shouldBe` 2
      getFilenameAt 1 dl' `shouldBe` Just "first.txt"
      getFilenameAt 2 dl' `shouldBe` Just "second.txt"

    it "deletes range" $ do
      let dl' = deleteDocRange 1 2 dl
      documentCount dl' `shouldBe` 1
      getFilenameAt 1 dl' `shouldBe` Just "third.txt"

    it "deletes all documents" $ do
      let dl' = deleteDocRange 1 3 dl
      documentCount dl' `shouldBe` 0

    it "adjusts current doc when deleting before it" $ do
      let dl1 = setDlCurrentDoc 3 dl
          dl' = deleteDocRange 1 1 dl1
      dlCurrentDoc dl' `shouldBe` 2

    it "adjusts current doc when deleting it" $ do
      let dl1 = setDlCurrentDoc 2 dl
          dl' = deleteDocRange 2 2 dl1
      dlCurrentDoc dl' `shouldBe` 2  -- moves to next valid

  describe "getDocStateAt / setDocStateAt" $ do
    let ds1 = emptyDocumentState { docCurrentLine = 5 }
        ds2 = emptyDocumentState { docCurrentLine = 10 }
        dl = singletonDocumentList "test.txt" ds1

    it "gets document state" $
      fmap docCurrentLine (getDocStateAt 1 dl) `shouldBe` Just 5

    it "returns Nothing for invalid index" $ do
      getDocStateAt 0 dl `shouldBe` Nothing
      getDocStateAt 2 dl `shouldBe` Nothing

    it "sets document state" $ do
      let dl' = setDocStateAt 1 ds2 dl
      fmap docCurrentLine (getDocStateAt 1 dl') `shouldBe` Just 10

    it "ignores invalid index" $ do
      let dl' = setDocStateAt 0 ds2 dl
      fmap docCurrentLine (getDocStateAt 1 dl') `shouldBe` Just 5

  describe "getCurrentDocState" $ do
    let ds = emptyDocumentState { docCurrentLine = 7 }
        dl = setDlCurrentDoc 1 (singletonDocumentList "test.txt" ds)

    it "gets current document state" $
      fmap docCurrentLine (getCurrentDocState dl) `shouldBe` Just 7

    it "returns Nothing when no current doc" $
      getCurrentDocState emptyDocumentList `shouldBe` Nothing

  describe "getFilenameAt / setFilenameAt" $ do
    let ds = emptyDocumentState
        dl = singletonDocumentList "original.txt" ds

    it "gets filename" $
      getFilenameAt 1 dl `shouldBe` Just "original.txt"

    it "returns Nothing for invalid index" $ do
      getFilenameAt 0 dl `shouldBe` Nothing
      getFilenameAt 2 dl `shouldBe` Nothing

    it "sets filename" $ do
      let dl' = setFilenameAt 1 "new.txt" dl
      getFilenameAt 1 dl' `shouldBe` Just "new.txt"

  describe "getCurrentFilename / setCurrentFilename" $ do
    let ds = emptyDocumentState
        dl = setDlCurrentDoc 1 (singletonDocumentList "current.txt" ds)

    it "gets current filename" $
      getCurrentFilename dl `shouldBe` Just "current.txt"

    it "returns Nothing when no current doc" $
      getCurrentFilename emptyDocumentList `shouldBe` Nothing

    it "sets current filename" $ do
      let dl' = setCurrentFilename "updated.txt" dl
      getCurrentFilename dl' `shouldBe` Just "updated.txt"

  describe "modifyCurrentDocState" $ do
    let ds = emptyDocumentState { docCurrentLine = 1 }
        dl = setDlCurrentDoc 1 (singletonDocumentList "test.txt" ds)

    it "modifies current document state" $ do
      let dl' = modifyCurrentDocState (\s -> s { docCurrentLine = 99 }) dl
      fmap docCurrentLine (getCurrentDocState dl') `shouldBe` Just 99

    it "does nothing when no current doc" $ do
      let dl' = modifyCurrentDocState (\s -> s { docCurrentLine = 99 }) emptyDocumentList
      documentCount dl' `shouldBe` 0

  describe "moveDocument" $ do
    let ds = emptyDocumentState
        dl = setDlCurrentDoc 1
            (insertDocAfter 2 "third.txt" ds
            $ insertDocAfter 1 "second.txt" ds
            $ insertDocAfter 0 "first.txt" ds emptyDocumentList)

    it "moves document forward" $ do
      let dl' = moveDocument 1 1 2 dl
      getFilenameAt 1 dl' `shouldBe` Just "second.txt"
      getFilenameAt 2 dl' `shouldBe` Just "first.txt"
      getFilenameAt 3 dl' `shouldBe` Just "third.txt"

    it "moves document backward" $ do
      let dl' = moveDocument 3 3 0 dl
      getFilenameAt 1 dl' `shouldBe` Just "third.txt"
      getFilenameAt 2 dl' `shouldBe` Just "first.txt"
      getFilenameAt 3 dl' `shouldBe` Just "second.txt"

    it "moves range of documents" $ do
      let dl' = moveDocument 1 2 3 dl
      getFilenameAt 1 dl' `shouldBe` Just "third.txt"
      getFilenameAt 2 dl' `shouldBe` Just "first.txt"
      getFilenameAt 3 dl' `shouldBe` Just "second.txt"

    it "does not move to destination within range" $ do
      let dl' = moveDocument 1 3 2 dl
      -- Should be unchanged (invalid move)
      getFilenameAt 1 dl' `shouldBe` Just "first.txt"
      getFilenameAt 2 dl' `shouldBe` Just "second.txt"
      getFilenameAt 3 dl' `shouldBe` Just "third.txt"

  describe "hasUnsavedChanges / unsavedDocuments" $ do
    let ds1 = emptyDocumentState { docChangeFlag = Unchanged }
        ds2 = emptyDocumentState { docChangeFlag = Changed }
        ds3 = emptyDocumentState { docChangeFlag = ChangedAndWarned }

    it "returns False for all unchanged" $ do
      let dl = insertDocAfter 1 "second.txt" ds1
             $ insertDocAfter 0 "first.txt" ds1 emptyDocumentList
      hasUnsavedChanges dl `shouldBe` False
      unsavedDocuments dl `shouldBe` []

    it "returns True when one is changed" $ do
      let dl = insertDocAfter 1 "second.txt" ds2
             $ insertDocAfter 0 "first.txt" ds1 emptyDocumentList
      hasUnsavedChanges dl `shouldBe` True
      unsavedDocuments dl `shouldBe` [2]

    it "returns True when one is warned" $ do
      let dl = insertDocAfter 1 "second.txt" ds3
             $ insertDocAfter 0 "first.txt" ds1 emptyDocumentList
      hasUnsavedChanges dl `shouldBe` True
      unsavedDocuments dl `shouldBe` [2]

    it "lists all unsaved documents" $ do
      let dl = insertDocAfter 2 "third.txt" ds2
             $ insertDocAfter 1 "second.txt" ds1
             $ insertDocAfter 0 "first.txt" ds2 emptyDocumentList
      sort (unsavedDocuments dl) `shouldBe` [1, 3]
