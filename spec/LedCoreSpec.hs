module LedCoreSpec where

import Test.Hspec

import qualified Data.Map.Strict as Map
import LedCore (LedState(..), initialState)
import LedUndo (newUndoManager)
import LedNexus (BufferChangeFlag(..), DocumentState(..), DocumentList(..), emptyDocumentState, documentCount, getCurrentDocState, singletonDocumentList, dlCurrentDoc)

spec :: Spec
spec = describe "LedCore" $ do
  describe "BufferChangeFlag" $ do
    it "has three distinct states" $
      [Unchanged, Changed, ChangedAndWarned] `shouldBe` [Unchanged, Changed, ChangedAndWarned]

    it "Unchanged /= Changed" $
      Unchanged `shouldNotBe` Changed

    it "Changed /= ChangedAndWarned" $
      Changed `shouldNotBe` ChangedAndWarned

  describe "initialState" $ do
    it "starts with empty document list" $ do
      undoMgr <- newUndoManager
      documentCount (ledDocumentList (initialState undoMgr False True Nothing [] [] ".")) `shouldBe` 0

    it "starts with no prompt active when no -p given" $ do
      undoMgr <- newUndoManager
      ledPromptActive (initialState undoMgr False True Nothing [] [] ".") `shouldBe` False

    it "starts with prompt active when -p given" $ do
      undoMgr <- newUndoManager
      ledPromptActive (initialState undoMgr False True (Just ":") [] [] ".") `shouldBe` True

    it "uses * as default prompt" $ do
      undoMgr <- newUndoManager
      ledPrompt (initialState undoMgr False True Nothing [] [] ".") `shouldBe` "*"

    it "uses provided prompt string" $ do
      undoMgr <- newUndoManager
      ledPrompt (initialState undoMgr False True (Just ":") [] [] ".") `shouldBe` ":"

    it "stores silent flag" $ do
      undoMgr <- newUndoManager
      ledSilent (initialState undoMgr True True Nothing [] [] ".") `shouldBe` True

    it "starts with help mode off" $ do
      undoMgr <- newUndoManager
      ledHelpMode (initialState undoMgr False True Nothing [] [] ".") `shouldBe` False

    it "starts with no last error" $ do
      undoMgr <- newUndoManager
      ledLastError (initialState undoMgr False True Nothing [] [] ".") `shouldBe` Nothing

    it "starts with no last shell command" $ do
      undoMgr <- newUndoManager
      ledLastShellCommand (initialState undoMgr False True Nothing [] [] ".") `shouldBe` Nothing

  describe "DocumentState" $ do
    it "emptyDocumentState has Unchanged flag" $
      docChangeFlag emptyDocumentState `shouldBe` Unchanged

    it "emptyDocumentState has current line 0" $
      docCurrentLine emptyDocumentState `shouldBe` 0

    it "emptyDocumentState has empty marks" $
      docMarks emptyDocumentState `shouldBe` Map.empty


  describe "buffer change flag transitions" $ do
    it "Changed state blocks q/e (requires warning first)" $
      let ds = emptyDocumentState { docChangeFlag = Changed }
      in docChangeFlag ds `shouldBe` Changed

    it "ChangedAndWarned allows q/e on second attempt" $
      let ds = emptyDocumentState { docChangeFlag = ChangedAndWarned }
      in docChangeFlag ds `shouldNotBe` Changed

    it "Unchanged allows q/e immediately" $
      docChangeFlag emptyDocumentState `shouldBe` Unchanged

    it "warning transitions Changed to ChangedAndWarned" $
      let ds = emptyDocumentState { docChangeFlag = Changed }
          warned = ds { docChangeFlag = ChangedAndWarned }
      in docChangeFlag warned `shouldBe` ChangedAndWarned

    it "successful write resets to Unchanged" $
      let ds = emptyDocumentState { docChangeFlag = Changed }
          afterWrite = ds { docChangeFlag = Unchanged }
      in docChangeFlag afterWrite `shouldBe` Unchanged

    it "successful e/E resets to Unchanged" $
      let ds = emptyDocumentState { docChangeFlag = ChangedAndWarned }
          afterEdit = ds { docChangeFlag = Unchanged }
      in docChangeFlag afterEdit `shouldBe` Unchanged

  describe "error tracking" $ do
    it "starts with no error occurred" $ do
      undoMgr <- newUndoManager
      ledErrorOccurred (initialState undoMgr False True Nothing [] [] ".") `shouldBe` False

    it "starts with no command error" $ do
      undoMgr <- newUndoManager
      ledCommandError (initialState undoMgr False True Nothing [] [] ".") `shouldBe` False

  describe "interactive flag" $ do
    it "stores interactive=True" $ do
      undoMgr <- newUndoManager
      ledIsInteractive (initialState undoMgr False True Nothing [] [] ".") `shouldBe` True

    it "stores interactive=False" $ do
      undoMgr <- newUndoManager
      ledIsInteractive (initialState undoMgr False False Nothing [] [] ".") `shouldBe` False

  describe "DocumentList" $ do
    it "singletonDocumentList creates list with one document" $ do
      let dl = singletonDocumentList "test.txt" emptyDocumentState
      documentCount dl `shouldBe` 1
      dlCurrentDoc dl `shouldBe` 1

    it "getCurrentDocState returns document state" $ do
      let ds = emptyDocumentState { docCurrentLine = 5 }
          dl = singletonDocumentList "test.txt" ds
      fmap docCurrentLine (getCurrentDocState dl) `shouldBe` Just 5
