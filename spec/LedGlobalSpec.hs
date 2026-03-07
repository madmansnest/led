module LedGlobalSpec where

import Test.Hspec

import LedGlobal (GlobalHandlers(..))

-- Note: Most LedGlobal functions require the full Led monad stack and are
-- tested through integration tests in LedIntegrationSpec.hs. See:
--   - "g/RE/ sets previous RE for s//"
--   - "G/RE/ sets previous RE for subsequent s//"
--   - "@g/RE/cmd executes global command on param doc"
--   - "@v/RE/cmd executes inverse global on param doc"
--   - "&g (global on documents)" section
--   - "cross-doc global command" section

spec :: Spec
spec = describe "LedGlobal" $ do
  describe "GlobalHandlers" $ do
    it "is a record type for handler callbacks" $ do
      -- GlobalHandlers breaks circular dependency between LedGlobal and LedExec
      -- by allowing LedExec to pass handler functions as a record.
      -- This test verifies the record type exists and can be constructed.
      -- Actual functionality is tested in LedIntegrationSpec.
      let _dummyHandlers = GlobalHandlers
            { ghExecuteCommand  = const (pure True)
            , ghHandleDocList   = \_ _ -> pure True
            , ghHandleManage    = \_ _ -> pure True
            , ghHandleModified  = \_ _ -> pure True
            , ghHandleParam     = \_ _ -> pure True
            , ghHandleCrossDoc  = \_ _ _ -> pure True
            , ghExpandExpressions = pure
            }
      -- Record constructed successfully
      True `shouldBe` True
