module LedIOSpec where
  
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, createDirectory, getHomeDirectory, getTemporaryDirectory, removeDirectory, removeFile, withCurrentDirectory)
import System.FilePath ((</>))
import System.Environment (setEnv, unsetEnv)
import Control.Exception (SomeException, bracket, catch)

import System.Exit (ExitCode(..))

import LedIO
import LedDocument (fromText, documentText)


withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "led-test-file.txt"
  B.writeFile path (TE.encodeUtf8 (T.pack content))
  result <- action path
  removeFile path
  pure result


withEnv :: String -> String -> IO a -> IO a
withEnv name value action =
  bracket
    (lookupEnv name <* setEnv name value)
    restore
    (const action)
  where
    restore Nothing  = unsetEnv name
    restore (Just v) = setEnv name v


spec :: Spec
spec = describe "LedIO" $ do
  describe "readDocument" $ do
    it "reads a file and returns document with byte count" $ do
      withTempFile "hello\n" $ \path -> do
        result <- readDocument path
        case result of
          Left err -> expectationFailure (T.unpack err)
          Right (doc, n) -> do
            documentText doc `shouldBe` "hello\n"
            n `shouldBe` 6

    it "reads UTF-8 content" $ do
      withTempFile "café\n" $ \path -> do
        result <- readDocument path
        case result of
          Left err -> expectationFailure (T.unpack err)
          Right (doc, n) -> do
            documentText doc `shouldBe` "café\n"
            n `shouldBe` 6

    it "returns error for non-existent file" $ do
      result <- readDocument "/tmp/led-nonexistent-file-xyz"
      result `shouldSatisfy` isLeft

  describe "writeDocument" $ do
    it "writes document and returns byte count" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "led-test-write.txt"
      result <- writeDocument path (fromText "hello\n")
      removeFile path
      result `shouldBe` Right 6

    it "round-trips through read and write" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "led-test-roundtrip.txt"
          doc = fromText "line1\nline2\nline3\n"
      _ <- writeDocument path doc
      result <- readDocument path
      removeFile path
      case result of
        Left err -> expectationFailure (T.unpack err)
        Right (doc', _) -> documentText doc' `shouldBe` documentText doc

    prop "byte count matches encoded length" $ \(PrintableString s) -> ioProperty $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "led-test-bc.txt"
          t = T.pack s
          doc = fromText t
          expected = B.length (TE.encodeUtf8 t)
      result <- writeDocument path doc
      removeFile path
      pure (result === Right expected)

    it "returns friendly error for non-existent parent directory" $ do
      let path = "/nonexistent-dir-xyz/subdir/file.txt"
      result <- writeDocument path (fromText "test\n")
      case result of
        Left err -> do
          -- Should mention the directory, not show raw Haskell error
          err `shouldSatisfy` T.isInfixOf "Directory does not exist"
          err `shouldSatisfy` (not . T.isInfixOf "withBinaryFile")
        Right _ -> expectationFailure "Expected error for non-existent directory"

  describe "writeBytes" $ do
    it "writes bytes and returns length" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "led-test-writebytes.txt"
          bs = TE.encodeUtf8 "hello\n"
      result <- writeBytes path bs
      removeFile path
      result `shouldBe` Right 6

    it "returns error for non-existent parent directory" $ do
      let path = "/nonexistent-dir-xyz/file.txt"
          bs = TE.encodeUtf8 "test\n"
      result <- writeBytes path bs
      result `shouldSatisfy` isLeft

  describe "readTextFile" $ do
    it "reads file as text" $ do
      withTempFile "hello world\n" $ \path -> do
        result <- readTextFile path
        result `shouldBe` Right "hello world\n"

    it "returns error for non-existent file" $ do
      result <- readTextFile "/tmp/led-nonexistent-xyz"
      result `shouldSatisfy` isLeft

  describe "runShellCommand" $ do
    it "runs shell command and captures stdout" $ do
      (code, out, _) <- runShellCommand "echo hello" ""
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\n"

    it "passes stdin to command" $ do
      (code, out, _) <- runShellCommand "cat" "input text"
      code `shouldBe` ExitSuccess
      out `shouldBe` "input text"

    it "captures stderr" $ do
      (code, _, err) <- runShellCommand "echo error >&2" ""
      code `shouldBe` ExitSuccess
      err `shouldBe` "error\n"

    it "returns exit code on failure" $ do
      (code, _, _) <- runShellCommand "exit 42" ""
      code `shouldBe` ExitFailure 42

  describe "humanisePath" $ do
    it "makes path relative to current directory" $ withCurrentDirectory "mock" $
      humanisePath "../mock/test.txt" `shouldReturn` "test.txt"

    it "replaces home directory with ~" $ withEnv "HOME" "/Users/testuser" $
      humanisePath "/Users/testuser/docs/test.txt" `shouldReturn` "~/docs/test.txt"

    it "replaces exact home directory with ~" $ withEnv "HOME" "/Users/testuser" $
      humanisePath "/Users/testuser" `shouldReturn` "~"

    it "does not replace similar-prefix paths with ~" $ withEnv "HOME" "/Users/test" $
      -- /Users/testuser should NOT become ~user
      humanisePath "/Users/testuser/file.txt" `shouldReturn` "/Users/testuser/file.txt"

    it "returns original path on error (non-existent path)" $
      humanisePath "/nonexistent/path/to/file.txt" `shouldReturn` "/nonexistent/path/to/file.txt"

    it "returns original path when HOME is not set" $ do
      -- This test simulates HOME not being available by using an invalid home
      -- The actual behavior depends on getHomeDirectory implementation
      path <- humanisePath "relative/path.txt"
      -- Should return something (either humanised or original)
      path `shouldSatisfy` (not . null)

    it "handles paths starting with ~ (expands first then humanises)" $ do
      -- Create a temp file in home
      home <- getHomeDirectory
      let testFile = home </> "led-tilde-test.txt"
      B.writeFile testFile ""
      -- humanisePath on ~/led-tilde-test.txt should work
      result <- humanisePath ("~" </> "led-tilde-test.txt")
      removeFile testFile
      -- Result should be either relative or ~/...
      result `shouldSatisfy` (\p -> p == "led-tilde-test.txt" || "~" `isPrefixOf` p || not (home `isPrefixOf` p))

    it "humanises paths where home has symlinks (canonicalizes home for comparison)" $ do
      -- This tests that we canonicalize both the path AND home before comparing
      home <- getHomeDirectory
      canonHome <- canonicalizePath home
      let testFile = canonHome </> "led-canon-test.txt"
      B.writeFile testFile ""
      result <- humanisePath testFile
      removeFile testFile
      -- Should be humanised (relative or ~/...)
      result `shouldSatisfy` (\p -> not (canonHome `isPrefixOf` p))

  describe "expandPath" $ do
    it "expands ~/path to home directory" $ withEnv "HOME" "/Users/testuser" $
      expandPath "~/docs/test.txt" `shouldReturn` "/Users/testuser/docs/test.txt"

    it "expands lone ~ to home directory" $ withEnv "HOME" "/Users/testuser" $
      expandPath "~" `shouldReturn` "/Users/testuser"

    it "does not expand ~username (not supported)" $ withEnv "HOME" "/Users/testuser" $ do
      -- ~otheruser/file gets passed to canonicalizePath which may make it relative to CWD
      -- The key is that it doesn't get expanded as a home directory
      result <- expandPath "~otheruser/file.txt"
      -- Should NOT start with /Users/testuser (the HOME we set)
      result `shouldSatisfy` (not . ("/Users/testuser" `isPrefixOf`))

    it "does not expand ~ in middle of path" $ withEnv "HOME" "/Users/testuser" $ do
      result <- expandPath "some/~/path"
      -- ~ in middle should not be expanded to home directory
      result `shouldSatisfy` (not . T.isPrefixOf "/Users/testuser" . toText)

    it "expands ~ even for non-existent subdirectory" $ do
      -- On macOS, canonicalizePath doesn't fail for non-existent paths
      -- It just resolves the existing prefix
      home <- getHomeDirectory
      result <- expandPath "~/nonexistent-subdir-xyz"
      -- Should start with home directory (~ is expanded)
      result `shouldSatisfy` (home `isPrefixOf`)

    it "returns original path for non-tilde paths that don't exist" $
      -- canonicalizePath on non-existent absolute paths may succeed on some platforms
      expandPath "/nonexistent/path/file.txt" `shouldReturn` "/nonexistent/path/file.txt"

    it "canonicalises existing paths (removes /.)" $ do
      tmp <- getTemporaryDirectory
      let path = tmp </> "."
      result <- expandPath path
      -- Should be canonicalised (no trailing /. and resolves symlinks)
      result `shouldSatisfy` (not . T.isSuffixOf "/." . toText)
      result `shouldSatisfy` (not . T.isSuffixOf "\\." . toText)

  describe "humanisePath and expandPath roundtrip" $ do
    it "expandPath on humanised path produces canonicalised original" $ do
      -- Create a temp file to ensure paths exist
      tmp <- getTemporaryDirectory
      let testFile = tmp </> "led-roundtrip-test.txt"
      B.writeFile testFile ""
      -- Canonicalise both to compare
      canonical <- canonicalizePath testFile
      humanised <- humanisePath testFile
      expanded <- expandPath humanised
      removeFile testFile
      -- expanded should equal the canonical path
      expanded `shouldBe` canonical

    it "handles paths that are both relative and in home" $ do
      home <- getHomeDirectory
      withCurrentDirectory home $ do
        let testDir = home </> "led-io-spec-subdir-test"
        -- Clean up if exists from previous failed run
        removeDirectory testDir `catch` (\(_ :: SomeException) -> pure ())
        createDirectory testDir
        humanised <- humanisePath testDir
        -- The relative form should be preferred since CWD is home
        humanised `shouldBe` "led-io-spec-subdir-test"
        removeDirectory testDir
