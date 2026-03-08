module LedIntegrationSpec where

import Test.Hspec

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Process (readCreateProcessWithExitCode, proc)
import System.Exit (ExitCode(..))
import System.IO (hPutStr, hClose)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)

-- | Run led via cabal run.
runLed :: [String] -> String -> IO (ExitCode, String, String)
runLed args input = do
  let cp = proc "cabal" (["run", "-v0", "led", "--"] ++ args)
  readCreateProcessWithExitCode cp input

-- | Run led with a temp file containing given content.
withTempInput :: String -> ([String] -> IO a) -> IO a
withTempInput content action = withSystemTempFile "led-test.txt" $ \path h -> do
  hPutStr h content
  hFlush h
  hClose h
  action [path]

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = let (l, rest) = break (== c) s
              in l : case rest of { [] -> []; (_:r) -> splitOn c r }

spec :: Spec
spec = describe "Integration" $ do

  describe "exit codes" $ do
    it "exits 0 on successful quit" $ withTempInput "hello\n" $ \args -> do
      (code, _, _) <- runLed args "q\n"
      code `shouldBe` ExitSuccess

    it "exits 1 on error in scripted mode" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed args "99p\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "exits 0 when no errors occur" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed args "1p\nq\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("hello" `isInfixOf`)

    it "prints 'Is a directory' when given a directory" $ do
      withSystemTempDirectory "led-dir-test" $ \tmpDir -> do
        (_, out, _) <- runLed [tmpDir] "Q\n"
        out `shouldSatisfy` ("Is a directory" `isInfixOf`)

  describe "scripted mode (non-interactive stdin)" $ do
    it "stops on first error" $ withTempInput "line1\nline2\n" $ \args -> do
      (code, out, _) <- runLed args "99p\n1p\nq\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` (not . ("line1" `isInfixOf`))

    it "executes multiple commands without error" $ withTempInput "aaa\nbbb\n" $ \args -> do
      (code, out, _) <- runLed args "1p\n2p\nq\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("aaa" `isInfixOf`)
      out `shouldSatisfy` ("bbb" `isInfixOf`)

    it "silent mode suppresses byte count" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1p\nq\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\n"

  describe "stdout output" $ do
    it "prints ? on error to stdout" $ withTempInput "hello\n" $ \args -> do
      (_, out, _) <- runLed args "99p\n"
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "prints byte count to stdout" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed args "q\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("6" `isInfixOf`)

    it "! command prints output to stdout" $ withTempInput "" $ \args -> do
      (_, out, _) <- runLed args "!echo test123\nq\n"
      out `shouldSatisfy` ("test123" `isInfixOf`)

  describe "c (change) command" $ do
    it "replaces addressed lines with input text" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "2c\nXXX\nYYY\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aaa\nXXX\nYYY\nccc\n"

    it "c with range replaces multiple lines" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1,2c\nZZZ\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "ZZZ\nccc\n"

  describe "! (shell command)" $ do
    it "!cmd without range prints command output" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "!echo shelloutput\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("shelloutput" `isInfixOf`)

    it "!cmd without range does not modify buffer" $ withTempInput "original\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "!echo ignored\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("original" `isInfixOf`)

    it "range!cmd filters lines through shell" $ withTempInput "cherry\napple\nbanana\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) ",!sort\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "apple\nbanana\ncherry\n"

    it "single line !cmd filters that line" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1!tr a-z A-Z\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "HELLO WORLD\n"

    it "range !cmd replaces only specified lines" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1,2!tr a-z A-Z\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "AAA\nBBB\nccc\n"

    it ".!cmd filters current line" $ withTempInput "one\ntwo\nthree\n" $ \args -> do
      -- 2 goes to line 2 (and prints it), .! filters current line
      (code, out, _) <- runLed ("-s" : args) "2\n.!tr a-z A-Z\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Output: "two" from bare 2, then "one\nTWO\nthree" from ,p
      out `shouldBe` "two\none\nTWO\nthree\n"

    it "shell filter can change line count" $ withTempInput "a\nb\nc\n" $ \args -> do
      -- wc -l outputs count, replacing 3 lines with 1
      (code, out, _) <- runLed ("-s" : args) ",!wc -l\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- wc -l on 3 lines (with trailing newline handled by filterLines)
      out `shouldSatisfy` ("3" `isInfixOf`)

    it "stderr from shell filter is printed to stdout" $ withTempInput "test\n" $ \args -> do
      -- stderr from shell goes to led's stdout via outputStrLn'
      (code, out, _) <- runLed ("-s" : args) "1!sh -c 'echo error >&2; echo stdout'\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("error" `isInfixOf`)

  describe "inline text for a/i/c commands" $ do
    it "'a text' appends inline text" $ withTempInput "line1\nline2\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1a new line\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "line1\nnew line\nline2\n"

    it "'i text' inserts inline text" $ withTempInput "line1\nline2\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "2i inserted\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "line1\ninserted\nline2\n"

    it "'c text' changes to inline text" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "2c replacement\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aaa\nreplacement\nccc\n"

    it "'ap text' appends with print suffix" $ withTempInput "line1\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1ap appended\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should print the appended line (from suffix) then all lines
      out `shouldBe` "appended\nline1\nappended\n"

    it "'a * list item' appends markdown list item" $ withTempInput "# Header\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1a * list item\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "# Header\n* list item\n"

    it "'0a first line' appends at line 0 (prepend)" $ withTempInput "existing\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "0a first line\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "first line\nexisting\n"

    it "'0i first line' inserts at line 0 (prepend)" $ withTempInput "existing\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "0i first line\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "first line\nexisting\n"

    it "'0i' multiline inserts at beginning" $ withTempInput "existing\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "0i\nfirst\nsecond\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "first\nsecond\nexisting\n"

    it "'$a last line' appends after last line" $ withTempInput "first\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "$a last line\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "first\nlast line\n"

    it "inline text with special characters" $ withTempInput "test\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1a hello & goodbye | test\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "test\nhello & goodbye | test\n"

    it "c with empty input deletes lines" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "2c\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aaa\nccc\n"

  describe "bare newline" $ do
    it "bare newline advances and prints next line" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1p\n\n\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aaa\nbbb\nccc\n"

    it "bare newline past last line is an error" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1p\n\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

  describe "multiline a/c/i commands" $ do
    it "a followed by lines and dot appends multiple lines" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "a\nhello\nworld\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\nworld\n"

    it "i followed by lines and dot inserts multiple lines" $ withTempInput "existing\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1i\nfirst\nsecond\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "first\nsecond\nexisting\n"

    it "c followed by lines and dot changes to multiple lines" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "2c\nreplacement1\nreplacement2\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aaa\nreplacement1\nreplacement2\nccc\n"

    it "a in empty buffer creates content" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "a\nline1\nline2\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "line1\nline2\n"

    it "inline 'a text' in empty buffer appends immediately" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "a we are all\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "we are all\n"

    it "inline 'a text' then dot prints current line" $ withTempInput "" $ \args -> do
      -- User types: a we are all, then . (print current), then ,n (print with numbers)
      (code, out, _) <- runLed ("-s" : args) "a we are all\n.\n,n\nQ\n"
      code `shouldBe` ExitSuccess
      -- . prints current line "we are all", ,n prints "1\twe are all"
      out `shouldBe` "we are all\n1\twe are all\n"

    it "multiline c with range replaces multiple lines" $ withTempInput "one\ntwo\nthree\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1,2c\nreplaced\n.\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "replaced\nthree\n"

  describe "w !command" $ do
    it "pipes buffer to shell command" $ withTempInput "hello\nworld\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "w !wc -l\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("2" `isInfixOf`)

    it "w !command does not reset change flag" $ withTempInput "hello\n" $ \args -> do
      -- After a change + w !cmd, q should still warn
      (code, out, _) <- runLed ("-s" : args) "1c\nfoo\n.\nw !cat\nq\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

  describe "e !command" $ do
    it "replaces buffer with command output" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "e !echo hello\n1p\nq\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\n"

  describe "s command features" $ do
    it "missing closing delimiter implies p suffix" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "s/aaa/bbb\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should print the substituted line (implicit p)
      out `shouldSatisfy` ("bbb" `isInfixOf`)

    it "% as sole replacement reuses last replacement" $ withTempInput "aaa\nbbb\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1s/aaa/zzz/\n2s/bbb/%/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("zzz" `isInfixOf`)
      -- line 2 should also be "zzz" (reused replacement)
      let outLines = filter (== "zzz") (splitOn '\n' out)
      length outLines `shouldBe` 2

    it "backslash-newline in replacement splits line" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "s/o w/o\\\nw/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\nworld\n"

    it "backslash-newline continuation in scripted mode" $ withTempInput "one two three\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "s/two/TWO\\\ninserted/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "one TWO\ninserted three\n"

    it "double backslash at end does not trigger continuation" $ withTempInput "hello\n" $ \args -> do
      -- \\\\ in Haskell string = \\ sent to led = literal backslash in replacement
      (code, out, _) <- runLed ("-s" : args) "s/hello/bye\\\\\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("bye\\" `isInfixOf`)

    it "backslash-newline works with address range" $ withTempInput "aa bb\ncc dd\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1,2s/ /\\\n/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aa\nbb\ncc\ndd\n"

    it "\\n in replacement also splits line" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "s/ /\\n/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\nworld\n"

    it "g/RE/ sets previous RE for s//" $ withTempInput "foo bar\nfoo baz\n" $ \args -> do
      -- g/foo/ should set last RE so s//xxx/ substitutes foo with xxx
      (code, out, _) <- runLed ("-s" : args) "g/foo/s//xxx/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "xxx bar\nxxx baz\n"

    it "g/pattern/s/^/prefix/ only affects matching lines" $ withTempInput "apple\nbanana\napricot\ncherry\n" $ \args -> do
      -- g/ap/s/^/FRUIT: / should only prefix lines containing "ap", not all lines
      -- This tests that batch substitute optimization respects the global pattern
      (code, out, _) <- runLed ("-s" : args) "g/ap/s/^/FRUIT: /\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Only "apple" and "apricot" should be prefixed
      out `shouldBe` "FRUIT: apple\nbanana\nFRUIT: apricot\ncherry\n"

    it "G/RE/ sets previous RE for subsequent s//" $ withTempInput "foo one\nbar two\nfoo three\n" $ \args -> do
      -- G/foo/ interactively visits lines with "foo", setting last RE to "foo"
      -- Then we can use s// on those lines
      -- We provide "s//XXX/" as the command for each matching line
      (code, out, _) <- runLed ("-s" : args) "G/foo/\ns//XXX/\ns//XXX/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("XXX one" `isInfixOf`)
      out `shouldSatisfy` ("XXX three" `isInfixOf`)

  describe "r !command suffix" $ do
    it "prints ! after r !command unless -s" $ withTempInput "" $ \args -> do
      (_, out, _) <- runLed args "r !echo hello\nQ\n"
      out `shouldSatisfy` ("!" `isInfixOf`)

    it "suppresses ! with -s flag" $ withTempInput "" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "r !echo hello\nQ\n"
      out `shouldSatisfy` (not . ("!" `isInfixOf`))

  describe "l command format" $ do
    it "shows tab as \\t" $ withTempInput "a\tb\n" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "1l\nQ\n"
      out `shouldSatisfy` ("\\t" `isInfixOf`)

    it "marks end of line with $" $ withTempInput "hello\n" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "1l\nQ\n"
      out `shouldSatisfy` ("hello$" `isInfixOf`)

    it "escapes $ in text" $ withTempInput "a$b\n" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "1l\nQ\n"
      out `shouldSatisfy` ("a\\$b$" `isInfixOf`)

    it "escapes backslash" $ withTempInput "a\\b\n" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "1l\nQ\n"
      out `shouldSatisfy` ("a\\\\b$" `isInfixOf`)

  describe "w with range" $ do
    it "writes only addressed range" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      withSystemTempFile "led-out.txt" $ \outPath outH -> do
        hClose outH
        (code, _, _) <- runLed ("-s" : args) ("2w " ++ outPath ++ "\nQ\n")
        code `shouldBe` ExitSuccess
        content <- readFileBS outPath
        content `shouldBe` "bbb\n"

    it "partial write does not reset change flag" $ withTempInput "aaa\nbbb\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1c\nxxx\n.\n1w /dev/null\nq\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

  describe "fn (user functions)" $ do
    it "defines and calls a simple function" $ withTempInput "## My Heading\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn hda {\ns/^/#/\n}\nhda\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "### My Heading\n"

    it "substitute with ^ regex works outside function" $ withTempInput "hello\n" $ \args -> do
      -- Verify ^ regex works at all
      (code, out, _) <- runLed ("-s" : args) "s/^/X/\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Xhello\n"

    it "substitute with [ in replacement" $ withTempInput "hello\n" $ \args -> do
      -- Test that [ works in replacement
      (code, out, err) <- runLed ("-s" : args) "H\ns/^/[/\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "[hello\n", "")

    it "substitute with $ anchor" $ withTempInput "hello\n" $ \args -> do
      -- Test that $ anchor works
      (code, out, err) <- runLed ("-s" : args) "H\ns/$/]/\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "hello]\n", "")

    it "two substitutes ^ and $ outside function" $ withTempInput "hello\n" $ \args -> do
      -- Test both substitutes in sequence
      (code, out, err) <- runLed ("-s" : args) "H\ns/^/[/\ns/$/]/\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "[hello]\n", "")

    it "two substitutes ^ and $ inside function" $ withTempInput "hello\n" $ \args -> do
      -- Same commands but inside function
      (code, out, err) <- runLed ("-s" : args) "H\nfn brackwrap {\ns/^/[/\ns/$/]/\n}\nbrackwrap\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "[hello]\n", "")

    it "function with simple substitute (no regex chars)" $ withTempInput "hello\n" $ \args -> do
      -- Even simpler test: substitute without special regex chars
      (code, out, err) <- runLed ("-s" : args) "H\nfn fix {\ns/hello/bye/\n}\nfix\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "bye\n", "")

    it "function with two substitute commands" $ withTempInput "hello\n" $ \args -> do
      -- Two simple substitutes
      (code, out, err) <- runLed ("-s" : args) "H\nfn fixtwo {\ns/h/H/\ns/o/O/\n}\nfixtwo\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "HellO\n", "")

    it "function with single substitute command" $ withTempInput "hello\n" $ \args -> do
      -- Simpler test: just one substitute in function
      (code, out, err) <- runLed ("-s" : args) "H\nfn addX {\ns/^/X/\n}\naddX\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "Xhello\n", "")

    it "function with multiple commands (debug)" $ withTempInput "hello\n" $ \args -> do
      -- Same as above but with H to see errors
      (code, out, err) <- runLed ("-s" : args) "H\nfn wrap {\ns/^/[/\ns/$/]/\n}\nwrap\n.p\nQ\n"
      (code, out, err) `shouldBe` (ExitSuccess, "[hello]\n", "")

    it "function with multiple commands" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn wrap {\ns/^/[/\ns/$/]/\n}\nwrap\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "[hello]\n"

    it "function with parameters" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn greet name {\n1s/hello/{@1p}/\n}\ngreet world\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "world\n"

    it "invalid function name produces error" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn @! { ;d }\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "query undefined function is error" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn hdb\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "redefine with no body prints existing body" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn hda { s/^/#/ }\nfn hda\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "fn hda {\ns/^/#/\n}\n"

    it "fn with no args lists defined functions" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn hda { s/a/b/ }\nfn hdr { s/b/a/ }\nfn\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hda hdr\n"

    it "redefine overwrites function" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn xx { s/hello/bye/ }\nfn xx { s/hello/world/ }\nxx\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "world\n"

    it "single-char function name is invalid" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn a { s/a/b/ }\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "defining function with built-in command name fails" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn wq { 1p }\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "function with hdr/hda heading example" $ withTempInput "## My Heading\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn hda { s/^/#/ }\nfn hdr { s/#// }\nhdr\n.p\nhda\n.p\nhda\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "# My Heading\n## My Heading\n### My Heading\n"

    it "function redefine then call uses new body" $ withTempInput "### My Heading\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn hda { s/^/#/ }\nfn hda {\na\n===\n.\n}\nhda\n-1,.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "### My Heading\n===\n"

  describe "range-prefix function invocation" $ do
    it "1,2myfun passes range as first arg" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      -- fn delfirst deletes the line at the address given by @1p
      (code, out, _) <- runLed ("-s" : args) "fn delfirst rng {\n{@1p}d\n}\n1,2delfirst\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "ccc\n"

    it "$myfun passes $ as first arg" $ withTempInput "aaa\nbbb\n" $ \args -> do
      -- fn show prints lines at the range given by @1p
      (code, out, _) <- runLed ("-s" : args) "fn show rng {\n{@1p}p\n}\n$show\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "bbb\n"

    it "range-prefix with extra args" $ withTempInput "hello\nworld\n" $ \args -> do
      -- fn rep substitutes using range from @1p and replacement from @2p
      (code, out, _) <- runLed ("-s" : args) "fn rep rng val {\n{@1p}s/.*/{@2p}/\n}\n1rep replaced\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "replaced\nworld\n"

    it "range-prefix does not trigger for undefined functions" $ withTempInput "aaa\n" $ \args -> do
      -- 1p is a normal command, not a function invocation
      (code, out, _) <- runLed ("-s" : args) "1p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "aaa\n"

  describe "error context in messages" $ do
    it "error in function includes function name and offending line" $ withTempInput "aaa\n" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "H\nfn bad { 999p }\nbad\nQ\n"
      out `shouldSatisfy` ("fn **bad**" `isInfixOf`)
      out `shouldSatisfy` (">> 999p <<" `isInfixOf`)

    it "error at top level has no function context" $ withTempInput "aaa\n" $ \args -> do
      (_, out, _) <- runLed ("-s" : args) "H\n999p\nQ\n"
      out `shouldSatisfy` (not . ("fn **" `isInfixOf`))

    it "error in function called from script includes script name" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-err-ctx" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/myscript.led"
        writeFile scriptPath "fn bad { 999p }\nbad\n"
        (_, out, _) <- runLed ("-s" : args) ("H\nim " ++ scriptPath ++ "\nQ\n")
        out `shouldSatisfy` ("fn **bad**" `isInfixOf`)
        out `shouldSatisfy` (">> 999p <<" `isInfixOf`)
        out `shouldSatisfy` ("in myscript.led" `isInfixOf`)

    it "error in script without function includes script name only" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-err-ctx" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/broken.led"
        writeFile scriptPath "999p\n"
        (_, out, _) <- runLed ("-s" : args) ("H\nim " ++ scriptPath ++ "\nQ\n")
        out `shouldSatisfy` (not . ("fn **" `isInfixOf`))
        out `shouldSatisfy` ("in broken.led" `isInfixOf`)

  describe "@ prefix (parameter document)" $ do
    it "@f prints param doc name (main at top level)" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@f\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "main\n"

    it "@a appends to param doc, @,p prints param doc" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a\nhello\nworld\n.\n@,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\nworld\n"

    it "function creates param doc with args, @1p reads first param" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn greet name {\n1s/hello/{@1p}/\n}\ngreet world\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "world\n"

    it "function with multiple params" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn swap aa bb {\n1s/aaa/{@2p}{@1p}/\n}\nswap hello world\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "worldhello\n"

    it "@e reads file content into param doc" $ do
      withSystemTempDirectory "led-param-test" $ \tmpDir -> do
        let inputFile = tmpDir ++ "/input.txt"
            dataFile = tmpDir ++ "/data.txt"
        writeFile inputFile "aaa\n"
        writeFile dataFile "alpha\nbeta\n"
        (code, out, _) <- runLed ["-s", inputFile] ("@e " ++ dataFile ++ "\n@,p\nQ\n")
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("alpha" `isInfixOf`)
        out `shouldSatisfy` ("beta" `isInfixOf`)

    it "@w writes param doc to file" $ do
      withSystemTempDirectory "led-param-test" $ \tmpDir -> do
        let inputFile = tmpDir ++ "/input.txt"
            outFile = tmpDir ++ "/out.txt"
        writeFile inputFile "aaa\n"
        (code, _out, _) <- runLed ["-s", inputFile] ("@a\nhello\nworld\n.\n@w " ++ outFile ++ "\nQ\n")
        code `shouldBe` ExitSuccess
        contents <- readFile outFile
        contents `shouldBe` "hello\nworld\n"

    it "@wq writes param doc but doesn't quit" $ do
      withSystemTempDirectory "led-param-test" $ \tmpDir -> do
        let inputFile = tmpDir ++ "/input.txt"
            outFile = tmpDir ++ "/out.txt"
        writeFile inputFile "aaa\n"
        (code, out, _) <- runLed ["-s", inputFile] ("@a\ndata\n.\n@wq " ++ outFile ++ "\n1p\nQ\n")
        code `shouldBe` ExitSuccess
        -- Should not have quit; 1p prints current doc line
        out `shouldSatisfy` ("aaa" `isInfixOf`)
        contents <- readFile outFile
        contents `shouldBe` "data\n"

    it "@f with arg is rejected on param doc" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@f newname\nQ\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "param doc is scoped to function invocation" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn test xx {\n@f\n}\ntest hello\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "test\n"

    it "$t@0 copies last line to beginning of param doc" $ withTempInput "hello\nworld\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "$t@0\n@,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "world\n"

    it "1,2t@$ copies lines to end of param doc" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a\nfirst\n.\n1,2t@$\n@,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "first\naaa\nbbb\n"

    it "$m@0 moves last line to param doc" $ withTempInput "hello\nworld\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "$m@0\n,p\n@,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\nworld\n"

    it "@$ prints last line number of param doc" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn test aa bb cc {\n@=\n}\ntest x y z\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "3\n"

    it "@g/RE/cmd executes global command on param doc" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a\nhello\nworld\n.\n@g/hello/p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\n"

    it "@v/RE/cmd executes inverse global on param doc" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a\nhello\nworld\n.\n@v/hello/p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "world\n"

    it "@g with substitution modifies param doc" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a\nhello\nworld\n.\n@g/hello/s/hello/goodbye/\n@,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "goodbye\nworld\n"

    it "cross-doc refs inside @ context refer to real documents" $ withTempInput "hello\nworld\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a\nfirst\n.\n@1v/^$/1:,n\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "1\thello\n2\tworld\n"

    it "@v conditional print of file via cross-doc ref" $ withTempInput "line1\nline2\nline3\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "@a {f}\n@1v/^[ \\t]*$/1:,n\n@,d\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should print all 3 lines of the file with line numbers
      out `shouldSatisfy` ("1\tline1" `isInfixOf`)
      out `shouldSatisfy` ("3\tline3" `isInfixOf`)

  describe "multiline brace blocks" $ do
    it "multiline expression substitution" $ withTempInput "hello\nworld\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1s/hello/{\n@a\nreplacement\n.\n@1p\n}/\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "replacement\n"

    it "fn definition with multiline body" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn upper {\n1s/a/A/\n1s/A/B/\n}\nupper\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Baa\n"

    it "single-line fn definition with braces" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn uc { 1s/a/A/ }\nuc\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Aaa\n"

    it "fn definition with delimiter syntax" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn/uc/1s/a/A/\nuc\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Aaa\n"

    it "fn definition with delimiter and multiline body" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn/upper/1s/a/A/\\\n1s/A/B/\nupper\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Baa\n"

    it "fn calling with delimiter params" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn greet { 1s/{@1p}/{@2p}/ }\ngreet/hello/goodbye\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "goodbye world\n"

    it "fn calling with delimiter params and suffix" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn greet { 1s/{@1p}/{@2p}/ }\ngreet/hello/goodbyep\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "goodbye world\n"

    it "fn calling with delimiter params trailing delimiter" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn greet { 1s/{@1p}/{@2p}/ }\ngreet/hello/goodbye/\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "goodbye world\n"

    it "fn calling with whitespace in delimiter params" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn greet { 1s/{@1p}/{@2p}/ }\ngreet/hello world/goodbye world\n.p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "goodbye world\n"

    it "fn calling with whitespace params and suffix" $ withTempInput "aaa bbb\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn rep { 1s/{@1p}/{@2p}/ }\nrep/aaa bbb/ccc dddp\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "ccc ddd\n"

    it "fn calling with whitespace params preserves internal spaces" $ withTempInput "test\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn show { @,p }\nshow/one two/three  four/\nQ\n"
      code `shouldBe` ExitSuccess
      -- Each param becomes a line in param doc, preserving internal whitespace
      out `shouldBe` "one two\nthree  four\n"

  describe "im command" $ do
    it "im file executes commands from file" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-im-test" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/test.led"
        writeFile scriptPath "a\nhello\n.\n"
        (code, out, _) <- runLed ("-s" : args) ("im " ++ scriptPath ++ "\n,p\nQ\n")
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("hello" `isInfixOf`)

    it "im !command executes commands from shell output" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "im !printf 'a\\nhello\\n.\\n'\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("hello" `isInfixOf`)

    it "im !command prints ! after execution unless -s" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed args "im !printf '#nothing\\n'\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("!" `isInfixOf`)

    it "im !command does not print ! with -s flag" $ withTempInput "aaa\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "im !printf '#nothing\\n'\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldNotSatisfy` ("!" `isInfixOf`)

    it "im !command can define functions" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "im !printf 'fn hello \\{ a Hello!\\n.\\n1,$p \\}\\n'\nhello\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("Hello!" `isInfixOf`)

    it "im !command does not modify import directory stack" $ withTempInput "aaa\n" $ \args -> do
      cwd <- getCurrentDirectory
      (code, out, _) <- runLed ("-s" : args) "im !printf '#comment\\n'\nimd\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` (cwd ++ "\n")

  describe "-e flag" $ do
    it "-e executes script on startup" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-e-test" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/script.led"
        writeFile scriptPath "a\nhello\n.\n"
        (code, out, _) <- runLed ("-s" : "-e" : scriptPath : args) ",p\nQ\n"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("hello" `isInfixOf`)

    it "multiple -e flags execute in order" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-e-test" $ \tmpDir -> do
        let script1 = tmpDir ++ "/script1.led"
            script2 = tmpDir ++ "/script2.led"
        writeFile script1 "a\nfirst\n.\n"
        writeFile script2 "a\nsecond\n.\n"
        (code, out, _) <- runLed ("-s" : "-e" : script1 : "-e" : script2 : args) ",p\nQ\n"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("first" `isInfixOf`)
        out `shouldSatisfy` ("second" `isInfixOf`)

    it "-e can define functions available in session" $ withTempInput "" $ \args -> do
      withSystemTempDirectory "led-e-test" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/funcs.led"
        writeFile scriptPath "fn hello { a Hello!\n.\n1,$p }\n"
        (code, out, _) <- runLed ("-s" : "-e" : scriptPath : args) "hello\nQ\n"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("Hello!" `isInfixOf`)

  describe "imd command" $ do
    it "imd returns cwd by default" $ withTempInput "aaa\n" $ \args -> do
      cwd <- getCurrentDirectory
      (code, out, _) <- runLed ("-s" : args) "imd\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` (cwd ++ "\n")

    it "imd returns script dir during im execution" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-imd-test" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/test.led"
        writeFile scriptPath "imd\n"
        (code, out, _) <- runLed ("-s" : args) ("im " ++ scriptPath ++ "\nQ\n")
        code `shouldBe` ExitSuccess
        out `shouldBe` (tmpDir ++ "\n")

    it "imd restores to cwd after im completes" $ withTempInput "aaa\n" $ \args -> do
      cwd <- getCurrentDirectory
      withSystemTempDirectory "led-imd-test" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/test.led"
        writeFile scriptPath "# nothing\n"
        (code, out, _) <- runLed ("-s" : args) ("im " ++ scriptPath ++ "\nimd\nQ\n")
        code `shouldBe` ExitSuccess
        out `shouldBe` (cwd ++ "\n")

    it "nested im pushes and pops correctly" $ withTempInput "aaa\n" $ \args -> do
      withSystemTempDirectory "led-imd-outer" $ \outerDir -> do
        let innerDir = outerDir ++ "/sub"
        createDirectoryIfMissing True innerDir
        let innerScript = innerDir ++ "/inner.led"
            outerScript = outerDir ++ "/outer.led"
        writeFile innerScript "imd\n"
        writeFile outerScript ("im " ++ innerScript ++ "\nimd\n")
        (code, out, _) <- runLed ("-s" : args) ("im " ++ outerScript ++ "\nQ\n")
        code `shouldBe` ExitSuccess
        -- inner.led prints innerDir, then outer.led prints outerDir
        let outLines = splitOn '\n' out
        -- Last element is empty string from trailing newline
        filter (not . null) outLines `shouldBe` [innerDir, outerDir]

    it "imd via expression substitution in im" $ withTempInput "" $ \args -> do
      withSystemTempDirectory "led-imd-expr" $ \tmpDir -> do
        let scriptPath = tmpDir ++ "/test.led"
        writeFile scriptPath "a {imd}\n.\n1p\n"
        (code, out, _) <- runLed ("-s" : args) ("im " ++ scriptPath ++ "\nQ\n")
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` (tmpDir `isInfixOf`)

  describe "multi-document support" $ do
    describe "&a (append document)" $ do
      it "&a opens a new document with filename from input" $ withTempInput "aaa\n" $ \args -> do
        (code, out, _) <- runLed ("-s" : args) "&a\ntest.txt\n.\n&n\nQ\n"
        code `shouldBe` ExitSuccess
        -- Should show two documents: original file and test.txt
        out `shouldSatisfy` ("test.txt" `isInfixOf`)

      it "&a with existing file loads its contents" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n1p\nQ\n")
          code `shouldBe` ExitSuccess
          -- After &a, current doc is file2, so 1p prints its first line
          out `shouldSatisfy` ("content2" `isInfixOf`)

    describe "&c (change filename)" $ do
      it "&c changes the current document filename" $ withTempInput "aaa\n" $ \args -> do
        (code, out, _) <- runLed ("-s" : args) "&c\nnewname.txt\n.\n&p\nQ\n"
        code `shouldBe` ExitSuccess
        out `shouldBe` "newname.txt\n"

      it "&c with no input closes the current document" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          -- &c on first doc with empty input deletes filename, closing doc 1
          (code, out, _) <- runLed ["-s", file1, file2] "&c\n.\n&p\nQ\n"
          code `shouldBe` ExitSuccess
          -- After closing doc 1, only file2 remains
          out `shouldSatisfy` ("file2.txt" `isInfixOf`)

    describe "&d (delete/close document)" $ do
      it "&d closes unmodified document" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&d\n&n\nQ\n")
          code `shouldBe` ExitSuccess
          -- After deleting file2, only file1 should remain
          out `shouldSatisfy` ("file1.txt" `isInfixOf`)
          out `shouldSatisfy` (not . ("file2.txt" `isInfixOf`))

    describe "&n (list documents with markers)" $ do
      it "&n lists all documents with numbers and current marker" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&1\n&,n\nQ\n")
          code `shouldBe` ExitSuccess
          -- Current document (1) should have > marker (format: "1\t>filename")
          out `shouldSatisfy` ("1\t>" `isInfixOf`)
          -- Other document should have space instead of > (format: "2\t filename")
          out `shouldSatisfy` ("2\t " `isInfixOf`)

      it "&n shows * for modified documents" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          -- Open both, switch to doc 2, modify it, then list
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&2\n1s/content2/modified/\n&,n\nQ\n")
          code `shouldBe` ExitSuccess
          -- Doc 2 is current and modified (format: "2\t>filename*")
          out `shouldSatisfy` ("\t>" `isInfixOf`)
          out `shouldSatisfy` ("*" `isInfixOf`)
          -- Doc 1 is unmodified, no * marker
          let outLines = splitOn '\n' out
              doc1Line = filter (\l -> "file1.txt" `isInfixOf` l) outLines
          case doc1Line of
            (l:_) -> l `shouldSatisfy` (not . ("*" `isInfixOf`))
            []    -> expectationFailure "file1.txt line not found in output"

      it "&n shows no * for unmodified documents" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&1\n&,n\nQ\n")
          code `shouldBe` ExitSuccess
          -- Neither document is modified, no * should appear
          out `shouldSatisfy` (not . ("*" `isInfixOf`))

    describe "&g (global on documents)" $ do
      it "&g/RE/s substitutes matching document filenames" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&g/file/s/file/doc/\n&,p\nQ\n")
          code `shouldBe` ExitSuccess
          -- Both filenames should have "file" replaced with "doc"
          out `shouldSatisfy` ("doc1.txt" `isInfixOf`)
          out `shouldSatisfy` ("doc2.txt" `isInfixOf`)

      it "&d closes multiple documents" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
              other = tmpDir ++ "/other.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          writeFile other "other\n"
          -- Open file2 and other, then delete first two (file1 and file2), keeping other
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&a\n" ++ other ++ "\n.\n&1,2d\n&,n\nQ\n")
          code `shouldBe` ExitSuccess
          -- Only other.txt should remain
          out `shouldSatisfy` ("other.txt" `isInfixOf`)
          out `shouldSatisfy` (not . ("file1.txt" `isInfixOf`))
          out `shouldSatisfy` (not . ("file2.txt" `isInfixOf`))

    describe "&e (edit into doc list)" $ do
      it "&e reads filenames from file into doc list" $ do
        withSystemTempDirectory "led-doclist-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
              listFile = tmpDir ++ "/list.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          writeFile listFile (file1 ++ "\n" ++ file2 ++ "\n")
          (code, out, _) <- runLed ["-s", file1] ("&E " ++ listFile ++ "\n&,p\nQ\n")
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` ("file1.txt" `isInfixOf`)
          out `shouldSatisfy` ("file2.txt" `isInfixOf`)

    describe "&w (write doc list)" $ do
      it "&w writes doc list filenames to file" $ do
        withSystemTempDirectory "led-doclist-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
              outFile = tmpDir ++ "/out.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, _out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&w " ++ outFile ++ "\nQ\n")
          code `shouldBe` ExitSuccess
          contents <- readFile outFile
          contents `shouldSatisfy` ("file1.txt" `isInfixOf`)
          contents `shouldSatisfy` ("file2.txt" `isInfixOf`)

    describe "&u (undo on doc list)" $ do
      it "&u is rejected (undo is global, not doc-list-specific)" $ do
        withSystemTempDirectory "led-doclist-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
          writeFile file1 "content1\n"
          -- &u should be rejected as undo is a global operation
          (code, out, _) <- runLed ["-s", file1] "&u\nQ\n"
          code `shouldBe` ExitFailure 1
          out `shouldSatisfy` ("?" `isInfixOf`)

    describe "&f rejection" $ do
      it "&f with arg is rejected on doc list" $ do
        withSystemTempDirectory "led-doclist-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
          writeFile file1 "content1\n"
          (code, out, _) <- runLed ["-s", file1] "&f newname\nQ\n"
          code `shouldBe` ExitFailure 1
          out `shouldSatisfy` ("?" `isInfixOf`)

    describe "&& (file-managing document list)" $ do
      it "&&a creates a new file on disk and opens it" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              newFile = tmpDir ++ "/new.txt"
          writeFile file1 "content1\n"
          (code, out, _) <- runLed ["-s", file1] ("&&a\n" ++ newFile ++ "\n.\n&,n\nQ\n")
          code `shouldBe` ExitSuccess
          -- new.txt should exist on disk
          doesFileExist newFile `shouldReturn` True
          -- Should show two documents
          out `shouldSatisfy` ("new.txt" `isInfixOf`)

      it "&&d warns about file deletion" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          -- &&$d should warn (about deleting file2) and abort in non-interactive mode
          (code, _out, _) <- runLed ["-s", file1, file2] ("&&$d\nQ\n")
          code `shouldBe` ExitFailure 1
          -- file2 should still exist (warning reverted the action)
          doesFileExist file2 `shouldReturn` True

      it "&&s renames file on disk" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
          writeFile file1 "hello\n"
          (code, _, _) <- runLed ["-s", file1] ("&&s/xalpha/xbeta/\nQ\n")
          code `shouldBe` ExitSuccess
          -- xalpha.txt should be gone, xbeta.txt should exist
          doesFileExist file1 `shouldReturn` False
          doesFileExist (tmpDir ++ "/xbeta.txt") `shouldReturn` True

      it "single rename has no warning" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
          writeFile file1 "hello\n"
          -- Single rename should proceed immediately (no second attempt needed)
          (code, out, _) <- runLed ["-s", file1] ("&&s/xalpha/xbeta/\n&,p\nQ\n")
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` ("xbeta.txt" `isInfixOf`)

      it "mass rename (2+) warns on first attempt" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
              file2 = tmpDir ++ "/xbeta.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          -- Mass rename should warn and abort in non-interactive mode
          (code, _out, _err) <- runLed ["-s", file1, file2] ("&&,s/\\.txt/.md/\nQ\n")
          code `shouldBe` ExitFailure 1
          -- Files should be unchanged (warning reverted)
          doesFileExist file1 `shouldReturn` True
          doesFileExist file2 `shouldReturn` True

      it "&&a warns if file already exists on disk" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              existing = tmpDir ++ "/existing.txt"
          writeFile file1 "content1\n"
          writeFile existing "already here\n"
          -- &&a for existing file should warn and abort in non-interactive mode
          (code, _out, _) <- runLed ["-s", file1] ("&&a\n" ++ existing ++ "\n.\nQ\n")
          code `shouldBe` ExitFailure 1

      it "rename preserves in-memory buffer content" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
          writeFile file1 "original content\n"
          -- Load, modify buffer, then rename via &&s
          (code, out, _) <- runLed ["-s", file1] ("1c\nmodified\n.\n&&s/xalpha/xbeta/\n1p\nQ\n")
          code `shouldBe` ExitSuccess
          -- After rename, buffer should still have modified content
          out `shouldSatisfy` ("modified" `isInfixOf`)

      it "&&e reads filenames from file with disk effects" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              newFile = tmpDir ++ "/new.txt"
              listFile = tmpDir ++ "/list.txt"
          writeFile file1 "content1\n"
          writeFile listFile (file1 ++ "\n" ++ newFile ++ "\n")
          (code, out, _) <- runLed ["-s", file1] ("&&E " ++ listFile ++ "\n&,p\nQ\n")
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` ("new.txt" `isInfixOf`)
          doesFileExist newFile `shouldReturn` True

      it "&&f with arg is rejected" $ do
        withSystemTempDirectory "led-manage-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
          writeFile file1 "content1\n"
          (code, out, _) <- runLed ["-s", file1] "&&f newname\nQ\n"
          code `shouldBe` ExitFailure 1
          out `shouldSatisfy` ("?" `isInfixOf`)

    describe "document switching" $ do
      it "&2 switches to document 2" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&1\n&2\n1p\nQ\n")
          code `shouldBe` ExitSuccess
          -- After &2, current doc is file2, so 1p prints its content
          out `shouldSatisfy` ("content2" `isInfixOf`)

    describe "cross-document operations" $ do
      it "1:1p prints line 1 of document 1" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "first\n"
          writeFile file2 "second\n"
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n1:1p\nQ\n")
          code `shouldBe` ExitSuccess
          -- 1:1p should print "first" from doc 1 even though we're in doc 2
          out `shouldSatisfy` ("first" `isInfixOf`)

      it "1:,t0 transfers doc 1 contents to beginning of active document" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "header\n"
          writeFile file2 "body\n"
          -- Open file1, add file2, switch to file2 (&2), then transfer doc 1 to line 0
          (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&2\n1:,t0\n,p\nQ\n")
          code `shouldBe` ExitSuccess
          -- file2 should now have "header" before "body"
          out `shouldSatisfy` ("header" `isInfixOf`)
          out `shouldSatisfy` ("body" `isInfixOf`)

      it ",:w writes all documents" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "original1\n"
          writeFile file2 "original2\n"
          -- Modify both files and save all with ,:w
          (code, _, _) <- runLed ["-s", file1, file2] "1s/original/modified/\n&2\n1s/original/modified/\n,:w\nQ\n"
          code `shouldBe` ExitSuccess
          -- Both files should be modified
          content1 <- readFileBS file1
          content2 <- readFileBS file2
          content1 `shouldBe` "modified1\n"
          content2 `shouldBe` "modified2\n"

      it "document range 1,2:,p prints all lines from docs 1 and 2" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
              file3 = tmpDir ++ "/file3.txt"
          writeFile file1 "first\n"
          writeFile file2 "second\n"
          writeFile file3 "third\n"
          (code, out, _) <- runLed ["-s", file1, file2, file3] "1,2:,p\nQ\n"
          code `shouldBe` ExitSuccess
          -- Should print contents of doc 1 and doc 2, but not doc 3
          out `shouldSatisfy` ("first" `isInfixOf`)
          out `shouldSatisfy` ("second" `isInfixOf`)
          out `shouldSatisfy` (not . ("third" `isInfixOf`))

      it "document range 1,2:,n prints with docnum:linenum format" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "line1\nline2\n"
          writeFile file2 "lineA\nlineB\n"
          (code, out, _) <- runLed ["-s", file1, file2] "1,2:,n\nQ\n"
          code `shouldBe` ExitSuccess
          -- Should print with docnum:linenum format
          out `shouldSatisfy` ("1:1\tline1" `isInfixOf`)
          out `shouldSatisfy` ("1:2\tline2" `isInfixOf`)
          out `shouldSatisfy` ("2:1\tlineA" `isInfixOf`)
          out `shouldSatisfy` ("2:2\tlineB" `isInfixOf`)

      it "single doc 1:,n prints with linenum only (no doc prefix)" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "line1\nline2\n"
          writeFile file2 "lineA\nlineB\n"
          (code, out, _) <- runLed ["-s", file1, file2] "1:,n\nQ\n"
          code `shouldBe` ExitSuccess
          -- Should print with linenum only (no doc prefix for single doc)
          out `shouldSatisfy` ("1\tline1" `isInfixOf`)
          out `shouldSatisfy` ("2\tline2" `isInfixOf`)
          -- Should not have doc prefix
          out `shouldSatisfy` (not . ("1:1\t" `isInfixOf`))

    describe "document marks as cross-document handles" $ do
      it "&ka then 'a:,p prints the marked document" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
              file2 = tmpDir ++ "/xbeta.txt"
          writeFile file1 "first\n"
          writeFile file2 "second\n"
          -- Mark doc 1 on the doc list, then use 'a: to reference it
          (code, out, _) <- runLed ["-s", file1, file2] "&1ka\n'a:,p\nQ\n"
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` ("first" `isInfixOf`)

      it "&1ka + &3kb then 'a,'b:,p prints docs 1-3" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
              file2 = tmpDir ++ "/xbeta.txt"
              file3 = tmpDir ++ "/xgamma.txt"
          writeFile file1 "first\n"
          writeFile file2 "second\n"
          writeFile file3 "third\n"
          (code, out, _) <- runLed ["-s", file1, file2, file3] "&1ka\n&3kb\n'a,'b:,p\nQ\n"
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` ("first" `isInfixOf`)
          out `shouldSatisfy` ("second" `isInfixOf`)
          out `shouldSatisfy` ("third" `isInfixOf`)

      it "@ka + @'ap works on parameter document" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/xalpha.txt"
          writeFile file1 "aaa\nbbb\nccc\n"
          -- Define a function that marks line 1 on param doc, then prints it via 'a
          (code, out, _) <- runLed ("-s" : [file1]) "fn test { @1ka\n@'ap }\ntest xx yy zz\nQ\n"
          code `shouldBe` ExitSuccess
          -- test is called with args xx yy zz, so param doc has 3 lines
          -- @1ka marks line 1 (xx), @'ap prints mark a (line 1 = xx)
          out `shouldSatisfy` ("xx" `isInfixOf`)

    describe "multiple files from command line" $ do
      it "opens multiple files specified on command line" $ do
        withSystemTempDirectory "led-multi-test" $ \tmpDir -> do
          let file1 = tmpDir ++ "/file1.txt"
              file2 = tmpDir ++ "/file2.txt"
          writeFile file1 "content1\n"
          writeFile file2 "content2\n"
          (code, out, _) <- runLed ["-s", file1, file2] "&,n\nQ\n"
          code `shouldBe` ExitSuccess
          out `shouldSatisfy` ("file1.txt" `isInfixOf`)
          out `shouldSatisfy` ("file2.txt" `isInfixOf`)

  describe "user function suffixes" $ do
    it "myfunp prints current line after function" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn myfun { 1d }\nmyfunp\nQ\n"
      code `shouldBe` ExitSuccess
      -- After deleting line 1 ("aaa"), current line becomes 1 ("bbb"), suffix p prints it
      out `shouldBe` "bbb\n"

    it "myfunn prints current line with number after function" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn myfun { 1d }\nmyfunn\nQ\n"
      code `shouldBe` ExitSuccess
      -- After deleting line 1, current line is 1 ("bbb"), suffix n prints "1\tbbb"
      out `shouldBe` "1\tbbb\n"

    it "function with range prefix and suffix" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn show rng { {@1p}p }\n2showp\nQ\n"
      code `shouldBe` ExitSuccess
      -- 2show passes "2" as arg, {@1p} prints "2", then 2p prints line 2
      -- After function, suffix p prints current line again
      out `shouldSatisfy` ("bbb" `isInfixOf`)

    it "shadowing warning when defining function ending in p" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn myfun { 1p }\nfn myfunp { 2p }\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("Warning" `isInfixOf`)

    it "shadowing warning mentions the shorter function" $ withTempInput "" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn myfun { 1p }\nfn myfunp { 2p }\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("myfun" `isInfixOf`)

    it "longest match wins: defined myfunp is not suffix" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "fn myfun { 1p }\nfn myfunp { 2p }\nmyfunp\nQ\n"
      code `shouldBe` ExitSuccess
      -- myfunp should invoke the "myfunp" function (prints line 2), not "myfun" with suffix p
      out `shouldSatisfy` ("bbb" `isInfixOf`)

  describe "auto-create empty document when doc list becomes empty" $ do
    it "&,d on single document recovers with empty unnamed document" $ withTempInput "hello\n" $ \args -> do
      -- Delete all documents, then check that we recovered
      (code, out, _) <- runLed ("-s" : args) "&,d\n&,n\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should have exactly one document listed (the auto-created empty one, marked as current)
      -- Format: "1\t>" (number tab current-marker)
      out `shouldSatisfy` ("1\t>" `isInfixOf`)

    it "&,d recovery document is empty" $ withTempInput "hello\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "&,d\n=\nQ\n"
      code `shouldBe` ExitSuccess
      -- Line count of recovered empty document should be 0
      out `shouldSatisfy` ("0" `isInfixOf`)

    it "&,d recovery document is unnamed" $ withTempInput "hello\n" $ \args -> do
      (code, _out, _) <- runLed ("-s" : args) "&,d\nf\nQ\n"
      code `shouldBe` ExitSuccess
      -- f command on unnamed document should print empty or no filename

    it "deleting all documents in multi-doc setup recovers" $ do
      withSystemTempDirectory "led-empty-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
            file2 = tmpDir ++ "/xfile2.txt"
        writeFile file1 "content1\n"
        writeFile file2 "content2\n"
        (code, out, _) <- runLed ["-s", file1] ("&a\n" ++ file2 ++ "\n.\n&,d\n&,n\nQ\n")
        code `shouldBe` ExitSuccess
        -- After deleting all, should have one recovered document (marked as current)
        -- Format: "1\t>" (number tab current-marker)
        out `shouldSatisfy` ("1\t>" `isInfixOf`)
        -- The original filenames should not appear in the document list
        out `shouldSatisfy` (not . ("xfile1.txt" `isInfixOf`))
        out `shouldSatisfy` (not . ("xfile2.txt" `isInfixOf`))

  describe "multilevel undo/redo" $ do
    it "u undoes single change" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1d\nu\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After delete + undo, all original lines should be present
      out `shouldSatisfy` ("aaa" `isInfixOf`)
      out `shouldSatisfy` ("bbb" `isInfixOf`)
      out `shouldSatisfy` ("ccc" `isInfixOf`)

    it "multiple undos work" $ withTempInput "line1\nline2\nline3\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1d\n2d\nu\nu\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After two deletes + two undos, all original lines should be present
      out `shouldSatisfy` ("line1" `isInfixOf`)
      out `shouldSatisfy` ("line2" `isInfixOf`)
      out `shouldSatisfy` ("line3" `isInfixOf`)

    it "U (redo) redoes an undone change" $ withTempInput "alpha\nbeta\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1d\nu\nU\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After delete, undo, redo: should only have "beta"
      out `shouldSatisfy` (not . ("alpha" `isInfixOf`))
      out `shouldSatisfy` ("beta" `isInfixOf`)

    it "new command clears redo stack" $ withTempInput "one\ntwo\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "1d\nu\na\nthree\n.\nU\n,p\nQ\n"
      code `shouldBe` ExitFailure 1  -- U should fail (nothing to redo)
      -- The redo stack was cleared by the 'a' command

    it "u with nothing to undo shows error" $ withTempInput "test\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "u\nQ\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "U with nothing to redo shows error" $ withTempInput "test\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "U\nQ\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "substitute command can be undone" $ withTempInput "hello world\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "s/world/universe/\nu\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After substitute + undo, original text should be restored
      out `shouldSatisfy` ("hello world" `isInfixOf`)

    it "im command is single undo step" $ do
      withSystemTempDirectory "led-undo-im-test" $ \tmpDir -> do
        let dataFile = tmpDir ++ "/data.txt"
            scriptFile = tmpDir ++ "/script.led"
        writeFile dataFile "original\n"
        writeFile scriptFile "1d\na\nnew1\nnew2\n.\n"
        (code, out, _) <- runLed ["-s", dataFile] ("im " ++ scriptFile ++ "\nu\n,p\nQ\n")
        code `shouldBe` ExitSuccess
        -- After im (which did delete and append) + single undo, original should be restored
        out `shouldSatisfy` ("original" `isInfixOf`)
        out `shouldSatisfy` (not . ("new1" `isInfixOf`))
        out `shouldSatisfy` (not . ("new2" `isInfixOf`))

    it "u restores document after &d closes it" $ do
      withSystemTempDirectory "led-undo-close-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xundoclose.txt"
        writeFile file1 "original content\n"
        -- Open file, delete text, write, close, then undo the close
        -- After undoing close, the document should be back with its content
        (code, out, _) <- runLed ["-s", file1] $
          ",d\n" ++      -- delete all text
          "w\n" ++       -- write (file is now empty)
          "&d\n" ++      -- close the document (creates default empty doc)
          "&,n\n" ++     -- show doc list (should show only default)
          "u\n" ++       -- undo the close
          "&,n\n" ++     -- show doc list again (should show xundoclose.txt back)
          "u\n" ++       -- undo the delete
          ",n\n" ++      -- show content (should be restored)
          "Q\n"
        code `shouldBe` ExitSuccess
        -- After second undo, original content should be restored
        out `shouldSatisfy` ("original content" `isInfixOf`)
        -- The filename should appear after the first undo
        out `shouldSatisfy` ("xundoclose.txt" `isInfixOf`)

    it "u restores multiple documents after closing" $ do
      withSystemTempDirectory "led-undo-multi-close-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xmulti1.txt"
            file2 = tmpDir ++ "/xmulti2.txt"
        writeFile file1 "content one\n"
        writeFile file2 "content two\n"
        -- Open two files, close the second, undo, check it's restored
        (code, out, _) <- runLed ["-s", file1, file2] $
          "&2d\n" ++     -- close second document
          "&,n\n" ++     -- show doc list (should show only xmulti1.txt)
          "u\n" ++       -- undo the close
          "&,n\n" ++     -- show doc list (should show both files)
          "2:,p\n" ++    -- print content of second doc
          "Q\n"
        code `shouldBe` ExitSuccess
        -- After undo, both files should be in the list
        out `shouldSatisfy` ("xmulti1.txt" `isInfixOf`)
        out `shouldSatisfy` ("xmulti2.txt" `isInfixOf`)
        -- The content of file2 should be restored
        out `shouldSatisfy` ("content two" `isInfixOf`)

  describe "% previous range" $ do
    it "% in line range uses previous line range" $ withTempInput "aaa\nbbb\nccc\nddd\n" $ \args -> do
      -- 2,3p prints lines 2-3, then %p uses same range again
      (code, out, _) <- runLed ("-s" : args) "2,3p\n%p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "bbb\nccc\nbbb\nccc\n"

    it "% in doc range uses previous doc range" $ do
      withSystemTempDirectory "led-prev-range-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/f1.txt"
            file2 = tmpDir ++ "/f2.txt"
        writeFile file1 "file1-line1\nfile1-line2\n"
        writeFile file2 "file2-line1\nfile2-line2\n"
        -- 1:1p prints line 1 of doc 1, then %:2p prints line 2 of doc 1 (same doc)
        (code, out, _) <- runLed ["-s", file1, file2] "1:1p\n%:2p\nQ\n"
        code `shouldBe` ExitSuccess
        out `shouldBe` "file1-line1\nfile1-line2\n"

    it "% with unresolved address 2,$" $ withTempInput "aaa\nbbb\nccc\nddd\n" $ \args -> do
      -- 2,$p prints lines 2-4, then switching docs and using % should use 2,$
      (code, out, _) <- runLed ("-s" : args) "2,$p\n%d\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After deleting 2,$, only "aaa" remains
      out `shouldBe` "bbb\nccc\nddd\naaa\n"

    it "% error when no previous range" $ withTempInput "aaa\n" $ \args -> do
      -- First command uses %, but no previous range exists
      (code, out, _) <- runLed ("-s" : args) "%p\nQ\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "%: error when no previous doc range" $ withTempInput "aaa\n" $ \args -> do
      -- First command uses %:, but no previous doc range exists
      (code, out, _) <- runLed ("-s" : args) "%:1p\nQ\n"
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` ("?" `isInfixOf`)

    it "% saves source range of m command, not target" $ withTempInput "aaa\nbbb\nccc\nddd\n" $ \args -> do
      -- 2,3m0 saves range 2,3 (source), not 0 (target)
      -- Then %p should print lines 2-3 (which are now aaa and ddd after move)
      (code, out, _) <- runLed ("-s" : args) "2,3m0\n%p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After 2,3m0: bbb,ccc,aaa,ddd -> %p prints lines 2-3 which are ccc,aaa
      out `shouldBe` "ccc\naaa\n"

    it "% saves source range of t command, not target" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      -- 1,2t$ saves range 1,2 (source), not $ (target)
      -- Then %p should print lines 1-2
      (code, out, _) <- runLed ("-s" : args) "1,2t$\n%p\nQ\n"
      code `shouldBe` ExitSuccess
      -- After 1,2t$: aaa,bbb,ccc,aaa,bbb -> %p prints lines 1-2 which are aaa,bbb
      out `shouldBe` "aaa\nbbb\n"

    it "% saves range from user function call" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      -- Define function, call with range 2,3, verify % saves that range
      -- Function body just prints current line, but the important part is that
      -- calling 2,3showit should save 2,3 to % for later use
      (code, out, _) <- runLed ("-s" : args) "fn showit {\np\n}\n2,3showit\n%p\nQ\n"
      code `shouldBe` ExitSuccess
      -- showit prints current line (ccc), then %p prints 2,3 (bbb,ccc)
      out `shouldBe` "ccc\nbbb\nccc\n"

    it "default range is not saved to %" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      -- 2,3p sets range to 2,3, then p (default = current line) should NOT save
      -- After 2,3p, current line is 3 so p prints ccc, but % should still be 2,3
      (code, out, _) <- runLed ("-s" : args) "2,3p\np\n%p\nQ\n"
      code `shouldBe` ExitSuccess
      -- 2,3p prints bbb,ccc (saves 2,3), p prints ccc (current=3, doesn't save), %p prints bbb,ccc (uses 2,3)
      out `shouldBe` "bbb\nccc\nccc\nbbb\nccc\n"

  describe "cross-doc global command" $ do
    it "g/ command in cross-doc range operates on each matched doc" $ do
      withSystemTempDirectory "led-crossdoc-global" $ \tmpDir -> do
        let file1 = tmpDir ++ "/f1.txt"
            file2 = tmpDir ++ "/f2.txt"
        writeFile file1 "line1\ncat-match\nline3\n"
        writeFile file2 "lineA\ncat-match\nlineC\n"
        -- 1,2:g/cat/m0 should move "cat-match" to top of EACH doc where it matched
        (code, out, _) <- runLed ["-s", file1, file2] "1,2:g/cat/m0\n1:,p\n2:,p\nQ\n"
        code `shouldBe` ExitSuccess
        -- Doc 1 should have cat-match at top
        out `shouldSatisfy` ("cat-match\nline1\nline3\n" `isInfixOf`)
        -- Doc 2 should have cat-match at top
        out `shouldSatisfy` ("cat-match\nlineA\nlineC\n" `isInfixOf`)

    it "g/ with delete operates on each matched doc" $ do
      withSystemTempDirectory "led-crossdoc-global-del" $ \tmpDir -> do
        let file1 = tmpDir ++ "/f1.txt"
            file2 = tmpDir ++ "/f2.txt"
        writeFile file1 "keep1\ndelete-me\nkeep2\n"
        writeFile file2 "keepA\ndelete-me\nkeepB\n"
        -- 1,2:g/delete/d should delete "delete-me" from EACH doc
        (code, out, _) <- runLed ["-s", file1, file2] "1,2:g/delete/d\n1:,p\n2:,p\nQ\n"
        code `shouldBe` ExitSuccess
        -- Doc 1 should not have delete-me
        out `shouldSatisfy` ("keep1\nkeep2\n" `isInfixOf`)
        -- Doc 2 should not have delete-me
        out `shouldSatisfy` ("keepA\nkeepB\n" `isInfixOf`)

    it "g/ with substitute operates on each matched doc" $ do
      withSystemTempDirectory "led-crossdoc-global-sub" $ \tmpDir -> do
        let file1 = tmpDir ++ "/f1.txt"
            file2 = tmpDir ++ "/f2.txt"
        writeFile file1 "hello world\ngoodbye world\n"
        writeFile file2 "hello universe\ngoodbye universe\n"
        -- 1,2:g/hello/s/hello/hi/ should substitute in EACH doc's matching line
        (code, out, _) <- runLed ["-s", file1, file2] "1,2:g/hello/s/hello/hi/\n1:,p\n2:,p\nQ\n"
        code `shouldBe` ExitSuccess
        -- Doc 1 should have "hi world"
        out `shouldSatisfy` ("hi world" `isInfixOf`)
        -- Doc 2 should have "hi universe"
        out `shouldSatisfy` ("hi universe" `isInfixOf`)

  describe "&* (modified documents prefix)" $ do
    it "&*p prints current line from all modified documents" $ do
      withSystemTempDirectory "led-modified-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
            file2 = tmpDir ++ "/xfile2.txt"
            file3 = tmpDir ++ "/xfile3.txt"
        writeFile file1 "content1\n"
        writeFile file2 "content2\n"
        writeFile file3 "content3\n"
        -- Modify file1 and file3, leave file2 unchanged
        -- &*p should print current line from modified docs only
        (code, out, _) <- runLed ["-s", file1, file2, file3] "1s/content1/modified1/\n&3\n1s/content3/modified3/\n&*p\nQ\n"
        code `shouldBe` ExitSuccess
        -- Should print from modified docs (1 and 3)
        out `shouldSatisfy` ("modified1" `isInfixOf`)
        out `shouldSatisfy` ("modified3" `isInfixOf`)
        -- Should NOT print from unmodified doc (2)
        out `shouldSatisfy` (not . ("content2" `isInfixOf`))

    it "&*,p prints all lines from all modified documents" $ do
      withSystemTempDirectory "led-modified-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
            file2 = tmpDir ++ "/xfile2.txt"
        writeFile file1 "line1a\nline1b\n"
        writeFile file2 "line2a\nline2b\n"
        -- Modify only file1
        (code, out, _) <- runLed ["-s", file1, file2] "1a\nnewline\n.\n&*,p\nQ\n"
        code `shouldBe` ExitSuccess
        -- Should print all lines from file1 (modified)
        out `shouldSatisfy` ("line1a" `isInfixOf`)
        out `shouldSatisfy` ("newline" `isInfixOf`)
        out `shouldSatisfy` ("line1b" `isInfixOf`)
        -- Should NOT print from file2 (unmodified)
        out `shouldSatisfy` (not . ("line2a" `isInfixOf`))

    it "&*w writes all modified documents" $ do
      withSystemTempDirectory "led-modified-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
            file2 = tmpDir ++ "/xfile2.txt"
            file3 = tmpDir ++ "/xfile3.txt"
        writeFile file1 "original1\n"
        writeFile file2 "original2\n"
        writeFile file3 "original3\n"
        -- Modify files 1 and 3, then &*w to save only modified
        (code, _, _) <- runLed ["-s", file1, file2, file3] "1s/original1/changed1/\n&3\n1s/original3/changed3/\n&*w\nQ\n"
        code `shouldBe` ExitSuccess
        -- File1 and file3 should be modified on disk
        c1 <- readFileBS file1
        c3 <- readFileBS file3
        c1 `shouldBe` "changed1\n"
        c3 `shouldBe` "changed3\n"
        -- File2 should be unchanged
        c2 <- readFileBS file2
        c2 `shouldBe` "original2\n"

    it "&* with no modified documents shows error" $ do
      withSystemTempDirectory "led-modified-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
        writeFile file1 "content\n"
        -- No modifications made, &*p should fail
        (code, out, _) <- runLed ["-s", file1] "&*p\nQ\n"
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("?" `isInfixOf`)

    it "&*s/old/new/ substitutes in all modified documents" $ do
      withSystemTempDirectory "led-modified-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
            file2 = tmpDir ++ "/xfile2.txt"
        writeFile file1 "hello world\n"
        writeFile file2 "hello earth\n"
        -- Modify file1 by adding a line, then &*,s should substitute on all lines of modified docs
        (code, out, _) <- runLed ["-s", file1, file2] "1a\nextra\n.\n&*,s/hello/hi/\n&*,p\nQ\n"
        code `shouldBe` ExitSuccess
        -- File1 (modified) should have substitution applied
        out `shouldSatisfy` ("hi world" `isInfixOf`)
        -- File2 (unmodified) should not be affected

    it "&*g/pattern/command global on modified documents" $ do
      withSystemTempDirectory "led-modified-test" $ \tmpDir -> do
        let file1 = tmpDir ++ "/xfile1.txt"
            file2 = tmpDir ++ "/xfile2.txt"
        writeFile file1 "aaa\nbbb\nccc\n"
        writeFile file2 "xxx\nyyy\nzzz\n"
        -- Mark file1 as modified, then &*g/./p should print all lines from it
        (code, out, _) <- runLed ["-s", file1, file2] "1d\nu\n&*g/./p\nQ\n"
        code `shouldBe` ExitSuccess
        -- After delete+undo, file1 is modified, so &*g should operate on it
        out `shouldSatisfy` ("aaa" `isInfixOf`)
        out `shouldSatisfy` ("bbb" `isInfixOf`)
        -- File2 is not modified
        out `shouldSatisfy` (not . ("xxx" `isInfixOf`))

  describe "expression substitution in global commands" $ do
    it "simple global without expression still works" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      -- Basic sanity check: g/./.n should print all lines with numbers
      (code, out, _) <- runLed ("-s" : args) "g/./.n\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "1\taaa\n2\tbbb\n3\tccc\n"

    it "g/pattern/{.=}n re-evaluates expression for each match" $ withTempInput "aaa\nbbb\nccc\n" $ \args -> do
      -- {.=} should be evaluated for EACH matching line, not once before the global runs
      -- .= prints current line number; g/./ matches all 3 lines
      (code, out, _) <- runLed ("-s" : args) "g/./{.=}n\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should print 1, 2, 3 (the line number at each iteration)
      out `shouldBe` "1\taaa\n2\tbbb\n3\tccc\n"

    it "g/pattern/cmd with {.=} in substitute replacement" $ withTempInput "alpha\nbeta\ngamma\n" $ \args -> do
      -- {.=} in substitute replacement should use current line number for each match
      (code, out, _) <- runLed ("-s" : args) "g/./s/$/:{.=}/\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Each line should have its own line number appended
      out `shouldBe` "alpha:1\nbeta:2\ngamma:3\n"

    it "g/pattern/cmd evaluates expression after command changes state" $ withTempInput "one\ntwo\nthree\n" $ \args -> do
      -- After delete, {$=} should give the new last line NUMBER (not content)
      -- After deleting line 1, there are 2 lines, so {$=} should be "2"
      (code, out, _) <- runLed ("-s" : args) "g/one/d\n{$=}p\nQ\n"
      code `shouldBe` ExitSuccess
      -- {$=} evaluates to 2 (new last line number), then 2p prints "three"
      out `shouldBe` "three\n"

    it "v/pattern/{.=}n re-evaluates expression for non-matches" $ withTempInput "aa\nbb\ncc\n" $ \args -> do
      -- v/bb/ matches lines 1 and 3; {.=}n should print each line's number
      (code, out, _) <- runLed ("-s" : args) "v/bb/{.=}n\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "1\taa\n3\tcc\n"

  describe "smart replace" $ do
    it "s/lowercase/lowercase/ preserves title case" $ withTempInput "Cat and cat\n" $ \args -> do
      -- Smart replace: all-lowercase pattern and replacement should preserve case
      (code, out, _) <- runLed ("-s" : args) "s/cat/dog/g\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- "Cat" should become "Dog", "cat" should become "dog"
      out `shouldBe` "Dog and dog\n"

    it "s/lowercase/lowercase/ preserves all-caps" $ withTempInput "CAT and cat\n" $ \args -> do
      (code, out, _) <- runLed ("-s" : args) "s/cat/dog/g\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- "CAT" should become "DOG", "cat" should become "dog"
      out `shouldBe` "DOG and dog\n"

    it "s/Uppercase/... disables smart replace" $ withTempInput "Cat and cat\n" $ \args -> do
      -- If pattern has uppercase, no smart replace
      (code, out, _) <- runLed ("-s" : args) "s/Cat/dog/g\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Only "Cat" matches (case-sensitive), becomes "dog"
      out `shouldBe` "dog and cat\n"

    it "smart replace with Cyrillic preserves title case" $ withTempInput "Кот и кот\n" $ \args -> do
      -- Cyrillic smart replace: кот -> пёс
      (code, out, _) <- runLed ("-s" : args) "s/кот/пёс/g\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- "Кот" should become "Пёс", "кот" should become "пёс"
      out `shouldBe` "Пёс и пёс\n"

    it "g/lowercase/ uses smart search (case-insensitive)" $ withTempInput "Cat\nDog\ncat\n" $ \args -> do
      -- g/cat/ with all-lowercase pattern should match both "Cat" and "cat"
      (code, out, _) <- runLed ("-s" : args) "g/cat/p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should print both lines with "cat" (case-insensitive)
      out `shouldBe` "Cat\ncat\n"

    it "g/lowercase/s//replacement/g uses smart replace" $ withTempInput "Cat on mat\ncat on mat\n" $ \args -> do
      -- g/cat/s//dog/g should match case-insensitively and preserve case
      (code, out, _) <- runLed ("-s" : args) "g/cat/s//dog/g\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- "Cat" -> "Dog", "cat" -> "dog"
      out `shouldBe` "Dog on mat\ndog on mat\n"

    it "g/Uppercase/ uses case-sensitive search" $ withTempInput "Cat\nDog\ncat\n" $ \args -> do
      -- g/Cat/ with uppercase should only match "Cat"
      (code, out, _) <- runLed ("-s" : args) "g/Cat/p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Cat\n"

    it "smart replace Cyrillic phrase preserves case in global" $ withTempInput "Техническое обслуживание\nтехническое обслуживание\n" $ \args -> do
      -- g/техническое обслуживание/s//техобслуживание/g should use smart replace
      (code, out, _) <- runLed ("-s" : args) "g/техническое обслуживание/s//техобслуживание/g\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Title case: "Техническое обслуживание" -> "Техобслуживание"
      -- Lowercase: "техническое обслуживание" -> "техобслуживание"
      out `shouldBe` "Техобслуживание\nтехобслуживание\n"

    it "v/lowercase/ uses smart search (case-insensitive)" $ withTempInput "Cat\nDog\ncat\n" $ \args -> do
      -- v/cat/ with all-lowercase pattern should NOT match "Cat" and "cat" (inverse)
      (code, out, _) <- runLed ("-s" : args) "v/cat/p\nQ\n"
      code `shouldBe` ExitSuccess
      -- Should only print "Dog" (the only line NOT matching "cat" case-insensitively)
      out `shouldBe` "Dog\n"

    it "v/Uppercase/ uses case-sensitive search" $ withTempInput "Cat\nDog\ncat\n" $ \args -> do
      -- v/Cat/ with uppercase should match lines that don't contain "Cat"
      (code, out, _) <- runLed ("-s" : args) "v/Cat/p\nQ\n"
      code `shouldBe` ExitSuccess
      -- "Cat" matches, so print "Dog" and "cat"
      out `shouldBe` "Dog\ncat\n"

    it "g/lowercase/d deletes matching lines case-insensitively" $ withTempInput "Cat\nDog\ncat\n" $ \args -> do
      -- g/cat/d should delete both "Cat" and "cat"
      (code, out, _) <- runLed ("-s" : args) "g/cat/d\n,p\nQ\n"
      code `shouldBe` ExitSuccess
      out `shouldBe` "Dog\n"
