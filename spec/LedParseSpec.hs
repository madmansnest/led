module LedParseSpec where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Text as T

import LedParse

spec :: Spec
spec = describe "LedParse" $ do
  describe "Address parsing" $ do
    it "parses current line (.)" $ do
      parseCommand Set.empty ".p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle Current)) PrintSuffix)

    it "parses last line ($)" $ do
      parseCommand Set.empty "$p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle LastLine)) PrintSuffix)

    it "parses line number" $ do
      parseCommand Set.empty "5p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle (Number 5))) PrintSuffix)

    it "parses mark ('a)" $ do
      parseCommand Set.empty "'ap" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle (Mark 'a'))) PrintSuffix)

    it "parses forward search" $ do
      parseCommand Set.empty "/foo/p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle (Next "foo"))) PrintSuffix)

    it "parses backward search" $ do
      parseCommand Set.empty "?bar?p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle (Prev "bar"))) PrintSuffix)

    it "parses positive offset" $ do
      parseCommand Set.empty "+3p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle (AddrOffset Current 3))) PrintSuffix)

    it "parses negative offset" $ do
      parseCommand Set.empty "-2p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineSingle (AddrOffset Current (-2)))) PrintSuffix)

  describe "Line range parsing" $ do
    it "parses comma range" $ do
      parseCommand Set.empty "1,5p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineFree (Number 1) (Number 5))) PrintSuffix)

    it "parses semicolon range" $ do
      parseCommand Set.empty "1;5p" `shouldBe`
        Complete (PrintLines (FullRange DocDefault (LineBound (Number 1) (Number 5))) PrintSuffix)

  describe "Document range parsing" $ do
    it "parses & prefix" $ do
      parseCommand Set.empty "&p" `shouldBe`
        Complete (PrintLines (FullRange DocAll LineDefault) PrintSuffix)

    it "parses && prefix" $ do
      parseCommand Set.empty "&&p" `shouldBe`
        Complete (PrintLines (FullRange DocManage LineDefault) PrintSuffix)

    it "parses &: prefix (optional colon)" $ do
      parseCommand Set.empty "&:p" `shouldBe`
        Complete (PrintLines (FullRange DocAll LineDefault) PrintSuffix)

    it "parses &:1,2p (optional colon with line range)" $ do
      parseCommand Set.empty "&:1,2p" `shouldBe`
        Complete (PrintLines (FullRange DocAll (LineFree (Number 1) (Number 2))) PrintSuffix)

    it "parses &&: prefix (optional colon)" $ do
      parseCommand Set.empty "&&:p" `shouldBe`
        Complete (PrintLines (FullRange DocManage LineDefault) PrintSuffix)

    it "parses &*: prefix (optional colon)" $ do
      parseCommand Set.empty "&*:p" `shouldBe`
        Complete (PrintLines (FullRange DocModified LineDefault) PrintSuffix)

    it "parses @ prefix" $ do
      parseCommand Set.empty "@p" `shouldBe`
        Complete (PrintLines (FullRange DocParam LineDefault) PrintSuffix)

    it "parses @: prefix (optional colon)" $ do
      parseCommand Set.empty "@:p" `shouldBe`
        Complete (PrintLines (FullRange DocParam LineDefault) PrintSuffix)

    it "parses @:1,2p (optional colon with line range)" $ do
      parseCommand Set.empty "@:1,2p" `shouldBe`
        Complete (PrintLines (FullRange DocParam (LineFree (Number 1) (Number 2))) PrintSuffix)

    it "parses @!cmd (shell filter with @ range)" $ do
      parseCommand Set.empty "@!echo test" `shouldBe`
        Complete (ShellFilter (FullRange DocParam LineDefault) "echo test")

    it "parses @:!cmd (shell filter with @: range)" $ do
      parseCommand Set.empty "@:!echo test" `shouldBe`
        Complete (ShellFilter (FullRange DocParam LineDefault) "echo test")

    it "parses @1,2!cmd (shell filter with @ and line range)" $ do
      parseCommand Set.empty "@1,2!echo test" `shouldBe`
        Complete (ShellFilter (FullRange DocParam (LineFree (Number 1) (Number 2))) "echo test")

    it "parses n: prefix" $ do
      parseCommand Set.empty "2:p" `shouldBe`
        Complete (PrintLines (FullRange (DocSingle (Number 2)) LineDefault) PrintSuffix)

    it "parses n,m: prefix" $ do
      parseCommand Set.empty "1,3:p" `shouldBe`
        Complete (PrintLines (FullRange (DocFree (Number 1) (Number 3)) LineDefault) PrintSuffix)

  describe "Unranged commands" $ do
    it "parses q" $ do
      parseCommand Set.empty "q" `shouldBe` Complete Quit

    it "parses Q" $ do
      parseCommand Set.empty "Q" `shouldBe` Complete QuitAlways

    it "parses h" $ do
      parseCommand Set.empty "h" `shouldBe` Complete Help

    it "parses H" $ do
      parseCommand Set.empty "H" `shouldBe` Complete HelpMode

    it "parses P" $ do
      parseCommand Set.empty "P" `shouldBe` Complete (TogglePrompt Nothing)

    it "parses P with text" $ do
      parseCommand Set.empty "P *" `shouldBe` Complete (TogglePrompt (Just "*"))

  describe "Text insertion commands" $ do
    it "parses inline append" $ do
      parseCommand Set.empty "a hello" `shouldBe`
        Complete (Append (FullRange DocDefault LineDefault) "hello" NoSuffix)

    it "parses inline append with suffix" $ do
      parseCommand Set.empty "ap hello" `shouldBe`
        Complete (Append (FullRange DocDefault LineDefault) "hello" PrintSuffix)

    it "parses multiline append with empty body (runner handles input)" $ do
      parseCommand Set.empty "a" `shouldBe`
        Complete (Append (FullRange DocDefault LineDefault) "" NoSuffix)

    it "parses inline insert" $ do
      parseCommand Set.empty "i text" `shouldBe`
        Complete (Insert (FullRange DocDefault LineDefault) "text" NoSuffix)

    it "parses inline change" $ do
      parseCommand Set.empty "c new" `shouldBe`
        Complete (Change (FullRange DocDefault LineDefault) "new" NoSuffix)

  describe "Multiline a/c/i" $ do
    it "collects lines until dot" $ do
      feedLines Set.empty ["a", "line1", "line2", "."] `shouldBe`
        Complete (Append (FullRange DocDefault LineDefault) "line1\nline2" NoSuffix)

    it "a with empty body returns Complete (runner handles input)" $ do
      feedLines Set.empty ["a", "line1", "line2"] `shouldBe`
        Complete (Append (FullRange DocDefault LineDefault) "" NoSuffix)

  describe "Function commands" $ do
    it "parses fn (list)" $ do
      parseCommand Set.empty "fn" `shouldBe` Complete FnList

    it "parses fn name (query)" $ do
      parseCommand Set.empty "fn hello" `shouldBe` Complete (FnQuery "hello")

    it "parses fn name { body }" $ do
      parseCommand Set.empty "fn hello { 1,3p }" `shouldBe`
        Complete (FnDefine "hello" [] "1,3p")

    it "parses fn name params { body }" $ do
      parseCommand Set.empty "fn greet x y { $x $y }" `shouldBe`
        Complete (FnDefine "greet" ["x", "y"] "$x $y")

    it "parses fn/name/ (query with delimiter)" $ do
      parseCommand Set.empty "fn/hello/" `shouldBe` Complete (FnQuery "hello")

    it "parses fn/name (query without trailing delimiter)" $ do
      parseCommand Set.empty "fn/hello" `shouldBe` Complete (FnQuery "hello")

    it "parses fn/name/body (definition with delimiter)" $ do
      parseCommand Set.empty "fn/hello/1,3p" `shouldBe`
        Complete (FnDefine "hello" [] "1,3p")

    it "parses fn#name#body (definition with # delimiter)" $ do
      parseCommand Set.empty "fn#wrap#s/^/[/" `shouldBe`
        Complete (FnDefine "wrap" [] "s/^/[/")

    it "parses fn/name/multiline with backslash continuation" $ do
      feedLines Set.empty ["fn/wrap/s/^/[/\\", "s/$/]/"] `shouldBe`
        Complete (FnDefine "wrap" [] "s/^/[/\ns/$/]/")

  describe "Delete command" $ do
    it "parses d" $ do
      parseCommand Set.empty "d" `shouldBe`
        Complete (Delete (FullRange DocDefault LineDefault) NoSuffix)

    it "parses 1,5d" $ do
      parseCommand Set.empty "1,5d" `shouldBe`
        Complete (Delete (FullRange DocDefault (LineFree (Number 1) (Number 5))) NoSuffix)

  describe "Substitute command" $ do
    it "parses s/old/new/" $ do
      parseCommand Set.empty "s/old/new/" `shouldBe`
        Complete (Substitute (FullRange DocDefault LineDefault) "old" "new" (SubstFlags False 0 False) NoSuffix)

    it "parses s/old/new/g" $ do
      parseCommand Set.empty "s/old/new/g" `shouldBe`
        Complete (Substitute (FullRange DocDefault LineDefault) "old" "new" (SubstFlags True 0 False) NoSuffix)

    it "parses s/old/new/3" $ do
      parseCommand Set.empty "s/old/new/3" `shouldBe`
        Complete (Substitute (FullRange DocDefault LineDefault) "old" "new" (SubstFlags False 3 False) NoSuffix)

    it "parses s/old/new/gp" $ do
      parseCommand Set.empty "s/old/new/gp" `shouldBe`
        Complete (Substitute (FullRange DocDefault LineDefault) "old" "new" (SubstFlags True 0 False) PrintSuffix)

    it "parses s/old/new/i (insensitive flag)" $ do
      parseCommand Set.empty "s/old/new/i" `shouldBe`
        Complete (Substitute (FullRange DocDefault LineDefault) "old" "new" (SubstFlags False 0 True) NoSuffix)

    it "parses s/old/new/gi (global + insensitive)" $ do
      parseCommand Set.empty "s/old/new/gi" `shouldBe`
        Complete (Substitute (FullRange DocDefault LineDefault) "old" "new" (SubstFlags True 0 True) NoSuffix)

  describe "Global commands" $ do
    it "parses g/re/p" $ do
      parseCommand Set.empty "g/foo/p" `shouldBe`
        Complete (Global (FullRange DocDefault LineDefault) "foo" "p")

    it "parses g/re/ with default cmdlist" $ do
      parseCommand Set.empty "g/foo/" `shouldBe`
        Complete (Global (FullRange DocDefault LineDefault) "foo" "p")

    it "parses v/re/d" $ do
      parseCommand Set.empty "v/bar/d" `shouldBe`
        Complete (GlobalReverse (FullRange DocDefault LineDefault) "bar" "d")

  describe "Move/Transfer commands" $ do
    it "parses m with local target" $ do
      parseCommand Set.empty "1,3m5" `shouldBe`
        Complete (Move (FullRange DocDefault (LineFree (Number 1) (Number 3)))
                       (LocalTarget (Number 5)) NoSuffix)

    it "parses t with local target" $ do
      parseCommand Set.empty "1,3t$" `shouldBe`
        Complete (Transfer (FullRange DocDefault (LineFree (Number 1) (Number 3)))
                          (LocalTarget LastLine) NoSuffix)

  describe "File commands" $ do
    it "parses e" $ do
      parseCommand Set.empty "e" `shouldBe`
        Complete (Edit (FullRange DocDefault LineDefault) Nothing)

    it "parses e file.txt" $ do
      parseCommand Set.empty "e file.txt" `shouldBe`
        Complete (Edit (FullRange DocDefault LineDefault) (Just "file.txt"))

    it "parses w" $ do
      parseCommand Set.empty "w" `shouldBe`
        Complete (Write (FullRange DocDefault LineDefault) Nothing)

    it "parses w file.txt" $ do
      parseCommand Set.empty "w file.txt" `shouldBe`
        Complete (Write (FullRange DocDefault LineDefault) (Just "file.txt"))

    it "parses wq" $ do
      parseCommand Set.empty "wq" `shouldBe`
        Complete (WriteQuit (FullRange DocDefault LineDefault) Nothing)

  describe "Shell command" $ do
    it "parses !ls" $ do
      parseCommand Set.empty "!ls -la" `shouldBe`
        Complete (ShellCommand "ls -la")

  describe "Comment" $ do
    it "parses # comment" $ do
      parseCommand Set.empty "# this is a comment" `shouldBe` Complete Comment

  describe "User function invocation" $ do
    it "parses user function with args" $ do
      let fns = Set.fromList ["hello"]
      parseCommand fns "hello world" `shouldBe`
        Complete (InvokeFunction "hello" (FullRange DocDefault LineDefault) ["world"] NoSuffix)

    it "parses user function with range" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "1,5myfn" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault (LineFree (Number 1) (Number 5))) [] NoSuffix)

    it "parses user function with delimited params" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/a/b/c" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["a", "b", "c"] NoSuffix)

    it "parses user function with delimited params and trailing delimiter" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/a/b/c/" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["a", "b", "c"] NoSuffix)

    it "parses user function with delimited params and suffix" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/a/b/cp" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["a", "b", "c"] PrintSuffix)

    it "parses user function with delimited params, trailing delimiter and suffix" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/a/b/c/p" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["a", "b", "c"] PrintSuffix)

    it "parses user function with # delimiter" $ do
      let fns = Set.fromList ["wrap"]
      parseCommand fns "wrap#hello#world" `shouldBe`
        Complete (InvokeFunction "wrap" (FullRange DocDefault LineDefault) ["hello", "world"] NoSuffix)

    it "parses user function with range and delimited params" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "1,5myfn/x/y" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault (LineFree (Number 1) (Number 5))) ["x", "y"] NoSuffix)

    it "parses user function with whitespace in delimited params" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/hello world/foo bar" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["hello world", "foo bar"] NoSuffix)

    it "parses user function with whitespace in params and suffix" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/hello world/foo barp" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["hello world", "foo bar"] PrintSuffix)

    it "parses user function with whitespace in params and trailing delimiter" $ do
      let fns = Set.fromList ["myfn"]
      parseCommand fns "myfn/hello world/foo bar/" `shouldBe`
        Complete (InvokeFunction "myfn" (FullRange DocDefault LineDefault) ["hello world", "foo bar"] NoSuffix)

  describe "hasDocRange" $ do
    it "returns False for Quit" $ do
      hasDocRange Quit `shouldBe` False

    it "returns False for Help" $ do
      hasDocRange Help `shouldBe` False

    it "returns True for Delete" $ do
      hasDocRange (Delete (FullRange DocDefault LineDefault) NoSuffix) `shouldBe` True

    it "returns True for PrintLines" $ do
      hasDocRange (PrintLines (FullRange DocDefault LineDefault) PrintSuffix) `shouldBe` True
