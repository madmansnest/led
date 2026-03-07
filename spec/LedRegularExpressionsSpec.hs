module LedRegularExpressionsSpec where

import Test.Hspec

import qualified Data.IntMap.Strict as IM

import LedRegularExpressions (BRE(..), AnchorType(..), ClassItem(..), RepeatKind(..), Match(..), parseBRE, matchBRE, matchesBRE, matchAllBRE, buildReplacement)

spec :: Spec
spec = describe "LedRegularExpressions" $ do

  -- -----------------------------------------------------------------------
  -- parseBRE
  -- -----------------------------------------------------------------------
  describe "parseBRE" $ do
    describe "literals" $ do
      it "parses a single literal" $
        parseBRE "a" `shouldBe` Right (Literal 'a')

      it "parses multiple literals as Concat" $
        parseBRE "abc" `shouldBe` Right (Concat [Literal 'a', Literal 'b', Literal 'c'])

      it "parses escaped special characters as literals" $
        parseBRE "\\." `shouldBe` Right (Literal '.')

      it "parses escaped backslash" $
        parseBRE "\\\\" `shouldBe` Right (Literal '\\')

      it "parses escaped star as literal" $
        parseBRE "\\*" `shouldBe` Right (Literal '*')

      it "parses escaped dollar as literal" $
        parseBRE "\\$" `shouldBe` Right (Literal '$')

      it "parses escaped caret as literal" $
        parseBRE "\\^" `shouldBe` Right (Literal '^')

      it "parses escaped bracket as literal" $
        parseBRE "\\[" `shouldBe` Right (Literal '[')

    describe "any char (.)" $ do
      it "parses dot as AnyChar" $
        parseBRE "." `shouldBe` Right AnyChar

      it "parses dot in sequence" $
        parseBRE "a.b" `shouldBe` Right (Concat [Literal 'a', AnyChar, Literal 'b'])

    describe "anchors" $ do
      it "parses ^ as StartAnchor" $
        parseBRE "^a" `shouldBe` Right (Concat [Anchor StartAnchor, Literal 'a'])

      it "parses $ as EndAnchor" $
        parseBRE "a$" `shouldBe` Right (Concat [Literal 'a', Anchor EndAnchor])

      it "parses ^$ as both anchors" $
        parseBRE "^$" `shouldBe` Right (Concat [Anchor StartAnchor, Anchor EndAnchor])

      it "parses ^abc$ as anchored literal" $
        parseBRE "^abc$" `shouldBe` Right (Concat [Anchor StartAnchor, Literal 'a', Literal 'b', Literal 'c', Anchor EndAnchor])

    describe "star repetition" $ do
      it "parses a* as Repeat" $
        parseBRE "a*" `shouldBe` Right (Repeat (Literal 'a') Star)

      it "parses .* as repeat any" $
        parseBRE ".*" `shouldBe` Right (Repeat AnyChar Star)

      it "leading * is literal" $
        parseBRE "*" `shouldBe` Right (Literal '*')

      it "leading * followed by more is literal then rest" $
        parseBRE "*a" `shouldBe` Right (Concat [Literal '*', Literal 'a'])

    describe "intervals" $ do
      it "parses exact interval \\{3\\}" $
        parseBRE "a\\{3\\}" `shouldBe` Right (Repeat (Literal 'a') (Interval 3 (Just 3)))

      it "parses range interval \\{2,5\\}" $
        parseBRE "a\\{2,5\\}" `shouldBe` Right (Repeat (Literal 'a') (Interval 2 (Just 5)))

      it "parses open-ended interval \\{2,\\}" $
        parseBRE "a\\{2,\\}" `shouldBe` Right (Repeat (Literal 'a') (Interval 2 Nothing))

      it "rejects interval where max < min" $
        parseBRE "a\\{5,2\\}" `shouldSatisfy` isLeft

      it "rejects malformed interval" $
        parseBRE "a\\{\\}" `shouldSatisfy` isLeft

    describe "bracket expressions" $ do
      it "parses simple character class [abc]" $
        parseBRE "[abc]" `shouldBe` Right (CharClass False [SingleChar 'a', SingleChar 'b', SingleChar 'c'])

      it "parses negated class [^abc]" $
        parseBRE "[^abc]" `shouldBe` Right (CharClass True [SingleChar 'a', SingleChar 'b', SingleChar 'c'])

      it "parses range [a-z]" $
        parseBRE "[a-z]" `shouldBe` Right (CharClass False [CharRange 'a' 'z'])

      it "parses multiple ranges [a-zA-Z]" $
        parseBRE "[a-zA-Z]" `shouldBe` Right (CharClass False [CharRange 'a' 'z', CharRange 'A' 'Z'])

      it "parses ] as first char in class" $
        parseBRE "[]a]" `shouldBe` Right (CharClass False [SingleChar ']', SingleChar 'a'])

      it "parses ] as first char in negated class" $
        parseBRE "[^]a]" `shouldBe` Right (CharClass True [SingleChar ']', SingleChar 'a'])

      it "parses - at end as literal" $ do
        let result = parseBRE "[a-]"
        result `shouldSatisfy` isRight

      it "parses POSIX class [:alpha:]" $
        parseBRE "[[:alpha:]]" `shouldBe` Right (CharClass False [PosixClass "alpha"])

      it "parses POSIX class [:digit:]" $
        parseBRE "[[:digit:]]" `shouldBe` Right (CharClass False [PosixClass "digit"])

      it "parses mixed class with POSIX and range" $
        parseBRE "[[:alpha:]0-9]" `shouldBe` Right (CharClass False [PosixClass "alpha", CharRange '0' '9'])

      it "rejects unclosed bracket" $
        parseBRE "[abc" `shouldSatisfy` isLeft

    describe "groups" $ do
      it "parses simple group \\(a\\)" $
        parseBRE "\\(a\\)" `shouldBe` Right (Group 1 (Literal 'a'))

      it "parses group with multiple atoms" $
        parseBRE "\\(ab\\)" `shouldBe` Right (Group 1 (Concat [Literal 'a', Literal 'b']))

      it "parses nested groups" $
        parseBRE "\\(a\\(b\\)\\)" `shouldBe` Right (Group 1 (Concat [Literal 'a', Group 2 (Literal 'b')]))

      it "numbers groups by opening paren order" $
        parseBRE "\\(a\\)\\(b\\)" `shouldBe` Right (Concat [Group 1 (Literal 'a'), Group 2 (Literal 'b')])

      it "parses group with star" $
        parseBRE "\\(ab\\)*" `shouldBe` Right (Repeat (Group 1 (Concat [Literal 'a', Literal 'b'])) Star)

      it "rejects unclosed group" $
        parseBRE "\\(abc" `shouldSatisfy` isLeft

      it "rejects unmatched close group" $
        parseBRE "\\)" `shouldSatisfy` isLeft

    describe "backreferences" $ do
      it "parses \\1" $
        parseBRE "\\(a\\)\\1" `shouldBe` Right (Concat [Group 1 (Literal 'a'), BackRef 1])

      it "parses \\9" $
        parseBRE "\\9" `shouldBe` Right (BackRef 9)

    describe "error cases" $ do
      it "rejects empty RE" $
        parseBRE "" `shouldSatisfy` isLeft

      it "rejects trailing backslash" $
        parseBRE "\\" `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- matchBRE
  -- -----------------------------------------------------------------------
  describe "matchBRE" $ do
    describe "literal matching" $ do
      it "matches a literal at the start" $
        matchStart <$> matchBRE (Literal 'a') "abc" `shouldBe` Just 0

      it "matches a literal in the middle" $ do
        let Just m = matchBRE (Literal 'b') "abc"
        matchStart m `shouldBe` 1
        matchEnd m `shouldBe` 2

      it "returns Nothing for no match" $
        matchBRE (Literal 'x') "abc" `shouldBe` Nothing

      it "matches multi-char literal sequence" $ do
        let Right bre = parseBRE "bc"
            Just m = matchBRE bre "abcd"
        matchText m `shouldBe` "bc"
        matchStart m `shouldBe` 1

    describe "any char" $ do
      it "matches any character" $ do
        let Just m = matchBRE AnyChar "x"
        matchText m `shouldBe` "x"

      it "does not match empty string" $
        matchBRE AnyChar "" `shouldBe` Nothing

    describe "anchors" $ do
      it "^ anchors match to start" $ do
        let Right bre = parseBRE "^abc"
        matchesBRE bre "abcdef" `shouldBe` True
        matchesBRE bre "xabc" `shouldBe` False

      it "$ anchors match to end" $ do
        let Right bre = parseBRE "abc$"
        matchesBRE bre "xyzabc" `shouldBe` True
        matchesBRE bre "abcx" `shouldBe` False

      it "^$ matches empty string" $ do
        let Right bre = parseBRE "^$"
        matchesBRE bre "" `shouldBe` True
        matchesBRE bre "a" `shouldBe` False

      it "^abc$ matches exact string" $ do
        let Right bre = parseBRE "^abc$"
        matchesBRE bre "abc" `shouldBe` True
        matchesBRE bre "abcd" `shouldBe` False
        matchesBRE bre "xabc" `shouldBe` False

    describe "star repetition" $ do
      it "a* matches zero occurrences" $ do
        let Right bre = parseBRE "a*"
            Just m = matchBRE bre "bbb"
        matchText m `shouldBe` ""
        matchStart m `shouldBe` 0

      it "a* matches multiple occurrences (greedy)" $ do
        let Right bre = parseBRE "a*"
            Just m = matchBRE bre "aaab"
        matchText m `shouldBe` "aaa"

      it ".* matches entire line" $ do
        let Right bre = parseBRE ".*"
            Just m = matchBRE bre "hello world"
        matchText m `shouldBe` "hello world"

      it "a*b matches b with zero a's" $ do
        let Right bre = parseBRE "a*b"
            Just m = matchBRE bre "b"
        matchText m `shouldBe` "b"

      it "a*b matches aab" $ do
        let Right bre = parseBRE "a*b"
            Just m = matchBRE bre "aab"
        matchText m `shouldBe` "aab"

    describe "intervals" $ do
      it "a\\{2\\} matches exactly 2" $ do
        let Right bre = parseBRE "a\\{2\\}"
        matchesBRE bre "aa" `shouldBe` True
        matchesBRE bre "a" `shouldBe` False

      it "a\\{2,4\\} matches 2 to 4" $ do
        let Right bre = parseBRE "a\\{2,4\\}"
            Just m = matchBRE bre "aaaaa"
        matchText m `shouldBe` "aaaa"  -- greedy, takes 4

      it "a\\{2,4\\} does not match 1" $do
        let Right bre = parseBRE "a\\{2,4\\}"
        matchesBRE bre "a" `shouldBe` False

      it "a\\{2,\\} matches 2 or more" $ do
        let Right bre = parseBRE "a\\{2,\\}"
            Just m = matchBRE bre "aaaaa"
        matchText m `shouldBe` "aaaaa"

      it "a\\{0,1\\} matches zero or one" $ do
        let Right bre = parseBRE "a\\{0,1\\}"
            Just m1 = matchBRE bre "abc"
        matchText m1 `shouldBe` "a"
        let Just m2 = matchBRE bre "bbb"
        matchText m2 `shouldBe` ""

    describe "bracket expressions" $ do
      it "matches character in class" $ do
        let Right bre = parseBRE "[abc]"
        matchesBRE bre "b" `shouldBe` True
        matchesBRE bre "x" `shouldBe` False

      it "matches character in range" $ do
        let Right bre = parseBRE "[a-z]"
        matchesBRE bre "m" `shouldBe` True
        matchesBRE bre "M" `shouldBe` False

      it "negated class excludes chars" $ do
        let Right bre = parseBRE "[^abc]"
        matchesBRE bre "a" `shouldBe` False
        matchesBRE bre "x" `shouldBe` True

      it "matches POSIX [:digit:] class" $ do
        let Right bre = parseBRE "[[:digit:]]"
        matchesBRE bre "5" `shouldBe` True
        matchesBRE bre "a" `shouldBe` False

      it "matches POSIX [:alpha:] class" $ do
        let Right bre = parseBRE "[[:alpha:]]"
        matchesBRE bre "a" `shouldBe` True
        matchesBRE bre "5" `shouldBe` False

      it "matches POSIX [:upper:] class" $ do
        let Right bre = parseBRE "[[:upper:]]"
        matchesBRE bre "A" `shouldBe` True
        matchesBRE bre "a" `shouldBe` False

      it "matches POSIX [:lower:] class" $ do
        let Right bre = parseBRE "[[:lower:]]"
        matchesBRE bre "a" `shouldBe` True
        matchesBRE bre "A" `shouldBe` False

      it "matches POSIX [:space:] class" $ do
        let Right bre = parseBRE "[[:space:]]"
        matchesBRE bre " " `shouldBe` True
        matchesBRE bre "a" `shouldBe` False

      it "matches POSIX [:alnum:] class" $ do
        let Right bre = parseBRE "[[:alnum:]]"
        matchesBRE bre "a" `shouldBe` True
        matchesBRE bre "5" `shouldBe` True
        matchesBRE bre "!" `shouldBe` False

      it "matches POSIX [:xdigit:] class" $ do
        let Right bre = parseBRE "[[:xdigit:]]"
        matchesBRE bre "f" `shouldBe` True
        matchesBRE bre "F" `shouldBe` True
        matchesBRE bre "g" `shouldBe` False

      it "matches POSIX [:blank:] class" $ do
        let Right bre = parseBRE "[[:blank:]]"
        matchesBRE bre " " `shouldBe` True
        matchesBRE bre "\t" `shouldBe` True
        matchesBRE bre "a" `shouldBe` False

      it "matches ] as first char in class" $ do
        let Right bre = parseBRE "[]ab]"
        matchesBRE bre "]" `shouldBe` True
        matchesBRE bre "a" `shouldBe` True
        matchesBRE bre "x" `shouldBe` False

      it "bracket with star [a-z]*" $ do
        let Right bre = parseBRE "[a-z]*"
            Just m = matchBRE bre "hello"
        matchText m `shouldBe` "hello"

    describe "groups and captures" $ do
      it "captures group text" $ do
        let Right bre = parseBRE "\\(abc\\)"
            Just m = matchBRE bre "xabcy"
        matchText m `shouldBe` "abc"
        IM.lookup 1 (matchGroups m) `shouldBe` Just "abc"

      it "captures multiple groups" $ do
        let Right bre = parseBRE "\\(a\\)\\(b\\)"
            Just m = matchBRE bre "ab"
        IM.lookup 1 (matchGroups m) `shouldBe` Just "a"
        IM.lookup 2 (matchGroups m) `shouldBe` Just "b"

      it "captures nested groups" $ do
        let Right bre = parseBRE "\\(a\\(b\\)c\\)"
            Just m = matchBRE bre "abc"
        IM.lookup 1 (matchGroups m) `shouldBe` Just "abc"
        IM.lookup 2 (matchGroups m) `shouldBe` Just "b"

      it "captures group with star" $ do
        let Right bre = parseBRE "\\(a*\\)"
            Just m = matchBRE bre "aaa"
        IM.lookup 1 (matchGroups m) `shouldBe` Just "aaa"

    describe "backreferences" $ do
      it "\\1 matches same text as group 1" $ do
        let Right bre = parseBRE "\\(a*\\)b\\1"
            Just m = matchBRE bre "aabaa"
        matchText m `shouldBe` "aabaa"

      it "\\1 fails when text differs" $ do
        let Right bre = parseBRE "\\(a\\)b\\1"
        matchesBRE bre "abb" `shouldBe` False

      it "backreference to empty group matches" $ do
        let Right bre = parseBRE "\\(a*\\)b\\1"
            Just m = matchBRE bre "b"
        matchText m `shouldBe` "b"

    describe "leftmost longest semantics" $ do
      it "finds leftmost match" $ do
        let Right bre = parseBRE "ab"
            Just m = matchBRE bre "xabyab"
        matchStart m `shouldBe` 1

      it "greedy star takes longest" $ do
        let Right bre = parseBRE "a.*b"
            Just m = matchBRE bre "aXbYb"
        matchText m `shouldBe` "aXbYb"

    describe "complex patterns" $ do
      it "matches email-like pattern" $ do
        let Right bre = parseBRE "[a-zA-Z]*@[a-zA-Z]*"
        matchesBRE bre "user@host" `shouldBe` True

      it "matches line with digits" $ do
        let Right bre = parseBRE "^[[:digit:]][[:digit:]]*$"
        matchesBRE bre "12345" `shouldBe` True
        matchesBRE bre "123a5" `shouldBe` False

      it "matches word boundaries with groups" $ do
        let Right bre = parseBRE "\\([a-z][a-z]*\\) \\1"
        matchesBRE bre "the the" `shouldBe` True
        matchesBRE bre "the that" `shouldBe` False

  -- -----------------------------------------------------------------------
  -- matchesBRE
  -- -----------------------------------------------------------------------
  describe "matchesBRE" $ do
    it "returns True for match" $ do
      let Right bre = parseBRE "hello"
      matchesBRE bre "say hello world" `shouldBe` True

    it "returns False for no match" $ do
      let Right bre = parseBRE "hello"
      matchesBRE bre "goodbye" `shouldBe` False

    it "matches empty pattern never (parseBRE rejects it)" $
      parseBRE "" `shouldSatisfy` isLeft

  -- -----------------------------------------------------------------------
  -- Edge cases
  -- -----------------------------------------------------------------------
  describe "edge cases" $ do
    it "matches against empty string with .*" $ do
      let Right bre = parseBRE ".*"
          Just m = matchBRE bre ""
      matchText m `shouldBe` ""

    it "dot does not match empty" $
      matchBRE AnyChar "" `shouldBe` Nothing

    it "escaped special chars match literally" $ do
      let Right bre = parseBRE "a\\.b"
      matchesBRE bre "a.b" `shouldBe` True
      matchesBRE bre "axb" `shouldBe` False

    it "star after bracket expression" $ do
      let Right bre = parseBRE "[0-9]*"
          Just m = matchBRE bre "123abc"
      matchText m `shouldBe` "123"

    it "bracket expression with - at end" $ do
      let Right bre = parseBRE "[a-]"
      matchesBRE bre "a" `shouldBe` True
      matchesBRE bre "-" `shouldBe` True
      matchesBRE bre "b" `shouldBe` False

    it "multiple stars in sequence" $ do
      let Right bre = parseBRE "a*b*c"
      matchesBRE bre "c" `shouldBe` True
      matchesBRE bre "aaabbc" `shouldBe` True

    it "group followed by interval" $ do
      let Right bre = parseBRE "\\(ab\\)\\{2\\}"
      matchesBRE bre "abab" `shouldBe` True
      matchesBRE bre "ab" `shouldBe` False

  -- -----------------------------------------------------------------------
  -- matchAllBRE
  -- -----------------------------------------------------------------------
  describe "matchAllBRE" $ do
    it "finds all non-overlapping matches" $ do
      let Right bre = parseBRE "ab"
      let ms = matchAllBRE bre "ababab"
      length ms `shouldBe` 3
      map matchText ms `shouldBe` ["ab", "ab", "ab"]

    it "returns empty for no match" $ do
      let Right bre = parseBRE "xy"
      matchAllBRE bre "abc" `shouldBe` []

    it "finds single match" $ do
      let Right bre = parseBRE "b."
      let ms = matchAllBRE bre "abcd"
      length ms `shouldBe` 1
      map matchText ms `shouldBe` ["bc"]

    it "handles non-overlapping correctly" $ do
      let Right bre = parseBRE "aa"
      let ms = matchAllBRE bre "aaaa"
      length ms `shouldBe` 2

  -- -----------------------------------------------------------------------
  -- buildReplacement
  -- -----------------------------------------------------------------------
  describe "buildReplacement" $ do
    it "replaces & with whole match" $ do
      let m = Match "foo" 0 3 IM.empty
      buildReplacement "[&]" m `shouldBe` "[foo]"

    it "replaces \\1 with captured group" $ do
      let m = Match "abc" 0 3 (IM.singleton 1 "bc")
      buildReplacement "x\\1y" m `shouldBe` "xbcy"

    it "replaces \\\\ with literal backslash" $ do
      let m = Match "a" 0 1 IM.empty
      buildReplacement "\\\\" m `shouldBe` "\\"

    it "passes literal text through" $ do
      let m = Match "x" 0 1 IM.empty
      buildReplacement "hello" m `shouldBe` "hello"

    it "handles missing group as empty" $ do
      let m = Match "x" 0 1 IM.empty
      buildReplacement "\\1" m `shouldBe` ""

    it "replaces \\& with literal &" $ do
      let m = Match "foo" 0 3 IM.empty
      buildReplacement "\\&" m `shouldBe` "&"

  -- -----------------------------------------------------------------------
  -- Unicode support for POSIX character classes
  -- -----------------------------------------------------------------------
  describe "Unicode POSIX classes" $ do
    it "[:alpha:] matches Cyrillic letters" $ do
      let Right bre = parseBRE "[[:alpha:]]"
      matchesBRE bre "с" `shouldBe` True
      matchesBRE bre "я" `shouldBe` True
      matchesBRE bre "Ж" `shouldBe` True

    it "[:alpha:] matches Greek letters" $ do
      let Right bre = parseBRE "[[:alpha:]]"
      matchesBRE bre "α" `shouldBe` True
      matchesBRE bre "Ω" `shouldBe` True

    it "[:alpha:] matches accented Latin letters" $ do
      let Right bre = parseBRE "[[:alpha:]]"
      matchesBRE bre "é" `shouldBe` True
      matchesBRE bre "ñ" `shouldBe` True
      matchesBRE bre "ü" `shouldBe` True

    it "[:alnum:] matches Unicode letters and digits" $ do
      let Right bre = parseBRE "[[:alnum:]]"
      matchesBRE bre "с" `shouldBe` True
      matchesBRE bre "5" `shouldBe` True
      matchesBRE bre "!" `shouldBe` False

    it "[:upper:] matches Unicode uppercase" $ do
      let Right bre = parseBRE "[[:upper:]]"
      matchesBRE bre "Ж" `shouldBe` True
      matchesBRE bre "Σ" `shouldBe` True
      matchesBRE bre "ж" `shouldBe` False

    it "[:lower:] matches Unicode lowercase" $ do
      let Right bre = parseBRE "[[:lower:]]"
      matchesBRE bre "ж" `shouldBe` True
      matchesBRE bre "σ" `shouldBe` True
      matchesBRE bre "Ж" `shouldBe` False

    it "[:alpha:]* matches full Cyrillic word" $ do
      let Right bre = parseBRE "[[:alpha:]]*"
          Just m = matchBRE bre "секунды"
      matchText m `shouldBe` "секунды"

    it "negated [^[:alpha:]] excludes Unicode letters" $ do
      let Right bre = parseBRE "[^[:alpha:]]"
      matchesBRE bre "с" `shouldBe` False
      matchesBRE bre "5" `shouldBe` True
      matchesBRE bre "!" `shouldBe` True

    it "mixed ASCII and Cyrillic with [:alpha:]" $ do
      let Right bre = parseBRE "^[[:alpha:]][[:alpha:]]*$"
      matchesBRE bre "hello" `shouldBe` True
      matchesBRE bre "привет" `shouldBe` True
      matchesBRE bre "hello123" `shouldBe` False
