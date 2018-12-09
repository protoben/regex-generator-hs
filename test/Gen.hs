{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (evaluate)
import System.Random
import Test.Hspec
import Test.HUnit.Lang
import Control.Monad (when)
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as S

import Text.Regex.Regen

shouldGenOpts :: StdGen -> Options -> S.ByteString -> Expectation
shouldGenOpts gen o reg = do
        p <- parsePatternOpts o reg
        case let (e,_) = generatePattern p gen in e of
            Left e   -> assertFailure (eThrown e)
            Right bs -> do
                bs' <- evaluate $ bs =~ reg
                when (bs /= bs') $ assertFailure (eUnexp bs bs')
    where
    eThrown e     = "`generate` threw exception on " ++ show reg ++ ": " ++ show e
    eUnexp bs bs' = show reg ++ " generated " ++ show bs ++ " but matched " ++ show bs'

main :: IO ()
main = do
    seed <- randomIO
    let gen       = mkStdGen seed
        shouldGen = shouldGenOpts gen defaultOptions
    hspec $ test seed shouldGen

test :: Int -> (S.ByteString -> Expectation) -> Spec
test seed f = describe ("generate (seed = " ++ show seed ++ ")") $ mapM_ ($f)
    [ literals
    , classes
    , subpatterns
    , quantifiers
    , anchors
    , globalOpts
    , localOpts
    , verbs
    , callouts
    , lookarounds
    , calls
    ]

literals :: (S.ByteString -> Expectation) -> Spec
literals shouldGen = describe "handles literals and escapes" $ do
    it "accepts a literal string"   $ shouldGen "foo"
    it "accepts an escaped literal" $ shouldGen "\\F\\H\\8\\9"
    it "accepts hex escapes"        $ shouldGen "\\x0\\x61\\x{62}\\x7f"
    it "accepts octal escapes"      $ shouldGen "\\0\\o{141}\\177"
    it "accepts whitespace escape"  $ shouldGen "\\R"
    it "accepts C escapes"          $ shouldGen "\\a\\n\\f\\t"
    it "accepts escaped specials"   $ shouldGen "\\[\\(\\|\\$\\^\\.\\*"
    it "rejects '\\C'" $
        generate "\\C" `shouldThrow` isENotSupported
    it "rejects '\\X'" $
        generate "\\X" `shouldThrow` isENotSupported
    it "rejects '\\L'" $
        generate "\\L" `shouldThrow` isENotSupported
    it "rejects '\\l'" $
        generate "\\l" `shouldThrow` isENotSupported
    it "rejects '\\U'" $
        generate "\\U" `shouldThrow` isENotSupported
    it "rejects '\\u'" $
        generate "\\u" `shouldThrow` isENotSupported
    it "rejects '\\Q'" $
        generate "\\Q" `shouldThrow` isENotSupported
    it "rejects '\\E'" $
        generate "\\E" `shouldThrow` isENotSupported
    it "rejects '\\o'" $
        generate "\\o" `shouldThrow` isEParserError
    it "rejects '\\cx'" $
        generate "\\c" `shouldThrow` isENotSupported

classes :: (S.ByteString -> Expectation) -> Spec
classes shouldGen = describe "handles character classes" $ do
    it "accepts '.'"                    $ shouldGen "."
    it "accepts character class"        $ shouldGen "[asdf]"
    it "accepts negated class"          $ shouldGen "[^asdf]"
    it "accepts class with '-'"         $ shouldGen "[-]"
    it "accepts negated class with '-'" $ shouldGen "[^-]"
    it "accepts class with ']'"         $ shouldGen "[]]"
    it "accepts negated class with ']'" $ shouldGen "[^]]"
    it "accepts class range"            $ shouldGen "[a-c]"
    it "accepts multiple-range class"   $ shouldGen "[a-cd-f]"
    it "accepts negated class range"    $ shouldGen "[^A-z]"
    it "accepts canned range in class"  $ shouldGen "[\\d]"
    it "accepts negated canned range"   $ shouldGen "[^\\d]"
    it "accepts posix class"            $ shouldGen "[[:alpha:]]"
    it "accepts negated posix class"    $ shouldGen "[^[:alpha:]]"
    it "accepts class with everything"  $ shouldGen "[]\\wxa-b[:digit:]-]"
    it "accepts partial posix class"    $ shouldGen "[[:f]"
    it "accepts class with repeats"     $ shouldGen "[ooo]"
    it "accepts class with escapes"     $ shouldGen "[\\x61\\141\\0\\n\\A]"
    it "accepts canned classes"         $ shouldGen "\\d\\D\\w\\W"
    it "rejects malformed class ranges" $
        generate "[z-a]" `shouldThrow` isEClassOutOfOrder
    it "rejects good property ranges" $
        generate "\\p{Ll}" `shouldThrow` isENotSupported
    it "rejects bad property ranges" $
        generate "\\p{bad}" `shouldThrow` isENotSupported
    it "rejects unicode '\\X'" $
        generate "\\X" `shouldThrow` isENotSupported
    it "rejects unicode '\\C'" $
        generate "\\C" `shouldThrow` isENotSupported
    it "rejects posix range out of class" $
        generate "[:alpha:]" `shouldThrow` isENotSupported
    it "rejects bad posix classes" $
        generate "[[:foo:]]" `shouldThrow` isENoSuchClass

subpatterns :: (S.ByteString -> Expectation) -> Spec
subpatterns shouldGen = describe "handles subpatterns" $ do
    it "accepts numbered groups"               $ shouldGen "(x)\\1"
    it "accepts each numbered ref syntax"      $ shouldGen "(x)\\1\\g1\\g{1}"
    it "accepts nested numbered groups"        $ shouldGen "((x)y)\\1\\2"
    it "accepts named groups"                  $ shouldGen "(?P<foo>x)(?P=foo)"
    it "accepts numbered refs to named groups" $ shouldGen "(?P<foo>x)\\1"
    it "accepts non-capturing groups"          $ shouldGen "(?:foo)(x)\\1"
    it "accepts atomic groups"                 $ shouldGen "(?>foo)(x)\\1"
    it "accepts relative group refs" $
        shouldGen "(x)(y)(z)\\g{-2}\\g{-1}\\g{-3}"
    it "accepts nested named groups" $
        shouldGen "(?P<xy>(?P<x>x)y)(?P=xy)(?P=x)"
    it "accepts each named group syntax" $
        shouldGen "(?<x>x)(?'y'y)(?P<z>z)(?P=x)(?P=z)(?P=y)"
    it "accepts each named ref syntax" $
        shouldGen "(?<x>x)\\k'x'\\k<x>\\k{x}\\g{x}(?P=x)"
    it "rejects duplicate ref names" $
        generate "(?<x>x)(?<x>y)" `shouldThrow` isEDupGroupName
    it "rejects refs to non-existent group numbers" $
        generate "\\g1" `shouldThrow` isENoSuchIndex
    it "rejects refs to non-existent group names" $
        generate "(?P=foo)" `shouldThrow` isENoSuchIndex
    it "rejects bad group names" $
        generate "(?P<9foo>)" `shouldThrow` isEBadGroupName
    it "rejects bad group names in references" $
        generate "(?P=9foo)" `shouldThrow` isEBadGroupName
    it "rejects reset groups" $
        generate "(?|foo)" `shouldThrow` isENotSupported

quantifiers :: (S.ByteString -> Expectation) -> Spec
quantifiers shouldGen = describe "handles quantifiers" $ do
    it "accepts fixed quantifiers"         $ shouldGen "a{5}"
    it "accepts bounded quantifiers"       $ shouldGen "a{1,3}"
    it "accepts upper-bounded quantifiers" $ shouldGen "a{,3}"
    it "accepts lower-bounded quantifiers" $ shouldGen "a{3,}"
    it "accepts star quantifer"            $ shouldGen "a*"
    it "accepts plus quantifer"            $ shouldGen "a+"
    it "accepts question-mark quantifier"  $ shouldGen "a?"
    it "accepts quantified char classes"   $ shouldGen "[asdf]{1,3}"
    it "accepts quantified C escapes"      $ shouldGen "\\a{1,3}"
    it "accepts quantified canned classes" $ shouldGen "\\d{1,3}"
    it "accepts quantified hex escapes"    $ shouldGen "\\x61{1,3}"
    it "accepts quantified octal escapes"  $ shouldGen "\\141{1,3}"
    it "accepts quantified dot"            $ shouldGen ".{1,3}"
    it "accepts quantified groups"         $ shouldGen "(asdf){1,3}"
    it "rejects lazy quantifiers" $
        generate "a{1,3}?a" `shouldThrow` isENotSupported
    it "rejects possessive quantifiers" $
        generate "a{1,3}+a" `shouldThrow` isENotSupported
    it "rejects out-of-order ranges in quantifiers" $
        generate "a{3,1}" `shouldThrow` isEQuantOutOfOrder
    it "rejects naked quantifiers" $
        generate "{1,3}" `shouldThrow` isEParserError
    it "rejects quantifier after quantifier" $
        generate "a{1,3}{1,3}" `shouldThrow` isEParserError
    it "rejects quantifier after anchor" $
        generate "^{1,3}" `shouldThrow` isEParserError

anchors :: (S.ByteString -> Expectation) -> Spec
anchors shouldGen = describe "handles anchors" $ do
    describe "carat" $ do
        it "works at start of pattern" $ shouldGen "^foo"
        it "works in alternation"      $ shouldGen "aa|^zz"
        it "fails within pattern" $
            generate "a^" `shouldThrow` isEGenError
    describe "dollar" $ do
        it "works at end of pattern" $ shouldGen "foo$"
        it "works in alternation"    $ shouldGen "aa$|zz"
        it "works before newlines"   $ shouldGen "foo$\\n\\n"
        it "fails within pattern" $
            generate "$a" `shouldThrow` isEGenError
    describe "\\A" $ do
        it "works at start of pattern" $ shouldGen "\\Afoo"
        it "works in alteration"       $ shouldGen "aa|\\Azz"
        it "fails within pattern" $
            generate "a\\A" `shouldThrow` isEGenError
    describe "\\Z" $ do
        it "works at end of pattern" $ shouldGen "foo\\Z"
        it "works in alternation"    $ shouldGen "aa\\Z|zz"
        it "fails within pattern" $
            generate "\\Za" `shouldThrow` isEGenError
    describe "\\z" $ do
        it "works at end of pattern" $ shouldGen "foo\\z"
        it "works in alternation"    $ shouldGen "aa\\z|zz"
        it "fails within pattern" $
            generate "\\za" `shouldThrow` isEGenError
    describe "\\b" $ do
        it "works before space"        $ shouldGen "foo\\b bar"
        it "works after space"         $ shouldGen "foo \\bbar"
        it "works at start of pattern" $ shouldGen "\\baa"
        it "works at end of pattern"   $ shouldGen "zz\\b"
        it "fails within word" $
            generate "foo\\bbar" `shouldThrow` isEGenError
    describe "\\B" $ do
        it "works within word"         $ shouldGen "foo\\Bbar"
        it "fails before space" $
            generate "foo\\B bar" `shouldThrow` isEGenError
        it "fails after space" $
            generate "foo \\Bbar" `shouldThrow` isEGenError
        it "fails at start of pattern" $
            generate "\\Baa" `shouldThrow` isEGenError
        it "fails at end of pattern" $
            generate "zz\\B" `shouldThrow` isEGenError
    describe "\\G" $ do
        it "is unsupported" $
            generate "foo\\G." `shouldThrow` isENotSupported

globalOpts :: (S.ByteString -> Expectation) -> Spec
globalOpts shouldGen = describe "global options" $ do
    it "ignores 'LIMIT_MATCH='"     $ shouldGen "(*LIMIT_MATCH=10)foo"
    it "ignores 'LIMIT_RECURSION='" $ shouldGen "(*LIMIT_RECURSION=10)foo"
    it "ignores 'NO_AUTO_POSSESS'"  $ shouldGen "(*NO_AUTO_POSSESS)foo"
    it "ignores 'NO_START_OPT'"     $ shouldGen "(*NO_START_OPT)foo"
    it "ignores 'BSR_ANYCRLF'"      $ shouldGen "(*BSR_ANYCRLF)foo"
    it "allows 'UTF8'"              $ shouldGen "(*UTF8)foo"
    it "allows 'LF'"                $ shouldGen "(*LF)foo"
    it "allows 'CR'"                $ shouldGen "(*CR)foo"
    it "allows 'ANY'"               $ shouldGen "(*ANY)foo"
    it "allows 'ANYCRLF'"           $ shouldGen "(*ANYCRLF)foo"
    it "rejects 'CRLF'" $
        generate "(*CRLF)foo" `shouldThrow` isENotSupported
    it "rejects 'BSR_UNICODE'" $
        generate "(*BSR_UNICODE)foo" `shouldThrow` isENotSupported
    it "rejects 'UTF16'" $
        generate "(*UTF16)foo" `shouldThrow` isENotSupported
    it "rejects 'UTF32'" $
        generate "(*UTF32)foo" `shouldThrow` isENotSupported
    it "rejects 'UTF'" $
        generate "(*UTF)foo" `shouldThrow` isENotSupported
    it "rejects 'UCP'" $
        generate "(*UCP)foo" `shouldThrow` isENotSupported
    it "rejects bad global options" $
        generate "(*BLAH)foo" `shouldThrow` isEParserError

localOpts :: (S.ByteString -> Expectation) -> Spec
localOpts shouldGen = describe "local options" $ do
    it "allows setting ignore-case"     $ shouldGen "(?i)asdf[asdf]{5}"
    it "allows unsetting ignore-case"   $ shouldGen "(?i)asdf(?-i)asdf[asdf]{5}"
    it "allows unsetting multiline"     $ shouldGen "(?-m)foo"
    it "allows unsetting dup-groups"    $ shouldGen "(?-J)foo"
    it "allows setting dotall"          $ shouldGen "(?s).{100}"
    it "allows unsetting dotall"        $ shouldGen "(?s).{100}(?-s).{100}"
    it "allows unsetting ungreedy"      $ shouldGen "(?-U)foo"
    it "allows setting free-spacing"    $ shouldGen "(?x)a s\\td\\nf(?#x)g"
    it "allows unsetting free-spacing"  $ shouldGen "(?x)a s(?-x)a s\\td\\nf(?#x)g"
    it "allows setting multi options"   $ shouldGen "(?ix)a s\\td\\nf(?#x)g"
    it "allows unsetting multi options" $ shouldGen "(?ix)(?-ix)a s\\td\\nf(?#x)g"
    it "allows setting and unsetting"   $ shouldGen "(?i)(?x-i)a s\\td\\nf(?#x)g"
    it "rejects setting dup-groups" $
        generate "(?J)foo" `shouldThrow` isENotSupported
    it "rejects setting ungreedy" $
        generate "(?U)foo" `shouldThrow` isENotSupported

verbs :: (S.ByteString -> Expectation) -> Spec
verbs _ = describe "backtracking verbs" $ do
    it "rejects 'ACCEPT' verb" $
        generate "foo(*ACCEPT)bar" `shouldThrow` isENotSupported
    it "rejects 'FAIL' verb" $
        generate "foo(*FAIL)bar" `shouldThrow` isENotSupported
    it "rejects 'F' verb" $
        generate "foo(*F)bar" `shouldThrow` isENotSupported
    it "rejects 'COMMIT' verb" $
        generate "foo(*COMMIT)bar" `shouldThrow` isENotSupported
    it "rejects 'PRUNE' verb" $
        generate "foo(*PRUNE)bar" `shouldThrow` isENotSupported
    it "rejects 'SKIP' verb" $
        generate "foo(*SKIP)bar" `shouldThrow` isENotSupported
    it "rejects 'MARK:' verb" $
        generate "foo(*MARK:blah)bar" `shouldThrow` isENotSupported
    it "rejects ':' verb" $
        generate "foo(*:blah)bar" `shouldThrow` isENotSupported
    it "rejects 'COMMIT:' verb" $
        generate "foo(*COMMIT:blah)bar" `shouldThrow` isENotSupported
    it "rejects 'PRUNE:' verb" $
        generate "foo(*PRUNE:blah)bar" `shouldThrow` isENotSupported
    it "rejects 'SKIP:' verb" $
        generate "foo(*SKIP:blah)bar" `shouldThrow` isENotSupported

callouts :: (S.ByteString -> Expectation) -> Spec
callouts _ = describe "callouts" $ do
    it "rejects callouts" $
        generate "(?C)" `shouldThrow` isENotSupported
    it "rejects callouts with arguments" $
        generate "(?C42)" `shouldThrow` isENotSupported

lookarounds :: (S.ByteString -> Expectation) -> Spec
lookarounds _ = describe "lookarounds" $ do
    it "rejects positive lookahead" $
        generate "(?=blah)" `shouldThrow` isENotSupported
    it "rejects negative lookahead" $
        generate "(?!blah)" `shouldThrow` isENotSupported
    it "rejects positive lookbehind" $
        generate "(?<=blah)" `shouldThrow` isENotSupported
    it "rejects negative lookbehind" $
        generate "(?<!blah)" `shouldThrow` isENotSupported

calls :: (S.ByteString -> Expectation) -> Spec
calls _ = describe "pattern calls" $ do
    it "rejects global recursive calls" $
        generate "(?R)" `shouldThrow` isENotSupported
    it "rejects absolutely numbered calls with (?n)" $
        generate "(...)(?1)" `shouldThrow` isENotSupported
    it "rejects relatively numbered calls with (?-n)" $
        generate "(...)(?-1)" `shouldThrow` isENotSupported
    it "rejects relatively numbered calls with (?+n)" $
        generate "(x)(?+1)(...)" `shouldThrow` isENotSupported
    it "rejects absolutely numbered calls with \\g'n'" $
        generate "(...)\\g'1'" `shouldThrow` isENotSupported
    it "rejects relatively numbered calls with \\g'-n'" $
        generate "(...)\\g'-1'" `shouldThrow` isENotSupported
    it "rejects relatively numbered calls with \\g'+n'" $
        generate "(x)\\g'+1'(...)" `shouldThrow` isENotSupported
    it "rejects absolutely numbered calls with \\g<n>" $
        generate "(...)\\g<1>" `shouldThrow` isENotSupported
    it "rejects relatively numbered calls with \\g<-n>" $
        generate "(...)\\g<-1>" `shouldThrow` isENotSupported
    it "rejects relatively numbered calls with \\g<+n>" $
        generate "(x)\\g<+1>(...)" `shouldThrow` isENotSupported
    it "rejects named calls with (?&)" $
        generate "(?<x>...)(?&x)" `shouldThrow` isENotSupported
    it "rejects named calls with (?P>)" $
        generate "(?<x>...)(?P>x)" `shouldThrow` isENotSupported
    it "rejects named calls with \\g''" $
        generate "(?<x>...)\\g'x'" `shouldThrow` isENotSupported
    it "rejects named calls with \\g<>" $
        generate "(?<x>...)\\g<x>" `shouldThrow` isENotSupported
