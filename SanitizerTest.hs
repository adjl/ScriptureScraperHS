import Test.HUnit
import Sanitizer

testOpeningQuotes :: Test
testOpeningQuotes =
    TestCase $ assertEqual "Replace Unicode opening double quotes"
        "``Vanity"
        (sanitize "\128Vanity")

-- testPunctuationSpace :: Test
-- testPunctuationSpace =
--     TestCase $ assertEqual "Put space after punctuation"
--         "Jerusalem. ``"
--         sanitize "Jerusalem.``"

tests :: Test
tests = TestList [TestLabel "testOpeningQuotes" testOpeningQuotes]
