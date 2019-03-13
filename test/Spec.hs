import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
-- import qualified Test.Tasty.QuickCheck as QC

import           Webby.Server
import           Webby.Types
import           WebbyPrelude

import           Webby.RouteTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Webby Tests" [text2PathSegmentsTest, testHashTrie]

text2PathSegmentsTest :: TestTree
text2PathSegmentsTest = testGroup "text2PathSegments" [text2PathSegmentsUT]
  where
    text2PathSegmentsUT :: TestTree
    text2PathSegmentsUT = HUnit.testCase "Unit tests" $ do
        text2PathSegments "" @?= []

        text2PathSegments "/" @?= []

        text2PathSegments "/ab" @?= [Literal "ab"]

        text2PathSegments "ab" @?= [Literal "ab"]

        text2PathSegments "/ab/bc" @?= [Literal "ab", Literal "bc"]

        text2PathSegments ":ab/bc" @?= [Capture "ab", Literal "bc"]

        text2PathSegments "/:ab/bc" @?= [Capture "ab", Literal "bc"]

        text2PathSegments "/ab/:bc/cd" @?= [Literal "ab", Capture "bc", Literal "cd"]
