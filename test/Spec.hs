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
tests = testGroup "Webby Tests" []

