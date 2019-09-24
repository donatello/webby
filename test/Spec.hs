import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit

import           Network.Wai.Internal
import qualified Data.HashMap.Strict as H

import           Webby.Server
import           Webby.Types
import           Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Webby Tests" [matchRequestTests]

matchRequestTests :: TestTree
matchRequestTests = testGroup "matchRequest" [unitTests]
  where
    routes1 = []
    routes2 = [ (RoutePattern methodGet ["api"], 1::Int)
              , (RoutePattern methodPut ["api"], 2)
              , (RoutePattern methodGet ["api", "v1"], 3)
              ]
    routes3 = [ (RoutePattern methodGet ["api", "vX"], 1)
              , (RoutePattern methodGet ["api", ":version"], 2)
              , (RoutePattern methodGet ["api", "vX", "aaa"], 3)
              ]
    routes4 = [ (RoutePattern methodGet ["api", ":version"], 1)
              , (RoutePattern methodGet [":api", "version"], 2)]
    r mthd path = defaultRequest { pathInfo = path
                                 , requestMethod = mthd
                                 }

    unitTests :: TestTree
    unitTests = HUnit.testCase "Unit tests" $ do
        let cases = [ (methodGet, ["api"], routes1, Nothing)

                    , (methodGet, ["api"], routes2, Just (H.empty, 1))
                    , (methodPut, ["api"], routes2, Just (H.empty, 2))
                    , (methodPost, ["api"], routes2, Nothing)
                    , (methodGet, ["api", "v1"], routes2, Just (H.empty, 3))
                    , (methodPut, ["api", "ab"], routes2, Nothing)
                    , (methodPut, ["api", ""], routes2, Just (H.empty, 2))

                    , (methodGet, ["api"], routes3, Nothing)
                    , (methodGet, ["api", "vX"], routes3, Just (H.empty, 1))
                    , (methodGet, ["api", "v1"], routes3, Just (H.fromList [("version", "v1")], 2))
                    , (methodGet, ["api", "vX", "aaa"], routes3, Just (H.empty, 3))
                    , (methodGet, ["api", "vX", "aaa1"], routes3, Nothing)
                    , (methodGet, ["api", "vX", "aaa", "b"], routes3, Nothing)

                    , (methodGet, ["api", "version"], routes4, Just (H.fromList [("version", "version")], 1))
                    , (methodGet, ["api1", "version"], routes4, Just (H.fromList [("api", "api1")], 2))
                    ]
          in mapM_ (\(m, path, rs, res) -> matchRequest (r m path) rs @?= res) cases
