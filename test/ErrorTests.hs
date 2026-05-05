module ErrorTests (errorTests) where

import qualified UnliftIO.Exception as E
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
import Webby.Types
import Prelude

errorTests :: TestTree
errorTests = testGroup "WebbyError displayException" [unitTests]
  where
    unitTests = HUnit.testCase "renders each constructor" $ do
      E.displayException (WebbyJSONParseError "ignored")
        @?= "Invalid JSON body"
      E.displayException (WebbyParamParseError "page" "not an integer")
        @?= "Param parse error: page not an integer"
      E.displayException (WebbyMissingCapture "userId")
        @?= "userId missing"
