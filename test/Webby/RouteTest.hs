module Webby.RouteTest where

import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.HUnit    ((@?=))
import qualified Test.Tasty.HUnit    as HUnit

import qualified Data.HashMap.Strict as H

import           Webby.Route
import           Webby.Types
import           WebbyPrelude

testHashTrie :: TestTree
testHashTrie = testGroup "HashTrie tests" [htAddItem, htLookupItem]

htAddItem :: TestTree
htAddItem = HUnit.testCase "Unit Test - Add Item" $ do
    let n = 1 :: Int
        leaf = HashTrie (Just n) H.empty Nothing
    addItem ([], n) emptyHashTrie @?=
        Just leaf

    addItem ([Literal "ab"], n) emptyHashTrie @?=
        Just (HashTrie Nothing (H.fromList [("ab", leaf)]) Nothing)

    let tnode = HashTrie Nothing (H.fromList [("cd", leaf)]) Nothing
    addItem ([Literal "ab", Literal "cd"], n) emptyHashTrie @?=
        Just (HashTrie Nothing (H.fromList [("ab", tnode)]) Nothing)

    -- capture test!
    addItem ([Capture "ab", Literal "cd"], n) emptyHashTrie @?=
        Just (HashTrie Nothing H.empty (Just ("ab", tnode)))

    -- TODO: capture test with overlapping capture and literal pattern

htLookupItem :: TestTree
htLookupItem = HUnit.testCase "Unit Test - Lookup Item" $ do
    lookupItem [] emptyHashTrie @?= (Nothing :: Maybe (Captures, Int))

    let n = 1 :: Int
        leaf = HashTrie (Just n) H.empty Nothing
    lookupItem [] leaf @?= Just (H.empty, n)

    let trie1 = HashTrie Nothing (H.fromList [("ab", leaf)]) Nothing
    lookupItem ["a"] trie1 @?= Nothing
    lookupItem ["ab"] trie1 @?= Just (H.empty, n)

    -- capture test!
    let tnode = HashTrie Nothing (H.fromList [("cd", leaf)]) Nothing
        trie2 = HashTrie Nothing H.empty (Just ("ab", tnode))
    lookupItem ["abc"] trie2 @?= Nothing
    lookupItem ["captureValue", "cd"] trie2 @?=
        Just (H.fromList [("ab", "captureValue")], n)

    -- TODO: capture test with overlapping capture and literal pattern
