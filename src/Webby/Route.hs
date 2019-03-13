{-# LANGUAGE BangPatterns #-}
module Webby.Route where

-- This modules contains the data structure and utilities to process
-- route patterns and lookup request paths internally in Webby.

import qualified Data.HashMap.Strict as H

import           Webby.Types
import           WebbyPrelude


data HashTrie a = HashTrie { handlerMay :: Maybe (WebbyM a ())
                           , litSubtree :: H.HashMap Text (HashTrie a)
                           , capMay     :: Maybe (Text, HashTrie a)
                           }

emptyHashTrie :: HashTrie a
emptyHashTrie = HashTrie Nothing H.empty Nothing

-- | addItem adds a route to the HashTrie, only if the route does not
-- already exist, and the route does not have overlapping captures.
--
-- An overlapping capture happens when there are two routes with
-- captures at the same position, for example:
--   @/person/:name@
--   @/person/:id@
--
-- Note that @/person/:name@ and @/address/:name@ are not overlapping
-- captures as they have different leading components.
addItem :: ([PathSegment], WebbyM a ()) -> HashTrie a -> Maybe (HashTrie a)
addItem ([], !handler) !trie =
    -- If there are no path segments remaining, set handler at the
    -- current node of the HashTrie
    case handlerMay trie of
      Nothing -> Just $ trie { handlerMay = Just handler }
      Just _  -> Nothing
addItem (b:bs, !handler) !trie =
    case b of
      -- For a literal path segment, lookup and insert in the
      -- litSubtree.
      Literal p ->
          let ltrie = litSubtree trie
              child = H.lookupDefault emptyHashTrie p ltrie
          in do nchild <- addItem (bs, handler) child
                let nltrie = H.insert p nchild ltrie
                return $ trie { litSubtree = nltrie }

      -- For a capture path segment, insert into the capture child of
      -- the current node (if it doesn't exist).
      Capture p ->
          case capMay trie of
            Nothing -> do ntrie <- addItem (bs, handler) emptyHashTrie
                          return $ trie { capMay = Just (p, ntrie) }
            Just _ -> Nothing

-- | lookupItem lookup the path segments in the tree and returns a
-- handler if a match is found along with a map of captures found.
lookupItem :: [Text] -> HashTrie a
           -> Maybe (Captures, WebbyM a ())
lookupItem !bs' !trie' = lookupItemWithCaptures bs' trie' H.empty
  where
    lookupItemWithCaptures [] !trie !h = do hdlr <- handlerMay trie
                                            return (h, hdlr)
    lookupItemWithCaptures (b:bs) !trie !h =
        let litChildMay = H.lookup b $ litSubtree trie
            captureLookup = do (capKey, childTrie) <- capMay trie
                               lookupItemWithCaptures bs childTrie $
                                   H.insert capKey b h
        in -- At lookup we always prefer a literal match over a
           -- capture if both are present at node.
          maybe captureLookup
          (\childTrie -> lookupItemWithCaptures bs childTrie h)
          litChildMay

routePattern2PathSegments :: RoutePattern -> [PathSegment]
routePattern2PathSegments (RoutePattern mthd ps) =
    (Literal $ decodeUtf8Lenient mthd) : ps

mkRoutesHashTrie :: Routes a -> Maybe (HashTrie a)
mkRoutesHashTrie rs = foldl combine (Just emptyHashTrie) rs
  where

    combine Nothing _ = Nothing
    combine (Just h) (rpat, handler) =
        let pathSegments = routePattern2PathSegments rpat
        in addItem (pathSegments, handler) h
