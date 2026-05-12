{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Forest operations keyed by 'TestId': flattening, filtering, and trie
-- construction.  Lives in @sydtest@ so it can reference 'SpecDefForest'
-- directly; re-exported from @sydtest-mutation@.
module Test.Syd.Mutation.Forest
  ( -- * Test identifier tries
    TestIdTrie (..),
    testIdTrieFromSet,
    testIdTrieFromList,

    -- * Forest operations
    flattenTestForestWithIds,
    filterTestForestByTrie,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Syd.Mutation.TestId (TestId (..))
import Test.Syd.SpecDef

-- * TestIdTrie

-- | A trie over 'TestId' paths, used for efficient filtering of a 'TestForest'.
data TestIdTrie
  = -- | This path identifies a selected leaf test.
    TrieLeaf
  | -- | Intermediate node: descend into matching children.
    TrieNode (Map (Text, Word) TestIdTrie)
  deriving (Eq, Show)

instance Semigroup TestIdTrie where
  TrieLeaf <> _ = TrieLeaf
  _ <> TrieLeaf = TrieLeaf
  TrieNode m1 <> TrieNode m2 = TrieNode (Map.unionWith (<>) m1 m2)

instance Monoid TestIdTrie where
  mempty = TrieNode Map.empty

-- | Build a 'TestIdTrie' from a set of 'TestId's.
testIdTrieFromSet :: Set TestId -> TestIdTrie
testIdTrieFromSet = testIdTrieFromList . Set.toList

-- | Build a 'TestIdTrie' from a list of 'TestId's.
testIdTrieFromList :: [TestId] -> TestIdTrie
testIdTrieFromList = foldr insertId (TrieNode Map.empty)
  where
    insertId (TestId []) _ = TrieLeaf
    insertId (TestId ((t, i) : rest)) trie =
      let child = insertId (TestId rest) (childOf t i trie)
       in case trie of
            TrieLeaf -> TrieLeaf
            TrieNode m -> TrieNode (Map.insert (t, i) child m)

    childOf t i = \case
      TrieLeaf -> TrieLeaf
      TrieNode m -> Map.findWithDefault (TrieNode Map.empty) (t, i) m

-- * Forest operations

-- | Flatten a 'TestForest' into a list of '(TestId, value)' pairs.
--
-- 'TestId's are assigned by traversing the forest in order.  Sibling nodes
-- with the same description text receive a zero-based per-description index
-- so that duplicate descriptions are still uniquely identified.
flattenTestForestWithIds :: SpecDefForest '[] () result -> [(TestId, result)]
flattenTestForestWithIds = goForest []
  where
    goForest :: [(Text, Word)] -> SpecDefForest outers inner c -> [(TestId, c)]
    goForest path = snd . foldl (goTree path) (Map.empty, [])

    goTree ::
      [(Text, Word)] ->
      (Map Text Word, [(TestId, c)]) ->
      SpecDefTree outers inner c ->
      (Map Text Word, [(TestId, c)])
    goTree path (seen, acc) tree =
      let (mkey, seen') = nextKey seen tree
       in case tree of
            DefSpecifyNode _ _ e ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> (seen', acc ++ [(TestId (reverse (key : path)), e)])
            DefPendingNode _ _ -> (seen', acc)
            DefDescribeNode _ sub ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> (seen', acc ++ goForest (key : path) sub)
            DefSetupNode _ sub -> (seen', acc ++ goForest path sub)
            DefBeforeAllNode _ sub -> (seen', acc ++ goForest path sub)
            DefBeforeAllWithNode _ sub -> (seen', acc ++ goForest path sub)
            DefWrapNode _ sub -> (seen', acc ++ goForest path sub)
            DefAroundAllNode _ sub -> (seen', acc ++ goForest path sub)
            DefAroundAllWithNode _ sub -> (seen', acc ++ goForest path sub)
            DefAfterAllNode _ sub -> (seen', acc ++ goForest path sub)
            DefParallelismNode _ sub -> (seen', acc ++ goForest path sub)
            DefRandomisationNode _ sub -> (seen', acc ++ goForest path sub)
            DefTimeoutNode _ sub -> (seen', acc ++ goForest path sub)
            DefRetriesNode _ sub -> (seen', acc ++ goForest path sub)
            DefFlakinessNode _ sub -> (seen', acc ++ goForest path sub)
            DefExpectationNode _ sub -> (seen', acc ++ goForest path sub)

    nextKey :: Map Text Word -> SpecDefTree outers inner c -> (Maybe (Text, Word), Map Text Word)
    nextKey seen tree = case descriptionOf tree of
      Nothing -> (Nothing, seen)
      Just t ->
        let idx = Map.findWithDefault 0 t seen
         in (Just (t, idx), Map.insert t (idx + 1) seen)

    descriptionOf :: SpecDefTree outers inner c -> Maybe Text
    descriptionOf = \case
      DefSpecifyNode t _ _ -> Just t
      DefPendingNode t _ -> Just t
      DefDescribeNode t _ -> Just t
      _ -> Nothing

-- | Filter a 'TestForest' to only the tests present in the given 'TestIdTrie'.
--
-- Wrapper nodes (setup, before-all, around-all, etc.) are kept whenever any
-- of their children are kept.
filterTestForestByTrie :: TestIdTrie -> TestForest '[] () -> TestForest '[] ()
filterTestForestByTrie trie = snd . filterForest trie Map.empty
  where
    filterForest ::
      TestIdTrie ->
      Map Text Word ->
      SpecDefForest outers inner () ->
      (Map Text Word, SpecDefForest outers inner ())
    filterForest t seen = foldl (filterTree t) (seen, [])

    filterTree ::
      TestIdTrie ->
      (Map Text Word, SpecDefForest outers inner ()) ->
      SpecDefTree outers inner () ->
      (Map Text Word, SpecDefForest outers inner ())
    filterTree t (seen, acc) tree =
      let (mkey, seen') = nextKey seen tree
       in case tree of
            DefSpecifyNode name td e ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> case matchLeaf t key of
                  False -> (seen', acc)
                  True -> (seen', acc ++ [DefSpecifyNode name td e])
            DefPendingNode _ _ -> (seen', acc)
            DefDescribeNode name sub ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> case stepTrie t key of
                  Nothing -> (seen', acc)
                  Just subTrie ->
                    let (_, sub') = filterForest subTrie Map.empty sub
                     in if null sub' then (seen', acc) else (seen', acc ++ [DefDescribeNode name sub'])
            DefSetupNode func sub -> keepWrapper seen' acc (DefSetupNode func) (filterForest t Map.empty sub)
            DefBeforeAllNode func sub -> keepWrapper seen' acc (DefBeforeAllNode func) (filterForest t Map.empty sub)
            DefBeforeAllWithNode func sub -> keepWrapper seen' acc (DefBeforeAllWithNode func) (filterForest t Map.empty sub)
            DefWrapNode func sub -> keepWrapper seen' acc (DefWrapNode func) (filterForest t Map.empty sub)
            DefAroundAllNode func sub -> keepWrapper seen' acc (DefAroundAllNode func) (filterForest t Map.empty sub)
            DefAroundAllWithNode func sub -> keepWrapper seen' acc (DefAroundAllWithNode func) (filterForest t Map.empty sub)
            DefAfterAllNode func sub -> keepWrapper seen' acc (DefAfterAllNode func) (filterForest t Map.empty sub)
            DefParallelismNode p sub -> keepWrapper seen' acc (DefParallelismNode p) (filterForest t Map.empty sub)
            DefRandomisationNode p sub -> keepWrapper seen' acc (DefRandomisationNode p) (filterForest t Map.empty sub)
            DefTimeoutNode f sub -> keepWrapper seen' acc (DefTimeoutNode f) (filterForest t Map.empty sub)
            DefRetriesNode f sub -> keepWrapper seen' acc (DefRetriesNode f) (filterForest t Map.empty sub)
            DefFlakinessNode fm sub -> keepWrapper seen' acc (DefFlakinessNode fm) (filterForest t Map.empty sub)
            DefExpectationNode em sub -> keepWrapper seen' acc (DefExpectationNode em) (filterForest t Map.empty sub)

    keepWrapper ::
      Map Text Word ->
      SpecDefForest outers inner () ->
      (SpecDefForest outers2 inner2 () -> SpecDefTree outers inner ()) ->
      (Map Text Word, SpecDefForest outers2 inner2 ()) ->
      (Map Text Word, SpecDefForest outers inner ())
    keepWrapper seen' acc wrap (_, sub')
      | null sub' = (seen', acc)
      | otherwise = (seen', acc ++ [wrap sub'])

    matchLeaf :: TestIdTrie -> (Text, Word) -> Bool
    matchLeaf TrieLeaf _ = True
    matchLeaf (TrieNode m) key = case Map.lookup key m of
      Just TrieLeaf -> True
      _ -> False

    stepTrie :: TestIdTrie -> (Text, Word) -> Maybe TestIdTrie
    stepTrie TrieLeaf _ = Just TrieLeaf
    stepTrie (TrieNode m) key = Map.lookup key m

    nextKey :: Map Text Word -> SpecDefTree outers inner c -> (Maybe (Text, Word), Map Text Word)
    nextKey seen tree = case descriptionOf tree of
      Nothing -> (Nothing, seen)
      Just t ->
        let idx = Map.findWithDefault 0 t seen
         in (Just (t, idx), Map.insert t (idx + 1) seen)

    descriptionOf :: SpecDefTree outers inner c -> Maybe Text
    descriptionOf = \case
      DefSpecifyNode t _ _ -> Just t
      DefPendingNode t _ -> Just t
      DefDescribeNode t _ -> Just t
      _ -> Nothing
