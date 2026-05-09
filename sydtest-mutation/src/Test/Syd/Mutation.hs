{-# LANGUAGE DataKinds #-}

module Test.Syd.Mutation
  ( -- * Test identifiers
    TestId (..),
    renderTestId,
    parseTestIdFilterArg,

    -- * Test identifier tries
    TestIdTrie (..),
    testIdTrieFromSet,
    testIdTrieFromList,

    -- * Forest operations
    flattenTestForestWithIds,
    filterTestForestByTrie,

    -- * Running
    execTestDefM',
    getTestIds,
    runFilteredForest,
  )
where

import Test.Syd
import Test.Syd.Mutation.Forest
  ( TestIdTrie (..),
    filterTestForestByTrie,
    flattenTestForestWithIds,
    testIdTrieFromList,
    testIdTrieFromSet,
  )
import Test.Syd.Mutation.TestId (TestId (..), parseTestIdFilterArg, renderTestId)
import Test.Syd.OptParse (Settings (..), defaultSettings)

-- * Running

-- | Like 'execTestDefM', but also returns a 'TestIdTrie' covering all tests
-- in the resulting forest.
-- Use this when you need to enumerate test IDs and run the forest without
-- evaluating the 'Spec' twice.
execTestDefM' :: Settings -> Spec -> IO (TestForest '[] (), TestIdTrie)
execTestDefM' sets spec = do
  forest <- execTestDefM sets spec
  let ids = map fst (flattenTestForestWithIds forest)
  pure (forest, testIdTrieFromList ids)

-- | Enumerate all 'TestId's in a 'Spec' without running any tests.
getTestIds :: Spec -> IO [TestId]
getTestIds spec = do
  forest <- execTestDefM defaultSettings spec
  pure $ map fst (flattenTestForestWithIds forest)

-- | Run a pre-built 'TestForest', filtered to only the tests in the given
-- 'TestIdTrie'.  Returns 'True' if all selected tests passed.
--
-- Use 'execTestDefM'' to build the forest and full trie once, then call this
-- function with a per-mutation trie on each iteration.
runFilteredForest :: Settings -> TestForest '[] () -> TestIdTrie -> IO Bool
runFilteredForest settings forest trie = do
  let filtered = filterTestForestByTrie trie forest
  timedResult <- runSpecForestSynchronously settings filtered
  pure $ not $ anyFailedTests settings (timedValue timedResult)
