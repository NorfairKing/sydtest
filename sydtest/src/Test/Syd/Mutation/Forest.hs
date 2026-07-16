{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    flattenTestForestWithIdsAndCallStacks,
    filterTestForestByTrie,
    reorderTestForestByTiming,
    reorderForMutationChild,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify')
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Stack (CallStack)
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
    insertId (TestId steps) trie = insertSteps (NE.toList steps) trie

    insertSteps [] _ = TrieLeaf
    insertSteps ((t, i) : rest) trie =
      let child = insertSteps rest (childOf t i trie)
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
flattenTestForestWithIds = flattenTestForestWith (\_callStack e -> e)

-- | Like 'flattenTestForestWithIds', but pair each leaf 'TestId' with the
-- 'CallStack' captured at its @it@\/@prop@\/@specify@ call site
-- ('testDefCallStack') instead of the leaf value.  The 'TestId' assignment is
-- identical, so the two stay in lockstep: a leaf's id here is the same id that
-- the coverage phase records coverage against.
--
-- The 'CallStack' lets the diff-scoped runner map a changed test-source line
-- back to the tests defined there.
flattenTestForestWithIdsAndCallStacks :: SpecDefForest '[] () result -> [(TestId, CallStack)]
flattenTestForestWithIdsAndCallStacks = flattenTestForestWith (\callStack _e -> callStack)

-- | Flatten a 'TestForest' into a list of '(TestId, value)' pairs, where each
-- leaf's value is computed from its 'CallStack' ('testDefCallStack') and its
-- leaf value by the given projection.  'flattenTestForestWithIds' and
-- 'flattenTestForestWithIdsAndCallStacks' are the two projections we use.
--
-- 'TestId's are assigned by traversing the forest in order.  Sibling nodes
-- with the same description text receive a zero-based per-description index so
-- that duplicate descriptions are still uniquely identified.
flattenTestForestWith :: forall result a. (CallStack -> result -> a) -> SpecDefForest '[] () result -> [(TestId, a)]
flattenTestForestWith leaf f = evalState (execWriterT (goForest [] f)) Map.empty
  where
    -- The wrapper nodes (setup, before-all, ...) preserve the forest's @extra@
    -- (result) type parameter, so it is uniformly @result@ throughout; only
    -- @outers@ and @inner@ vary, which is why those stay polymorphic here.
    --
    -- Each call to 'goForest' establishes its own fresh sibling-index counter
    -- and restores the caller's counter on the way out.  Both 'DefDescribeNode'
    -- and the wrapper nodes descend via 'goForest', so each wrapped sub-forest
    -- gets its own counter, matching the accumulator-style implementation this
    -- replaces.
    goForest :: [(Text, Word)] -> SpecDefForest outers inner result -> WriterT [(TestId, a)] (State (Map Text Word)) ()
    goForest path sub = do
      saved <- gets id
      modify' (const Map.empty)
      mapM_ (goTree path) sub
      modify' (const saved)

    goTree ::
      [(Text, Word)] ->
      SpecDefTree outers inner result ->
      WriterT [(TestId, a)] (State (Map Text Word)) ()
    goTree path tree = do
      mkey <- nextKey tree
      case tree of
        DefSpecifyNode _ td e ->
          case mkey of
            Nothing -> pure ()
            Just key -> tell [(TestId (NE.fromList (reverse (key : path))), leaf (testDefCallStack td) e)]
        DefPendingNode _ _ -> pure ()
        DefDescribeNode _ sub ->
          case mkey of
            Nothing -> pure ()
            Just key -> goForest (key : path) sub
        DefSetupNode _ sub -> goForest path sub
        DefBeforeAllNode _ sub -> goForest path sub
        DefBeforeAllWithNode _ sub -> goForest path sub
        DefWrapNode _ sub -> goForest path sub
        DefAroundAllNode _ sub -> goForest path sub
        DefAroundAllWithNode _ sub -> goForest path sub
        DefAfterAllNode _ sub -> goForest path sub
        DefParallelismNode _ sub -> goForest path sub
        DefRandomisationNode _ sub -> goForest path sub
        DefTimeoutNode _ sub -> goForest path sub
        DefRetriesNode _ sub -> goForest path sub
        DefFlakinessNode _ sub -> goForest path sub
        DefExpectationNode _ sub -> goForest path sub

    -- [tag:ReorderIdScheme] 'reorderTestForestByTiming' replicates this
    -- per-description sibling-index assignment so its cost lookups line up with
    -- the baselines recorded against these ids.
    nextKey :: SpecDefTree outers inner result -> WriterT [(TestId, a)] (State (Map Text Word)) (Maybe (Text, Word))
    nextKey tree = case descriptionOf tree of
      Nothing -> pure Nothing
      Just t -> do
        idx <- gets (Map.findWithDefault 0 t)
        modify' (Map.insert t (idx + 1))
        pure (Just (t, idx))

    descriptionOf :: SpecDefTree outers inner result -> Maybe Text
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
    -- 'filterForestRev' accumulates in reverse so each kept tree is consed
    -- onto the head of @acc@ in O(1).  We reverse once at the end of
    -- 'filterForest' to restore source order.  The previous implementation
    -- used @acc ++ [tree]@ at every step, which is O(N^2) over sibling
    -- count and visible on large per-test coverage runs.
    filterForest ::
      TestIdTrie ->
      Map Text Word ->
      SpecDefForest outers inner () ->
      (Map Text Word, SpecDefForest outers inner ())
    filterForest t seen sub =
      let (seen', revAcc) = foldl (filterTree t) (seen, []) sub
       in (seen', reverse revAcc)

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
                  True -> (seen', DefSpecifyNode name td e : acc)
            DefPendingNode _ _ -> (seen', acc)
            DefDescribeNode name sub ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> case stepTrie t key of
                  Nothing -> (seen', acc)
                  Just subTrie ->
                    let (_, sub') = filterForest subTrie Map.empty sub
                     in if null sub' then (seen', acc) else (seen', DefDescribeNode name sub' : acc)
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
      | otherwise = (seen', wrap sub' : acc)

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

-- | Reorder a 'TestForest' so that, at every sibling level, cheaper tests come
-- first.  A test's cost is its recorded baseline in the given map; a missing
-- baseline counts as @0@, so an untimed test runs first.  A subtree is ordered
-- by the minimum cost of any leaf it contains, so the group most likely to yield
-- a cheap early kill runs first.
--
-- This is a drop-in alternative to execution-order randomisation, legal under
-- exactly the same conditions.  It mirrors 'randomiseTestForest': inside a
-- 'DoNotRandomiseExecutionOrder' scope it leaves the whole subtree as-is at
-- every depth, and the caller only applies it when the suite's execution-order
-- randomisation is enabled.
--
-- Only sibling order changes.  Wrapper nodes keep wrapping their own children,
-- so shared setup and teardown still run once per group and are never split.
-- The sort is stable, so equal-cost siblings keep their source order.
--
-- 'TestId's are assigned with the same scheme as 'flattenTestForestWith', purely
-- to look costs up; nothing downstream reads the ids of the reordered forest.
-- When this runs on a forest that 'filterTestForestByTrie' has thinned, the
-- per-description sibling index of same-named siblings can shift relative to the
-- baseline map's keys.  That degrades ordering quality for same-named siblings
-- only, never which tests run.
reorderTestForestByTiming :: Map TestId Word -> TestForest '[] () -> TestForest '[] ()
reorderTestForestByTiming costs = snd . goForest []
  where
    -- Reorder the sub-forest and report the minimum leaf cost it contains
    -- ('maxBound' when it contains no leaf, so leafless subtrees sort last).
    goForest ::
      [(Text, Word)] ->
      SpecDefForest outers inner () ->
      (Word, SpecDefForest outers inner ())
    goForest path sub =
      let keyed =
            reverse $
              snd $
                foldl
                  ( \(seen, acc) tree ->
                      let (mkey, seen') = nextKey seen tree
                       in (seen', goTree path mkey tree : acc)
                  )
                  (Map.empty, [])
                  sub
       in ( if null keyed then maxBound else minimum (map fst keyed),
            map snd (sortOn fst keyed)
          )

    goTree ::
      [(Text, Word)] ->
      Maybe (Text, Word) ->
      SpecDefTree outers inner () ->
      (Word, SpecDefTree outers inner ())
    goTree path mkey tree = case tree of
      DefSpecifyNode name td e -> case mkey of
        Nothing -> (maxBound, tree)
        Just key ->
          let tid = TestId (NE.fromList (reverse (key : path)))
           in (Map.findWithDefault 0 tid costs, DefSpecifyNode name td e)
      DefPendingNode _ _ -> (maxBound, tree)
      DefDescribeNode name sub -> case mkey of
        Nothing -> (maxBound, tree)
        Just key ->
          let (cost, sub') = goForest (key : path) sub
           in (cost, DefDescribeNode name sub')
      DefSetupNode func sub -> wrap (DefSetupNode func) path sub
      DefBeforeAllNode func sub -> wrap (DefBeforeAllNode func) path sub
      DefBeforeAllWithNode func sub -> wrap (DefBeforeAllWithNode func) path sub
      DefWrapNode func sub -> wrap (DefWrapNode func) path sub
      DefAroundAllNode func sub -> wrap (DefAroundAllNode func) path sub
      DefAroundAllWithNode func sub -> wrap (DefAroundAllWithNode func) path sub
      DefAfterAllNode func sub -> wrap (DefAfterAllNode func) path sub
      DefParallelismNode p sub -> wrap (DefParallelismNode p) path sub
      DefTimeoutNode f sub -> wrap (DefTimeoutNode f) path sub
      DefRetriesNode f sub -> wrap (DefRetriesNode f) path sub
      DefFlakinessNode fm sub -> wrap (DefFlakinessNode fm) path sub
      DefExpectationNode em sub -> wrap (DefExpectationNode em) path sub
      DefRandomisationNode eor sub -> case eor of
        RandomiseExecutionOrder -> wrap (DefRandomisationNode eor) path sub
        -- [tag:ReorderRandomiseBoundary] Mirror 'randomiseTestForest': inside a
        -- 'DoNotRandomiseExecutionOrder' scope, leave the whole subtree as-is at
        -- every depth.  Still compute its cost (order-invariant) to position the
        -- node among its own siblings; discard the reordered structure.
        DoNotRandomiseExecutionOrder ->
          let (cost, _) = goForest path sub
           in (cost, DefRandomisationNode eor sub)

    -- Reorder a wrapper's children and keep the wrapper wrapping them.  The
    -- wrapper contributes no path step (matching 'flattenTestForestWith'), so
    -- its children stay keyed under the same @path@.
    wrap ::
      (SpecDefForest a b () -> SpecDefTree outers inner ()) ->
      [(Text, Word)] ->
      SpecDefForest a b () ->
      (Word, SpecDefTree outers inner ())
    wrap con path sub =
      let (cost, sub') = goForest path sub
       in (cost, con sub')

    -- [ref:ReorderIdScheme] Assign the per-description sibling index exactly as
    -- 'flattenTestForestWith' does, so cost lookups hit the baseline map.
    nextKey :: Map Text Word -> SpecDefTree outers inner () -> (Maybe (Text, Word), Map Text Word)
    nextKey seen tree = case descriptionOf tree of
      Nothing -> (Nothing, seen)
      Just t ->
        let idx = Map.findWithDefault 0 t seen
         in (Just (t, idx), Map.insert t (idx + 1) seen)

    descriptionOf :: SpecDefTree outers inner () -> Maybe Text
    descriptionOf = \case
      DefSpecifyNode t _ _ -> Just t
      DefPendingNode t _ -> Just t
      DefDescribeNode t _ -> Just t
      _ -> Nothing

-- | Decide the execution order for a mutation child's (already filtered)
-- forest.  Reorder cheapest-first with 'reorderTestForestByTiming' only when
-- the suite has execution-order randomisation enabled (so this stays a drop-in
-- alternative to that randomisation) and a baseline is available; otherwise
-- leave the forest untouched.
--
-- Kept pure and separate from the child's IO so the gate is testable: the
-- child reads the baseline and looks up 'settingRandomiseExecutionOrder', then
-- hands both here.
reorderForMutationChild :: Bool -> Maybe (Map TestId Word) -> TestForest '[] () -> TestForest '[] ()
reorderForMutationChild randomiseEnabled mCosts forest =
  case (randomiseEnabled, mCosts) of
    (True, Just costs) -> reorderTestForestByTiming costs forest
    _ -> forest
