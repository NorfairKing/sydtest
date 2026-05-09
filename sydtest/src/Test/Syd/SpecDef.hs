{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.SpecDef where

import Control.Monad
import Control.Monad.Random
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (find)
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack
import System.Random.Shuffle
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.SpecForest
import Test.Syd.TestId

data TDef value = TDef {testDefVal :: value, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest outers inner = SpecDefForest outers inner ()

type TestTree outers inner = SpecDefTree outers inner ()

type SpecDefForest (outers :: [Type]) inner extra = [SpecDefTree outers inner extra]

-- | A tree of tests
--
-- This type has three parameters:
--
-- * @outers@: A type-level list of the outer resources. These are resources that are prived once, around a group of tests. (This is the type of the results of `aroundAll`.)
-- * @inner@: The inner resource. This is a resource that is set up around every test, and even every example of a property test. (This is the type of the result of `around`.)
-- * @result@: The result ('TestDefM' is a monad.)
--
-- In practice, all of these three parameters should be '()' at the top level.
--
-- When you're just using sydtest and not writing a library for sydtest, you probably don't even want to concern yourself with this type.
data SpecDefTree (outers :: [Type]) inner extra where
  -- | Define a test
  DefSpecifyNode ::
    -- | The description of the test
    Text ->
    -- | How the test can be run given a function that provides the resources
    TDef (ProgressReporter -> ((HList outers -> inner -> IO ()) -> IO ()) -> IO TestRunResult) ->
    extra ->
    SpecDefTree outers inner extra
  -- | Define a pending test
  DefPendingNode ::
    -- | The description of the test
    Text ->
    -- | The reason why the test is pending
    Maybe Text ->
    SpecDefTree outers inner extra
  -- | Group tests using a description
  DefDescribeNode ::
    -- | The description
    Text ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefSetupNode ::
    -- | The function that runs before the test
    IO () ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefBeforeAllNode ::
    -- | The function to run (once), beforehand, to produce the outer resource.
    IO outer ->
    SpecDefForest (outer ': otherOuters) inner extra ->
    SpecDefTree otherOuters inner extra
  DefBeforeAllWithNode ::
    -- | The function to run (once), beforehand, to produce the outer resource.
    (oldOuter -> IO newOuter) ->
    SpecDefForest (newOuter ': oldOuter ': otherOuters) inner extra ->
    SpecDefTree (oldOuter ': otherOuters) inner extra
  DefWrapNode ::
    -- | The function that wraps running the tests.
    (IO () -> IO ()) ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefAroundAllNode ::
    -- | The function that provides the outer resource (once), around the tests.
    ((outer -> IO ()) -> IO ()) ->
    SpecDefForest (outer ': otherOuters) inner extra ->
    SpecDefTree otherOuters inner extra
  DefAroundAllWithNode ::
    -- | The function that provides the new outer resource (once), using the old outer resource.
    ((newOuter -> IO ()) -> (HList (oldOuter ': otherOuters) -> IO ())) ->
    SpecDefForest (newOuter ': oldOuter ': otherOuters) inner extra ->
    SpecDefTree (oldOuter ': otherOuters) inner extra
  DefAfterAllNode ::
    -- | The function to run (once), afterwards, using all outer resources.
    (HList outers -> IO ()) ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  -- | Control the level of parallelism for a given group of tests
  DefParallelismNode ::
    -- | The level of parallelism
    Parallelism ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  -- | Control the execution order randomisation for a given group of tests
  DefRandomisationNode ::
    -- | The execution order randomisation
    ExecutionOrderRandomisation ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefTimeoutNode ::
    -- | Modify the timeout setting
    (Timeout -> Timeout) ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefRetriesNode ::
    -- | Modify the number of retries
    (Word -> Word) ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefFlakinessNode ::
    -- | Whether to allow flakiness
    FlakinessMode ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefExpectationNode ::
    -- | Whether to expect passing or failing
    ExpectationMode ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra

instance Functor (SpecDefTree a c) where
  fmap :: forall e f. (e -> f) -> SpecDefTree a c e -> SpecDefTree a c f
  fmap f =
    let goF :: forall x y. SpecDefForest x y e -> SpecDefForest x y f
        goF = map (fmap f)
     in \case
          DefDescribeNode t sdf -> DefDescribeNode t $ goF sdf
          DefPendingNode t mr -> DefPendingNode t mr
          DefSpecifyNode t td e -> DefSpecifyNode t td (f e)
          DefSetupNode func sdf -> DefSetupNode func $ goF sdf
          DefBeforeAllNode func sdf -> DefBeforeAllNode func $ goF sdf
          DefBeforeAllWithNode func sdf -> DefBeforeAllWithNode func $ goF sdf
          DefWrapNode func sdf -> DefWrapNode func $ goF sdf
          DefAroundAllNode func sdf -> DefAroundAllNode func $ goF sdf
          DefAroundAllWithNode func sdf -> DefAroundAllWithNode func $ goF sdf
          DefAfterAllNode func sdf -> DefAfterAllNode func $ goF sdf
          DefParallelismNode p sdf -> DefParallelismNode p $ goF sdf
          DefRandomisationNode p sdf -> DefRandomisationNode p $ goF sdf
          DefTimeoutNode p sdf -> DefTimeoutNode p $ goF sdf
          DefRetriesNode p sdf -> DefRetriesNode p $ goF sdf
          DefFlakinessNode p sdf -> DefFlakinessNode p $ goF sdf
          DefExpectationNode p sdf -> DefExpectationNode p $ goF sdf

instance Foldable (SpecDefTree a c) where
  foldMap :: forall e m. (Monoid m) => (e -> m) -> SpecDefTree a c e -> m
  foldMap f =
    let goF :: forall x y. SpecDefForest x y e -> m
        goF = foldMap (foldMap f)
     in \case
          DefDescribeNode _ sdf -> goF sdf
          DefPendingNode _ _ -> mempty
          DefSpecifyNode _ _ e -> f e
          DefSetupNode _ sdf -> goF sdf
          DefBeforeAllNode _ sdf -> goF sdf
          DefBeforeAllWithNode _ sdf -> goF sdf
          DefWrapNode _ sdf -> goF sdf
          DefAroundAllNode _ sdf -> goF sdf
          DefAroundAllWithNode _ sdf -> goF sdf
          DefAfterAllNode _ sdf -> goF sdf
          DefParallelismNode _ sdf -> goF sdf
          DefRandomisationNode _ sdf -> goF sdf
          DefTimeoutNode _ sdf -> goF sdf
          DefRetriesNode _ sdf -> goF sdf
          DefFlakinessNode _ sdf -> goF sdf
          DefExpectationNode _ sdf -> goF sdf

instance Traversable (SpecDefTree a c) where
  traverse :: forall u w f. (Applicative f) => (u -> f w) -> SpecDefTree a c u -> f (SpecDefTree a c w)
  traverse f =
    let goF :: forall x y. SpecDefForest x y u -> f (SpecDefForest x y w)
        goF = traverse (traverse f)
     in \case
          DefDescribeNode t sdf -> DefDescribeNode t <$> goF sdf
          DefPendingNode t mr -> pure $ DefPendingNode t mr
          DefSpecifyNode t td e -> DefSpecifyNode t td <$> f e
          DefSetupNode func sdf -> DefSetupNode func <$> goF sdf
          DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goF sdf
          DefBeforeAllWithNode func sdf -> DefBeforeAllWithNode func <$> goF sdf
          DefWrapNode func sdf -> DefWrapNode func <$> goF sdf
          DefAroundAllNode func sdf -> DefAroundAllNode func <$> goF sdf
          DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goF sdf
          DefAfterAllNode func sdf -> DefAfterAllNode func <$> goF sdf
          DefParallelismNode p sdf -> DefParallelismNode p <$> goF sdf
          DefRandomisationNode p sdf -> DefRandomisationNode p <$> goF sdf
          DefTimeoutNode p sdf -> DefTimeoutNode p <$> goF sdf
          DefRetriesNode p sdf -> DefRetriesNode p <$> goF sdf
          DefFlakinessNode p sdf -> DefFlakinessNode p <$> goF sdf
          DefExpectationNode p sdf -> DefExpectationNode p <$> goF sdf

filterTestForest :: [Text] -> SpecDefForest outers inner result -> SpecDefForest outers inner result
filterTestForest fs = fromMaybe [] . goForest DList.empty
  where
    goForest :: DList Text -> SpecDefForest a b c -> Maybe (SpecDefForest a b c)
    goForest ts sdf = do
      let sdf' = mapMaybe (goTree ts) sdf
      guard $ not $ null sdf'
      pure sdf'

    filterGuard :: DList Text -> Bool
    filterGuard dl =
      null fs
        || any (\f -> f `T.isInfixOf` T.intercalate "." (DList.toList dl)) fs

    goTree :: DList Text -> SpecDefTree a b c -> Maybe (SpecDefTree a b c)
    goTree dl = \case
      DefSpecifyNode t td e -> do
        let tl = DList.snoc dl t
        guard $ filterGuard tl
        pure $ DefSpecifyNode t td e
      DefPendingNode t mr -> do
        let tl = DList.snoc dl t
        guard $ filterGuard tl
        pure $ DefPendingNode t mr
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest (DList.snoc dl t) sdf
      DefSetupNode func sdf -> DefSetupNode func <$> goForest dl sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goForest dl sdf
      DefBeforeAllWithNode func sdf -> DefBeforeAllWithNode func <$> goForest dl sdf
      DefWrapNode func sdf -> DefWrapNode func <$> goForest dl sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest dl sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goForest dl sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func <$> goForest dl sdf
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest dl sdf
      DefRandomisationNode func sdf -> DefRandomisationNode func <$> goForest dl sdf
      DefTimeoutNode func sdf -> DefTimeoutNode func <$> goForest dl sdf
      DefRetriesNode func sdf -> DefRetriesNode func <$> goForest dl sdf
      DefFlakinessNode func sdf -> DefFlakinessNode func <$> goForest dl sdf
      DefExpectationNode func sdf -> DefExpectationNode func <$> goForest dl sdf

-- | Flatten a 'TestForest' into a list of '(TestId, value)' pairs.
--
-- 'TestId's are assigned by traversing the forest in order.  Sibling nodes
-- with the same description text receive a zero-based per-description index
-- so that duplicate descriptions are still uniquely identified.
flattenTestForestWithIds :: SpecDefForest '[] () result -> [(TestId, result)]
flattenTestForestWithIds = goForest []
  where
    goForest :: [(Text, Int)] -> SpecDefForest outers inner c -> [(TestId, c)]
    goForest path = snd . foldl (goTree path) (Map.empty, [])

    goTree ::
      [(Text, Int)] ->
      (Map Text Int, [(TestId, c)]) ->
      SpecDefTree outers inner c ->
      (Map Text Int, [(TestId, c)])
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
            -- Wrapper nodes are transparent: same path level, same trie step.
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

    nextKey :: Map Text Int -> SpecDefTree outers inner c -> (Maybe (Text, Int), Map Text Int)
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
      Map Text Int ->
      SpecDefForest outers inner () ->
      (Map Text Int, SpecDefForest outers inner ())
    filterForest t seen trees = foldl (filterTree t) (seen, []) trees

    filterTree ::
      TestIdTrie ->
      (Map Text Int, SpecDefForest outers inner ()) ->
      SpecDefTree outers inner () ->
      (Map Text Int, SpecDefForest outers inner ())
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
            -- Wrapper nodes are transparent: pass the same trie level down.
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
      Map Text Int ->
      SpecDefForest outers inner () ->
      (SpecDefForest outers2 inner2 () -> SpecDefTree outers inner ()) ->
      (Map Text Int, SpecDefForest outers2 inner2 ()) ->
      (Map Text Int, SpecDefForest outers inner ())
    keepWrapper seen' acc wrap (_, sub')
      | null sub' = (seen', acc)
      | otherwise = (seen', acc ++ [wrap sub'])

    matchLeaf :: TestIdTrie -> (Text, Int) -> Bool
    matchLeaf TrieLeaf _ = True
    matchLeaf (TrieNode m) key = case Map.lookup key m of
      Just TrieLeaf -> True
      _ -> False

    stepTrie :: TestIdTrie -> (Text, Int) -> Maybe TestIdTrie
    stepTrie TrieLeaf _ = Just TrieLeaf
    stepTrie (TrieNode m) key = Map.lookup key m

    nextKey :: Map Text Int -> SpecDefTree outers inner c -> (Maybe (Text, Int), Map Text Int)
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

randomiseTestForest :: (MonadRandom m) => SpecDefForest outers inner result -> m (SpecDefForest outers inner result)
randomiseTestForest = goForest
  where
    goForest :: (MonadRandom m) => SpecDefForest a b c -> m (SpecDefForest a b c)
    goForest = traverse goTree >=> shuffleM
    goTree :: (MonadRandom m) => SpecDefTree a b c -> m (SpecDefTree a b c)
    goTree = \case
      DefSpecifyNode t td e -> pure $ DefSpecifyNode t td e
      DefPendingNode t mr -> pure $ DefPendingNode t mr
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest sdf
      DefSetupNode func sdf -> DefSetupNode func <$> goForest sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goForest sdf
      DefBeforeAllWithNode func sdf -> DefBeforeAllWithNode func <$> goForest sdf
      DefWrapNode func sdf -> DefWrapNode func <$> goForest sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goForest sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func <$> goForest sdf
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest sdf
      DefTimeoutNode i sdf -> DefTimeoutNode i <$> goForest sdf
      DefRetriesNode i sdf -> DefRetriesNode i <$> goForest sdf
      DefFlakinessNode i sdf -> DefFlakinessNode i <$> goForest sdf
      DefExpectationNode i sdf -> DefExpectationNode i <$> goForest sdf
      DefRandomisationNode eor sdf ->
        DefRandomisationNode eor <$> case eor of
          RandomiseExecutionOrder -> goForest sdf
          DoNotRandomiseExecutionOrder -> pure sdf

markSpecForestAsPending :: Maybe Text -> SpecDefForest outers inner result -> SpecDefForest outers inner result
markSpecForestAsPending mMessage = goForest
  where
    goForest :: SpecDefForest a b c -> SpecDefForest a b c
    goForest = map goTree

    goTree :: SpecDefTree a b c -> SpecDefTree a b c
    goTree = \case
      DefSpecifyNode t _ _ -> DefPendingNode t mMessage
      DefPendingNode t mr -> DefPendingNode t mr
      DefDescribeNode t sdf -> DefDescribeNode t $ goForest sdf
      DefSetupNode func sdf -> DefSetupNode func $ goForest sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func $ goForest sdf
      DefBeforeAllWithNode func sdf -> DefBeforeAllWithNode func $ goForest sdf
      DefWrapNode func sdf -> DefWrapNode func $ goForest sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func $ goForest sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func $ goForest sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func $ goForest sdf
      DefParallelismNode func sdf -> DefParallelismNode func $ goForest sdf
      DefTimeoutNode i sdf -> DefTimeoutNode i $ goForest sdf
      DefRetriesNode i sdf -> DefRetriesNode i $ goForest sdf
      DefFlakinessNode i sdf -> DefFlakinessNode i $ goForest sdf
      DefRandomisationNode eor sdf -> DefRandomisationNode eor (goForest sdf)
      DefExpectationNode i sdf -> DefExpectationNode i $ goForest sdf

data Parallelism
  = Parallel
  | Sequential
  deriving (Show, Eq, Generic)

data ExecutionOrderRandomisation
  = RandomiseExecutionOrder
  | DoNotRandomiseExecutionOrder
  deriving (Show, Eq, Generic)

data FlakinessMode
  = MayNotBeFlaky
  | MayBeFlaky !(Maybe String) -- A message to show whenever the test is flaky.
  deriving (Show, Eq, Generic)

data ExpectationMode
  = ExpectPassing
  | ExpectFailing
  deriving (Show, Eq, Generic)

type ResultForest = SpecForest (TDef (Timed TestRunReport))

type ResultTree = SpecTree (TDef (Timed TestRunReport))

computeTestSuiteStats :: Settings -> ResultForest -> TestSuiteStats
computeTestSuiteStats settings = goF []
  where
    goF :: [Text] -> ResultForest -> TestSuiteStats
    goF ts = foldMap (goT ts)
    goT :: [Text] -> ResultTree -> TestSuiteStats
    goT ts = \case
      SpecifyNode _ (TDef timed@Timed {..} _) ->
        let status = testRunReportStatus settings timedValue
         in TestSuiteStats
              { testSuiteStatSuccesses = case status of
                  TestPassed -> 1
                  TestFailed -> 0,
                testSuiteStatExamples =
                  testRunReportExamples timedValue,
                testSuiteStatFailures = case status of
                  TestPassed -> 0
                  TestFailed -> 1,
                testSuiteStatFlakyTests =
                  if testRunReportWasFlaky timedValue
                    then 1
                    else 0,
                testSuiteStatPending = 0,
                testSuiteStatSumTime = timedTime timed
              }
      PendingNode _ _ ->
        TestSuiteStats
          { testSuiteStatSuccesses = 0,
            testSuiteStatExamples = 0,
            testSuiteStatFailures = 0,
            testSuiteStatFlakyTests = 0,
            testSuiteStatPending = 1,
            testSuiteStatSumTime = 0
          }
      DescribeNode t sf -> goF (t : ts) sf
      SubForestNode sf -> goF ts sf

data TestSuiteStats = TestSuiteStats
  { testSuiteStatSuccesses :: !Word,
    testSuiteStatExamples :: !Word,
    testSuiteStatFailures :: !Word,
    testSuiteStatFlakyTests :: !Word,
    testSuiteStatPending :: !Word,
    testSuiteStatSumTime :: !Word64
  }
  deriving (Show, Eq)

instance Semigroup TestSuiteStats where
  (<>) tss1 tss2 =
    TestSuiteStats
      { testSuiteStatSuccesses = testSuiteStatSuccesses tss1 + testSuiteStatSuccesses tss2,
        testSuiteStatExamples = testSuiteStatExamples tss1 + testSuiteStatExamples tss2,
        testSuiteStatFailures = testSuiteStatFailures tss1 + testSuiteStatFailures tss2,
        testSuiteStatFlakyTests = testSuiteStatFlakyTests tss1 + testSuiteStatFlakyTests tss2,
        testSuiteStatPending = testSuiteStatPending tss1 + testSuiteStatPending tss2,
        testSuiteStatSumTime = testSuiteStatSumTime tss1 + testSuiteStatSumTime tss2
      }

instance Monoid TestSuiteStats where
  mappend = (<>)
  mempty =
    TestSuiteStats
      { testSuiteStatSuccesses = 0,
        testSuiteStatExamples = 0,
        testSuiteStatFailures = 0,
        testSuiteStatFlakyTests = 0,
        testSuiteStatPending = 0,
        testSuiteStatSumTime = 0
      }

shouldExitFail :: Settings -> ResultForest -> Bool
shouldExitFail settings resultForest =
  -- Fail if there were no tests.
  --
  -- This is technically valid but in practice we don't ever want to
  -- consider an empty test suite succesfull.
  --
  -- By considering an empty test suite unsuccesful, we can catch cases in
  -- which we have accidentally used a filter that does not match any
  -- tests at all.
  null resultForest
    -- ... or if any tests failed.
    || anyFailedTests settings resultForest

anyFailedTests :: Settings -> ResultForest -> Bool
anyFailedTests settings resultForest =
  any (any (testRunReportFailed settings . timedValue . testDefVal)) resultForest

data TestRunReport = TestRunReport
  { testRunReportExpectationMode :: !ExpectationMode,
    -- | Raw results, including retries, in order
    testRunReportRawResults :: !(NonEmpty TestRunResult),
    testRunReportFlakinessMode :: !FlakinessMode
  }
  deriving (Show, Generic)

testRunReportReportedRun :: TestRunReport -> TestRunResult
testRunReportReportedRun TestRunReport {..} =
  -- We always want to report the last failure if there are any failures.
  -- This is because a passed test does not give us any information, and we
  -- only want to do that if there are no failures.
  let reversed = NE.reverse testRunReportRawResults
   in case find ((== TestFailed) . testRunResultStatus) testRunReportRawResults of
        Nothing -> NE.head reversed
        Just trr -> trr

testRunReportFailed :: Settings -> TestRunReport -> Bool
testRunReportFailed settings testRunReport =
  testRunReportStatus settings testRunReport /= TestPassed

testRunReportStatus :: Settings -> TestRunReport -> TestStatus
testRunReportStatus Settings {..} testRunReport@TestRunReport {..} =
  let wasFlaky = testRunReportWasFlaky testRunReport
      lastResult = NE.last testRunReportRawResults
      actualStatus = case testRunReportFlakinessMode of
        MayNotBeFlaky ->
          if wasFlaky
            then TestFailed
            else testRunResultStatus lastResult
        MayBeFlaky _ ->
          if settingFailOnFlaky && wasFlaky
            then TestFailed
            else
              if any ((== TestPassed) . testRunResultStatus) testRunReportRawResults
                then TestPassed
                else TestFailed
      consideredStatus =
        if testStatusMatchesExpectationMode actualStatus testRunReportExpectationMode
          then TestPassed
          else TestFailed
   in consideredStatus

testStatusMatchesExpectationMode :: TestStatus -> ExpectationMode -> Bool
testStatusMatchesExpectationMode actualStatus expectationMode = case (actualStatus, expectationMode) of
  (TestPassed, ExpectPassing) -> True
  (TestFailed, ExpectFailing) -> True
  _ -> False

testRunReportExamples :: TestRunReport -> Word
testRunReportExamples = sum . NE.map testRunResultExamples . testRunReportRawResults

testRunResultExamples :: TestRunResult -> Word
testRunResultExamples TestRunResult {..} =
  fromMaybe 1 testRunResultNumTests + fromMaybe 0 testRunResultNumShrinks

testRunReportWasFlaky :: TestRunReport -> Bool
testRunReportWasFlaky =
  (> 1)
    . length
    . NE.group
    . NE.map testRunResultStatus
    . testRunReportRawResults

testRunReportRetries :: TestRunReport -> Maybe Word
testRunReportRetries TestRunReport {..} = case NE.length testRunReportRawResults of
  1 -> Nothing
  l -> Just $ fromIntegral l
