{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Hspec (fromHspec) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Writer
import Data.List
import qualified Test.Hspec.Core.Spec as Hspec
import Test.QuickCheck
import Test.Syd as Syd

-- | Import an Hspec 'Test.Hspec.Spec' as a Sydtest 'Test.Syd.Spec'.
--
-- The reasoning behind this function is that, eventhough migration from hspec
-- to sydtest is usually very simple, you might depend on certain libraries
-- beyond your control that still use hspec.  In that case you want to be able
-- to still use those libraries but also use sydtest already.
--
-- For this reason, and because hspec doesn't tell you wether a test is pending
-- until after you run it, pending tests are imported as passing tests.
fromHspec :: Hspec.Spec -> Syd.Spec
fromHspec spec = do
  trees <- liftIO $ runSpecM_ spec
  -- We have to use 'doNotRandomiseExecutionOrder' and 'sequential' because otherwise
  -- passing hspec tests would stop working when imported into sydtest
  doNotRandomiseExecutionOrder $ sequential $ mapM_ importSpecTree trees

runSpecM_ :: Hspec.SpecWith () -> IO [Hspec.SpecTree ()]
#if MIN_VERSION_hspec_core(2,10,1)
runSpecM_ = fmap snd . Hspec.runSpecM
#else
runSpecM_ = Hspec.runSpecM
#endif

-- Hspec.NodeWithCleanup's semantics are so weird that we can only do
-- this translation if inner equals ().
importSpecTree :: Hspec.SpecTree () -> Syd.Spec
importSpecTree = go
  where
    go :: Hspec.SpecTree () -> Syd.Spec
    go = \case
      Hspec.Leaf item -> importItem item
      Hspec.Node d ts -> describe d $ mapM_ go ts
#if MIN_VERSION_hspec_core(2,10,1)
      Hspec.NodeWithCleanup _ cleanup ts -> afterAll_ cleanup (mapM_ go ts)
#else
#if MIN_VERSION_hspec_core(2,8,0)
      Hspec.NodeWithCleanup _ cleanup ts -> afterAll_ (cleanup ()) (mapM_ go ts)
#else
      Hspec.NodeWithCleanup cleanup ts ->   afterAll_ (cleanup ()) (mapM_ go ts)
#endif
#endif

importItem :: forall inner. Hspec.Item inner -> Syd.TestDefM '[] inner ()
importItem item@Hspec.Item {..} =
  let parallelMod = case itemIsParallelizable of
        Just True -> parallel
        Just False -> sequential
        Nothing -> id
   in parallelMod $
        it itemRequirement (ImportedItem item :: ImportedItem inner)

newtype ImportedItem a = ImportedItem (Hspec.Item a)

instance IsTest (ImportedItem a) where
  type Arg1 (ImportedItem a) = ()
  type Arg2 (ImportedItem a) = a
  runTest = runImportedItem

applyWrapper2' ::
  forall r outerArgs innerArg.
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  (outerArgs -> innerArg -> IO r) ->
  IO r
applyWrapper2' wrapper func = do
  var <- newEmptyMVar
  wrapper $ \outerArgs innerArg -> do
    res <- func outerArgs innerArg >>= evaluate
    putMVar var res
  readMVar var

runImportedItem ::
  ImportedItem inner ->
  TestRunSettings ->
  ProgressReporter ->
  ((() -> inner -> IO ()) -> IO ()) ->
  IO TestRunResult
runImportedItem (ImportedItem Hspec.Item {..}) trs progressReporter wrapper = do
  let report = reportProgress progressReporter
  let qcargs = makeQuickCheckArgs trs
  let params :: Hspec.Params
      params =
        Hspec.Params
          { Hspec.paramsQuickCheckArgs = qcargs,
            -- TODO use the right depth when sydtest supports smallcheck
            Hspec.paramsSmallCheckDepth = Hspec.paramsSmallCheckDepth Hspec.defaultParams
          }
      callback :: Hspec.ProgressCallback
      callback = const $ pure ()
  exampleCounter <- newTVarIO 1
  let totalExamples = (fromIntegral :: Int -> Word) (maxSuccess qcargs)
  -- There's no real nice way to do progress reporting here because:
  --   * hspec does not tell us whether we're using a property or not
  --   * we could use the 'callback' above, but then we cannot time the examples.
  --
  -- The tradeoff that we are making is that the output is more verbose:
  -- You'll see 'ProgressExampleStarting' even for unit tests, but at least the
  -- examples in a property test are timed.
  report ProgressTestStarting
  result <-
    itemExample
      params
      ( \takeInner -> applyWrapper2' wrapper $ \() inner -> do
          exampleNr <- readTVarIO exampleCounter
          report $ ProgressExampleStarting totalExamples exampleNr
          (result, duration) <- timeItDuration $ takeInner inner
          report $ ProgressExampleDone totalExamples exampleNr duration
          atomically $ modifyTVar' exampleCounter succ
          pure result
      )
      callback
  report ProgressTestDone
  let (testRunResultStatus, testRunResultException) = case Hspec.resultStatus result of
        Hspec.Success -> (TestPassed, Nothing)
        -- This is certainly a debatable choice, but there's no need to make
        -- tests fail here, and there's no way to know ahead of time whether
        -- a test is pending so we have no choice.
        Hspec.Pending _ _ -> (TestPassed, Nothing)
        Hspec.Failure mloc fr ->
          let withExtraContext :: Maybe String -> SomeException -> SomeException
              withExtraContext = maybe id (\extraContext se -> SomeException $ addContextToException se extraContext)
              niceLocation :: Hspec.Location -> String
              niceLocation Hspec.Location {..} = intercalate ":" [locationFile, show locationLine, show locationColumn]
              withLocationContext :: SomeException -> SomeException
              withLocationContext = withExtraContext $ niceLocation <$> mloc
              exception = failureReasonToException withExtraContext fr
           in ( TestFailed,
                Just $ SomeException $ addContextToException (withLocationContext exception) (Hspec.resultInfo result)
              )
  let testRunResultNumTests = Nothing
  let testRunResultNumShrinks = Nothing
  let testRunResultGoldenCase = Nothing
  let testRunResultFailingInputs = []
  let testRunResultExtraInfo = Nothing
  let testRunResultLabels = Nothing
  let testRunResultClasses = Nothing
  let testRunResultTables = Nothing

  pure TestRunResult {..}

failureReasonToException :: (Maybe String -> SomeException -> SomeException) -> Hspec.FailureReason -> SomeException
failureReasonToException withExtraContext = \case
  Hspec.NoReason -> SomeException $ ExpectationFailed "Hspec had no more information about this failure."
  Hspec.Reason s -> SomeException $ ExpectationFailed s
  Hspec.ExpectedButGot mExtraContext expected actual -> withExtraContext mExtraContext $ SomeException $ NotEqualButShouldHaveBeenEqual actual expected
  Hspec.Error mExtraContext e -> withExtraContext mExtraContext e
#if MIN_VERSION_hspec_core(2,11,0)
  Hspec.ColorizedReason s -> SomeException $ ExpectationFailed s
#endif
