{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Runner.Wrappers where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Text (Text)
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest

data Next a = Continue a | Stop a
  deriving (Functor)

extractNext :: Next a -> a
extractNext (Continue a) = a
extractNext (Stop a) = a

failFastNext :: Settings -> TDef (Timed TestRunReport) -> Next (TDef (Timed TestRunReport))
failFastNext settings td@(TDef timed _) =
  if settingFailFast settings && testRunReportFailed settings (timedValue timed)
    then Stop td
    else Continue td

-- | What became of a group of tests run inside a resource.
--
-- Which of these it is decides what there is to report: a resource that threw
-- on the way in never reached the tests, so there are no results, whereas one
-- that threw on the way out threw after they had all run and reported.
data ResourceOutcome r
  = -- | The resource and the tests inside it both finished.
    ResourceRan r
  | -- | The tests ran, and then the resource threw.
    ResourceFailedAfter r SomeException
  | -- | The resource threw before the tests ran.
    ResourceFailedBefore SomeException
  | -- | The resource finished without ever running the tests inside it.
    --
    -- A wrapper that does not call the function it is given, which used to
    -- leave the runner waiting for results that were never coming.
    ResourceSkippedTests
  deriving (Functor)

-- | 'applySimpleWrapper'', reporting what became of the resource instead of
-- letting it throw.
applySimpleWrapperReported' ::
  ((a -> IO ()) -> IO ()) ->
  (a -> IO r) ->
  IO (ResourceOutcome r)
applySimpleWrapperReported' takeTakeA takeA = do
  var <- newEmptyMVar
  errOrUnit <- try $
    takeTakeA $ \a -> do
      r <- takeA a
      void $ tryPutMVar var r
  resourceOutcome var errOrUnit

-- | 'applySimpleWrapper', reporting what became of the resource instead of
-- letting it throw.
applySimpleWrapperReported ::
  ((a -> IO ()) -> (b -> IO ())) ->
  (a -> IO r) ->
  b ->
  IO (ResourceOutcome r)
applySimpleWrapperReported takeTakeA takeA b = do
  var <- newEmptyMVar
  errOrUnit <-
    try $
      takeTakeA
        ( \a -> do
            r <- takeA a
            void $ tryPutMVar var r
        )
        b
  resourceOutcome var errOrUnit

-- | 'applySimpleWrapper''', reporting what became of the resource instead of
-- letting it throw.
applySimpleWrapperReported'' ::
  (IO () -> IO ()) ->
  IO r ->
  IO (ResourceOutcome r)
applySimpleWrapperReported'' wrapper produceResult = do
  var <- newEmptyMVar
  errOrUnit <- try $
    wrapper $ do
      r <- produceResult
      void $ tryPutMVar var r
  resourceOutcome var errOrUnit

-- | The subtree to report for a group of tests and the resource around them.
--
-- A resource that threw after the tests ran is reported next to their results,
-- because those results are worth having: the tests ran and said what they
-- found.
--
-- A resource that threw before them is reported in front of the tests it
-- never ran, each of which is reported as having failed for that reason.  The
-- tests are in the report either way, which is what keeps a failed resource
-- from quietly shrinking the suite.
resourceSubtree :: Text -> SpecDefForest a b extra -> ResourceOutcome (Next ResultForest) -> IO (Next ResultTree)
resourceSubtree kind sdf = \case
  ResourceRan nrf -> pure $ SubForestNode <$> nrf
  ResourceFailedAfter nrf e -> pure $ (\rf -> SubForestNode (rf ++ [ResourceFailureNode kind e])) <$> nrf
  ResourceFailedBefore e -> pure $ Continue $ failedSubtree kind sdf e
  ResourceSkippedTests ->
    -- The resource ran, and never ran the tests.  Waiting for results that
    -- are not coming is what this used to do.
    pure $
      Continue $
        failedSubtree kind sdf $
          toException (ExpectationFailed "This resource finished without ever running the tests inside it.")

-- | The subtree for a resource that failed and the tests it never ran.
failedSubtree :: Text -> SpecDefForest a b extra -> SomeException -> ResultTree
failedSubtree kind sdf e =
  SubForestNode (failedTestsForest e sdf ++ [ResourceFailureNode kind e])

resourceOutcome :: MVar r -> Either SomeException () -> IO (ResourceOutcome r)
resourceOutcome var errOrUnit = do
  mResult <- tryReadMVar var
  pure $ case (errOrUnit, mResult) of
    (Right (), Just r) -> ResourceRan r
    (Right (), Nothing) -> ResourceSkippedTests
    (Left e, Just r) -> ResourceFailedAfter r e
    (Left e, Nothing) -> ResourceFailedBefore e

applySimpleWrapper2 ::
  (MonadIO m) =>
  ((a -> b -> m ()) -> (c -> d -> m ())) ->
  (a -> b -> m r) ->
  (c -> d -> m r)
applySimpleWrapper2 takeTakeAB takeAB c d = do
  var <- liftIO newEmptyMVar
  takeTakeAB
    ( \a b -> do
        r <- takeAB a b
        liftIO $ putMVar var r
    )
    c
    d
  liftIO $ readMVar var
