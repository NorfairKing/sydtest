{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Runner.Wrappers where

import Control.Concurrent
import Control.Exception
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
      putMVar var r
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
            putMVar var r
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
      putMVar var r
  resourceOutcome var errOrUnit

-- | The subtree to report for a group of tests and the resource around them.
--
-- A resource that threw after the tests ran is reported next to their results,
-- because those results are worth having: the tests ran and said what they
-- found.
--
-- A resource that threw before them is re-thrown, because there are no results
-- to report it next to and the tests under it never started.
resourceSubtree :: Text -> ResourceOutcome (Next ResultForest) -> IO (Next ResultTree)
resourceSubtree kind = \case
  ResourceRan nrf -> pure $ SubForestNode <$> nrf
  ResourceFailedAfter nrf e -> pure $ (\rf -> SubForestNode (rf ++ [ResourceFailureNode kind e])) <$> nrf
  ResourceFailedBefore e -> throwIO e
  ResourceSkippedTests ->
    -- The resource ran, and never ran the tests.  Waiting for results that
    -- are not coming is what this used to do.
    pure $
      Continue $
        ResourceFailureNode
          kind
          (toException (ExpectationFailed "This resource finished without ever running the tests inside it."))

resourceOutcome :: MVar r -> Either SomeException () -> IO (ResourceOutcome r)
resourceOutcome var errOrUnit = do
  mResult <- tryReadMVar var
  pure $ case (errOrUnit, mResult) of
    (Right (), Just r) -> ResourceRan r
    (Right (), Nothing) -> ResourceSkippedTests
    (Left e, Just r) -> ResourceFailedAfter r e
    (Left e, Nothing) -> ResourceFailedBefore e

applySimpleWrapper ::
  (MonadIO m) =>
  ((a -> m ()) -> (b -> m ())) ->
  (a -> m r) ->
  (b -> m r)
applySimpleWrapper takeTakeA takeA b = do
  var <- liftIO newEmptyMVar
  takeTakeA
    ( \a -> do
        r <- takeA a
        liftIO $ putMVar var r
    )
    b
  liftIO $ readMVar var

applySimpleWrapper' ::
  (MonadIO m) =>
  ((a -> m ()) -> m ()) ->
  (a -> m r) ->
  m r
applySimpleWrapper' takeTakeA takeA = do
  var <- liftIO newEmptyMVar
  takeTakeA
    ( \a -> do
        r <- takeA a
        liftIO $ putMVar var r
    )

  liftIO $ readMVar var

applySimpleWrapper'' ::
  (MonadIO m) =>
  (m () -> m ()) ->
  m r ->
  m r
applySimpleWrapper'' wrapper produceResult = do
  var <- liftIO newEmptyMVar
  wrapper $ do
    r <- produceResult
    liftIO $ putMVar var r

  liftIO $ readMVar var

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
