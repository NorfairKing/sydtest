{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Runner.Wrappers where

import Control.Concurrent
import Control.Monad.IO.Class
import Test.Syd.Run
import Test.Syd.Run.Result
import Test.Syd.SpecDef

data Next a = Continue a | Stop a
  deriving (Functor)

extractNext :: Next a -> a
extractNext (Continue a) = a
extractNext (Stop a) = a

failFastNext :: Bool -> TDef (Timed TestRunResult) -> Next (TDef (Timed TestRunResult))
failFastNext b td@(TDef (Timed trr _) _) =
  if b && testRunResultStatus trr == TestFailed
    then Stop td
    else Continue td

applySimpleWrapper ::
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
