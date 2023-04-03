{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Runner.Synchronous.Separate (runSpecForestSynchronously) where

import Control.Exception
import Control.Monad.Reader
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.Runner.Single
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest

runSpecForestSynchronously :: Settings -> TestForest '[] () -> IO ResultForest
runSpecForestSynchronously settings testForest =
  extractNext
    <$> runReaderT
      (goForest testForest)
      Env
        { eRetries = settingRetries settings,
          eFlakinessMode = MayNotBeFlaky,
          eExpectationMode = ExpectPassing,
          eExternalResources = HNil
        }
  where
    goForest :: forall a. TestForest a () -> R a (Next ResultForest)
    goForest [] = pure (Continue [])
    goForest (tt : rest) = do
      nrt <- goTree tt
      case nrt of
        Continue rt -> do
          nf <- goForest rest
          pure $ (rt :) <$> nf
        Stop rt -> pure $ Stop [rt]

    goTree :: forall a. TestTree a () -> R a (Next ResultTree)
    goTree = \case
      DefSpecifyNode t td () -> do
        Env {..} <- ask
        result <-
          liftIO $
            timeItT 0 $
              runSingleTestWithFlakinessMode
                noProgressReporter
                eExternalResources
                td
                eRetries
                eFlakinessMode
                eExpectationMode
        let td' = td {testDefVal = result}
        let r = failFastNext settings td'
        pure $ SpecifyNode t <$> r
      DefPendingNode t mr -> pure $ Continue $ PendingNode t mr
      DefDescribeNode t sdf -> fmap (DescribeNode t) <$> goForest sdf
      DefWrapNode func sdf -> do
        e <- ask
        liftIO $ fmap SubForestNode <$> applySimpleWrapper'' func (runReaderT (goForest sdf) e)
      DefBeforeAllNode func sdf -> do
        fmap SubForestNode
          <$> ( do
                  b <- liftIO func
                  withReaderT
                    (\e -> e {eExternalResources = HCons b (eExternalResources e)})
                    (goForest sdf)
              )
      DefAroundAllNode func sdf -> do
        e <- ask
        liftIO $
          fmap SubForestNode
            <$> applySimpleWrapper'
              func
              ( \b ->
                  runReaderT
                    (goForest sdf)
                    (e {eExternalResources = HCons b (eExternalResources e)})
              )
      DefAroundAllWithNode func sdf -> do
        e <- ask
        let HCons x _ = eExternalResources e
        liftIO $
          fmap SubForestNode
            <$> applySimpleWrapper
              func
              ( \b ->
                  runReaderT
                    (goForest sdf)
                    (e {eExternalResources = HCons b (eExternalResources e)})
              )
              x
      DefAfterAllNode func sdf -> do
        e <- ask
        let externalResources = eExternalResources e
        liftIO $ fmap SubForestNode <$> (runReaderT (goForest sdf) e `finally` func externalResources)
      DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest sdf -- Ignore, it's synchronous anyway
      DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest sdf -- Ignore, randomisation has already happened.
      DefRetriesNode modRetries sdf ->
        fmap SubForestNode
          <$> withReaderT
            (\e -> e {eRetries = modRetries (eRetries e)})
            (goForest sdf)
      DefFlakinessNode fm sdf ->
        fmap SubForestNode
          <$> withReaderT
            (\e -> e {eFlakinessMode = fm})
            (goForest sdf)
      DefExpectationNode em sdf ->
        fmap SubForestNode
          <$> withReaderT
            (\e -> e {eExpectationMode = em})
            (goForest sdf)

type R a = ReaderT (Env a) IO

-- Not exported, on purpose.
data Env externalResources = Env
  { eRetries :: !Word,
    eFlakinessMode :: !FlakinessMode,
    eExpectationMode :: !ExpectationMode,
    eExternalResources :: !(HList externalResources)
  }
