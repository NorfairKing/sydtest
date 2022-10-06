{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syd.Hedgehog (fromHedgehogGroup) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import qualified Hedgehog
import qualified Hedgehog.Internal.Config as Hedgehog
import qualified Hedgehog.Internal.Property as Hedgehog
import qualified Hedgehog.Internal.Report as Hedgehog
import qualified Hedgehog.Internal.Runner as Hedgehog
import qualified Hedgehog.Internal.Seed as Seed
import Test.Syd as Syd

-- | Import an Hedgehog 'Hedgehog.Group' as a Sydtest 'Test.Syd.Spec'.
--
-- The reasoning behind this function is that, eventhough migration from hedgehog
-- to sydtest is usually very simple, you might depend on certain libraries
-- beyond your control that still use hedgehog.  In that case you want to be able
-- to still use those libraries but also use sydtest already.
fromHedgehogGroup :: Hedgehog.Group -> Syd.Spec
fromHedgehogGroup hedgehogGroup = Syd.describe (Hedgehog.unGroupName $ Hedgehog.groupName hedgehogGroup) $
  forM_ (Hedgehog.groupProperties hedgehogGroup) $ \(propertyName, property) -> do
    it (Hedgehog.unPropertyName propertyName) property

instance IsTest Hedgehog.Property where
  type Arg1 Hedgehog.Property = ()
  type Arg2 Hedgehog.Property = ()
  runTest func = runHedgehogPropertyWithArg (\() () -> func)

instance IsTest (arg -> Hedgehog.Property) where
  type Arg1 (arg -> Hedgehog.Property) = ()
  type Arg2 (arg -> Hedgehog.Property) = arg
  runTest func = runHedgehogPropertyWithArg (\() a -> func a)

instance IsTest (outerArgs -> innerArg -> Hedgehog.Property) where
  type Arg1 (outerArgs -> innerArg -> Hedgehog.Property) = outerArgs
  type Arg2 (outerArgs -> innerArg -> Hedgehog.Property) = innerArg
  runTest = runHedgehogPropertyWithArg

runHedgehogPropertyWithArg ::
  forall outerArgs innerArg.
  (outerArgs -> innerArg -> Hedgehog.Property) ->
  TestRunSettings ->
  ProgressReporter ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runHedgehogPropertyWithArg
  hedgehogProp
  TestRunSettings {..}
  progressReporter
  wrapper = do
    let report = reportProgress progressReporter
    let size = Hedgehog.Size testRunSettingMaxSize
    seed <- case testRunSettingSeed of
      RandomSeed -> Seed.random
      FixedSeed i -> pure $ Seed.from (fromIntegral i)

    exampleCounter <- newTVarIO 1
    let totalExamples = (fromIntegral :: Int -> Word) testRunSettingMaxSuccess
    report ProgressTestStarting
    -- We make the same tradeoff here as in sydtest-hspec.
    -- We show ProgressExampleStarting for non-property tests as well so that
    -- we can attach timing information.
    -- In the case of hedgehog, non-property tests should be rarer so that
    -- should matter even less.
    errOrReport <- applyWrapper2 wrapper $ \outer inner -> do
      let config =
            (Hedgehog.propertyConfig (hedgehogProp outer inner))
              { Hedgehog.propertyDiscardLimit = Hedgehog.DiscardLimit testRunSettingMaxDiscardRatio,
                Hedgehog.propertyShrinkLimit = Hedgehog.ShrinkLimit testRunSettingMaxShrinks,
                Hedgehog.propertyTerminationCriteria = Hedgehog.NoConfidenceTermination $ Hedgehog.TestLimit testRunSettingMaxSuccess
              }

      Hedgehog.checkReport
        config
        size
        seed
        ( do
            exampleNr <- liftIO $ readTVarIO exampleCounter
            liftIO $ report $ ProgressExampleStarting totalExamples exampleNr
            timedResult <- timeItT $ Hedgehog.propertyTest (hedgehogProp outer inner)
            liftIO $ report $ ProgressExampleDone totalExamples exampleNr $ timedTime timedResult
            liftIO $ atomically $ modifyTVar' exampleCounter succ
            pure $ timedValue timedResult
        )
        (\_ -> pure ()) -- Don't report progress
    report ProgressTestDone
    ( testRunResultStatus,
      testRunResultException,
      testRunResultNumTests,
      testRunResultLabels,
      testRunResultNumShrinks,
      testRunResultFailingInputs
      ) <- case errOrReport of
      Left e -> pure (TestFailed, Just e, Nothing, Nothing, Nothing, [])
      Right hedgehogReport -> do
        let Hedgehog.TestCount testCountInt = Hedgehog.reportTests hedgehogReport
            numTests = Just $ fromIntegral testCountInt
            labelList =
              M.toList
                . Hedgehog.coverageLabels
                $ Hedgehog.reportCoverage hedgehogReport

            labels =
              if null labelList
                then Nothing
                else
                  Just
                    . M.fromList
                    . map
                      ( \(labelName, label) ->
                          ([Hedgehog.unLabelName labelName], Hedgehog.unCoverCount $ Hedgehog.labelAnnotation label)
                      )
                    $ labelList
        case Hedgehog.reportStatus hedgehogReport of
          Hedgehog.OK -> pure (TestPassed, Nothing, numTests, labels, Nothing, [])
          Hedgehog.GaveUp -> pure (TestFailed, Nothing, numTests, labels, Nothing, [])
          Hedgehog.Failed failureReport -> do
            s <-
              Hedgehog.renderResult
                Hedgehog.EnableColor
                Nothing
                hedgehogReport
            let Hedgehog.ShrinkCount shrinkCountInt = Hedgehog.failureShrinks failureReport
                numShrinks = Just $ fromIntegral shrinkCountInt
                exception = Just $ SomeException $ ExpectationFailed s
                inputs = map Hedgehog.failedValue $ Hedgehog.failureAnnotations failureReport
            pure (TestFailed, exception, numTests, labels, numShrinks, inputs) -- TODO
    let testRunResultGoldenCase = Nothing
    let testRunResultExtraInfo = Nothing
    let testRunResultClasses = Nothing
    let testRunResultTables = Nothing

    pure TestRunResult {..}
