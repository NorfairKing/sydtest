{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syd.Hedgehog (fromHedgehogGroup) where

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
  runTest = runHedgehogProperty

runHedgehogProperty :: Hedgehog.Property -> Syd.TestRunSettings -> ((() -> () -> IO ()) -> IO ()) -> IO TestRunResult
runHedgehogProperty hedgehogProp TestRunSettings {..} wrapper = do
  let config =
        (Hedgehog.propertyConfig hedgehogProp)
          { Hedgehog.propertyDiscardLimit = Hedgehog.DiscardLimit testRunSettingMaxDiscardRatio,
            Hedgehog.propertyShrinkLimit = Hedgehog.ShrinkLimit testRunSettingMaxShrinks
          }
  let size = Hedgehog.Size testRunSettingMaxSize
  seed <- case testRunSettingSeed of
    RandomSeed -> Seed.random
    FixedSeed i -> pure $ Seed.from (fromIntegral i)
  errOrReport <- applyWrapper2 wrapper $ \() () ->
    Hedgehog.checkReport
      config
      size
      seed
      (Hedgehog.propertyTest hedgehogProp)
      (\_ -> pure ()) -- Don't report progress
  ( testRunResultStatus,
    testRunResultException,
    testRunResultNumTests,
    testRunResultLabels,
    testRunResultNumShrinks,
    testRunResultFailingInputs
    ) <- case errOrReport of
    Left e -> pure (TestFailed, Just e, Nothing, Nothing, Nothing, [])
    Right report -> do
      let Hedgehog.TestCount testCountInt = Hedgehog.reportTests report
          numTests = Just $ fromIntegral testCountInt
          labelList =
            M.toList
              . Hedgehog.coverageLabels
              $ Hedgehog.reportCoverage report

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
      case Hedgehog.reportStatus report of
        Hedgehog.OK -> pure (TestPassed, Nothing, numTests, labels, Nothing, [])
        Hedgehog.GaveUp -> pure (TestFailed, Nothing, numTests, labels, Nothing, [])
        Hedgehog.Failed failureReport -> do
          s <-
            Hedgehog.renderResult
              Hedgehog.EnableColor
              Nothing
              report
          let Hedgehog.ShrinkCount shrinkCountInt = Hedgehog.failureShrinks failureReport
              numShrinks = Just $ fromIntegral shrinkCountInt
              exception = Just $ Left s
              inputs = map Hedgehog.failedValue $ Hedgehog.failureAnnotations failureReport
          pure (TestFailed, exception, numTests, labels, numShrinks, inputs) -- TODO
  let testRunResultRetries = Nothing
  let testRunResultGoldenCase = Nothing
  let testRunResultExtraInfo = Nothing
  let testRunResultClasses = Nothing
  let testRunResultTables = Nothing
  let testRunResultFlakinessMessage = Nothing

  pure TestRunResult {..}
