{-# LANGUAGE TypeApplications #-}

module Test.Syd.CoverageBaselineSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.AugmentedManifest (MutationProgressEvent)
import Test.Syd.Mutation.TestBaselineMap (TestBaselineMap)
import Test.Syd.Mutation.TestCoverageMap (TestCoverageMap)
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "TestBaselineMap" $ do
    genValidSpec @TestBaselineMap
    jsonSpec @TestBaselineMap

  describe "TestCoverageMap" $ do
    genValidSpec @TestCoverageMap
    jsonSpec @TestCoverageMap

  describe "MutationProgressEvent" $
    genValidSpec @MutationProgressEvent
