{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.AugmentedManifestSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "AugmentedMutationRecord" $ do
    genValidSpec @AugmentedMutationRecord
    jsonSpec @AugmentedMutationRecord
