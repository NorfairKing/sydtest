{-# LANGUAGE TypeApplications #-}

module Test.Syd.ManifestSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.Manifest
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @MutationRecord
  jsonSpec @MutationRecord

  genValidSpec @MutationGroup
  jsonSpec @MutationGroup

  genValidSpec @MutationManifest
  jsonSpec @MutationManifest
