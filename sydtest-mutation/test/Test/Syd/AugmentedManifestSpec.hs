{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Syd.AugmentedManifestSpec (spec) where

import Data.GenValidity
import Test.QuickCheck
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

instance Validity MutationId where
  validate (MutationId parts) =
    declare "parts is non-empty" (not (null parts))

instance GenValid MutationId where
  genValid = MutationId <$> listOf1 (listOf1 arbitraryPrintableChar)
  shrinkValid (MutationId parts) =
    [MutationId parts' | parts' <- shrinkList shrink parts, not (null parts')]

instance Validity AugmentedMutationRecord where
  validate r =
    mconcat
      [ validate (augmentedMutationRecordId r),
        declare "operator is non-empty" (not (null (augmentedMutationRecordOperator r)))
      ]

-- | 'augmentedMutationRecordCoveringTests' is always empty in generated values.
-- TestId JSON roundtrips are covered by Test.Syd.MutationSpec.
instance GenValid AugmentedMutationRecord where
  genValid =
    AugmentedMutationRecord
      <$> genValid
      <*> listOf1 arbitraryPrintableChar
      <*> listOf arbitraryPrintableChar
      <*> listOf arbitraryPrintableChar
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing
      <*> pure []
      <*> pure []
      <*> pure []
  shrinkValid _ = []

spec :: Spec
spec = do
  describe "AugmentedMutationRecord" $ do
    genValidSpec @AugmentedMutationRecord
    jsonSpec @AugmentedMutationRecord
