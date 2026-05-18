{-# LANGUAGE TypeApplications #-}

module Test.Syd.MutationIdSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.Runtime
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @MutationId

  describe "renderMutationId / parseMutationId roundtrip" $
    it "roundtrips valid MutationIds" $
      forAllValid $ \mid_ ->
        parseMutationId (renderMutationId mid_) `shouldBe` Just mid_
