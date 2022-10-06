{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.ExpectationSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  expectFailing $ do
    it "marks this as passing, because it fails" False
    expectPassing $ it "marks this as passing" True
