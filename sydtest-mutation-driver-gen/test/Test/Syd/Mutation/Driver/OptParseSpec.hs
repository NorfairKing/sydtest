{-# LANGUAGE TypeApplications #-}

module Test.Syd.Mutation.Driver.OptParseSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.Driver.Gen ()
import Test.Syd.Mutation.Driver.OptParse
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "SuiteConfig" $ do
    genValidSpec @SuiteConfig
    jsonSpec @SuiteConfig
  describe "MutationDriverConfig" $ do
    genValidSpec @MutationDriverConfig
    jsonSpec @MutationDriverConfig
