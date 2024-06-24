{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.DescriptionsSpec (spec) where

import Test.Syd

spec :: Spec
spec =
  describe "foo" $
    describe "bar" $
      describe "quux" $ do
        descs <- getTestDescriptionPath
        it "gets the right descriptions" $
          descs `shouldBe` ["quux", "bar", "foo", "Test.Syd.DescriptionsSpec"]
