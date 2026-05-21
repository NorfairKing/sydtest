{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Mutation.Driver.OptParseSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.Driver.Gen ()
import Test.Syd.Mutation.Driver.OptParse
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "SuiteConfig" $
    genValidSpec @SuiteConfig

  describe "SuitePkgSpec" $
    genValidSpec @SuitePkgSpec

  describe "MutationDriverSettings" $
    genValidSpec @MutationDriverSettings

  describe "CoverageSettings" $
    genValidSpec @CoverageSettings

  describe "MergeCoverageSettings" $
    genValidSpec @MergeCoverageSettings

  describe "parseSuitePkgSpec" $ do
    it "parses PNAME=ROOT=RESOURCE_DIR" $
      parseSuitePkgSpec "mypkg=/nix/store/abc/=/nix/store/def/"
        `shouldSatisfy` (\case Right _ -> True; Left _ -> False)
    it "rejects an empty PNAME" $
      parseSuitePkgSpec "=/nix/store/abc/=/nix/store/def/"
        `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
    it "rejects when fewer than three components" $
      parseSuitePkgSpec "mypkg=/nix/store/abc/"
        `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
    it "rejects when more than three components" $
      parseSuitePkgSpec "a=/b/=/c/=extra"
        `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
    it "rejects a relative path for the built-test-pkg root" $
      parseSuitePkgSpec "mypkg=relative/path=/nix/store/def/"
        `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
    it "rejects a relative path for the resource dir" $
      parseSuitePkgSpec "mypkg=/nix/store/abc/=relative/path"
        `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
