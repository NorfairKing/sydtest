{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.ManifestSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SB
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

  describe "MutationManifest forward compatibility" $
    it "decodes a manifest written before optional fields were added" $ do
      bytes <- SB.readFile "test_resources/legacy-mutation-manifest.json"
      case Aeson.eitherDecodeStrict bytes of
        Left err -> expectationFailure $ "failed to decode legacy manifest: " ++ err
        Right (MutationManifest groups) -> case groups of
          [MutationGroup [r]] -> do
            -- Fields that were absent should decode to their documented defaults.
            mutRecEndLine r `shouldBe` 0
            mutRecSourceFile r `shouldBe` Nothing
            mutRecSourceLines r `shouldBe` []
            mutRecMutatedLines r `shouldBe` []
            mutRecContextBefore r `shouldBe` []
            mutRecContextAfter r `shouldBe` []
            mutRecCoveringTests r `shouldBe` Nothing
          _ -> expectationFailure $ "unexpected manifest shape: " ++ show groups
