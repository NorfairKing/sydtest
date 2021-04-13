{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Aeson
  ( -- * Golden tests
    goldenJSONFile,
    pureGoldenJSONFile,
    goldenJSONValueFile,
    pureGoldenJSONValueFile,
  )
where

import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding as TE
import Path
import Path.IO
import Test.Syd

-- | Test that the produced 'JSON.Value' is the same as what we find in the given golden file.
--
-- This function shows a diff based on the encoding of the values.
goldenJSONFile :: FilePath -> IO JSON.Value -> GoldenTest JSON.Value
goldenJSONFile fp produceActualValue =
  GoldenTest
    { goldenTestRead = do
        p <- resolveFile' fp
        mContents <- forgivingAbsence $ SB.readFile (fromAbsFile p)
        forM mContents $ \contents ->
          case JSON.eitherDecode (LB.fromStrict contents) of
            Left err -> expectationFailure err
            Right r -> pure r,
      goldenTestProduce = produceActualValue,
      goldenTestWrite = \v -> do
        p <- resolveFile' fp
        ensureDir (parent p)
        SB.writeFile (fromAbsFile p) $ LB.toStrict $ JSON.encodePretty v,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else
            Just
              ( Context
                  ( textsNotEqualButShouldHaveBeenEqual
                      (TE.decodeUtf8 (LB.toStrict (JSON.encodePretty actual)))
                      (TE.decodeUtf8 (LB.toStrict (JSON.encodePretty expected)))
                  )
                  (goldenContext fp)
              )
    }

-- | Test that the given 'JSON.Value' is the same as what we find in the given golden file.
--
-- This function shows a diff based on the encoding of the values.
pureGoldenJSONFile :: FilePath -> JSON.Value -> GoldenTest JSON.Value
pureGoldenJSONFile fp actualValue = goldenJSONFile fp $ pure actualValue

-- | Test that the produced 'JSON.Value' is the same as what we find in the given golden file.
--
-- This test also tests that the previously written 'toJSON'-ed version of the given value is still parseable as to same value.
--
-- This function shows a diff based on the pretty 'Show'ing of the values.
goldenJSONValueFile :: (Show a, Eq a, FromJSON a, ToJSON a) => FilePath -> IO a -> GoldenTest a
goldenJSONValueFile fp produceActualValue =
  GoldenTest
    { goldenTestRead = do
        p <- resolveFile' fp
        mContents <- forgivingAbsence $ SB.readFile (fromAbsFile p)
        forM mContents $ \contents ->
          case JSON.eitherDecode (LB.fromStrict contents) of
            Left err -> expectationFailure err
            Right r -> pure r,
      goldenTestProduce = produceActualValue,
      goldenTestWrite = \v -> do
        p <- resolveFile' fp
        ensureDir (parent p)
        SB.writeFile (fromAbsFile p) $ LB.toStrict $ JSON.encodePretty v,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else Just (Context (stringsNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)) (goldenContext fp))
    }

-- | Test that the given 'JSON.Value' is the same as what we find in the given golden file.
--
-- This test also tests that the previously written 'toJSON'-ed version of the given value is still parseable as to same value.
--
-- This function shows a diff based on the pretty 'Show'ing of the values.
pureGoldenJSONValueFile :: (Show a, Eq a, FromJSON a, ToJSON a) => FilePath -> a -> GoldenTest a
pureGoldenJSONValueFile fp actualValue = goldenJSONValueFile fp $ pure actualValue
