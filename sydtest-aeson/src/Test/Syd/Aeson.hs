{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Aeson
  ( -- * Golden tests
    goldenAesonDocumentFile,
    pureGoldenAesonDocumentFile,
  )
where

import Data.Aeson as Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Path
import Path.IO
import Test.Syd

-- | Test that the produced 'Aeson.Value' is the same as what we find in the given golden file.
goldenAesonDocumentFile :: FilePath -> IO Aeson.Value -> GoldenTest Aeson.Value
goldenAesonDocumentFile fp produceActualDocument =
  GoldenTest
    { goldenTestRead = do
        ap <- resolveFile' fp
        exists <- doesFileExist ap
        if exists
          then decodeFileStrict' fp
          else pure Nothing,
      goldenTestProduce = produceActualDocument,
      goldenTestWrite = \d -> do
        ap <- resolveFile' fp
        ensureDir (parent ap)
        SB.writeFile (fromAbsFile ap) $ LB.toStrict $ Aeson.encode d,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else Just (Context (stringsNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)) (goldenContext fp))
    }

-- | Test that the given 'Aeson.Value' is the same as what we find in the given golden file.
pureGoldenAesonDocumentFile :: FilePath -> Aeson.Value -> GoldenTest Aeson.Value
pureGoldenAesonDocumentFile fp actualDocument = goldenAesonDocumentFile fp $ pure actualDocument
