module Test.Syd.Def.Golden where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import Test.Syd.Run

-- | Test that the given bytestring is the same as what we find in the given golden file
pureGoldenByteStringFile :: FilePath -> ByteString -> GoldenTest ByteString
pureGoldenByteStringFile fp bs = goldenByteStringFile fp (pure bs)

-- | Test that the produced bytestring is the same as what we find in the given golden file
goldenByteStringFile :: FilePath -> IO ByteString -> GoldenTest ByteString
goldenByteStringFile fp produceBS =
  GoldenTest
    { goldenTestRead = do
        resolvedFile <- resolveFile' fp
        forgivingAbsence $ SB.readFile $ fromAbsFile resolvedFile,
      goldenTestProduce = produceBS,
      goldenTestWrite = \actual -> do
        resolvedFile <- resolveFile' fp
        ensureDir $ parent resolvedFile
        SB.writeFile (fromAbsFile resolvedFile) actual,
      goldenTestCompare = (==)
    }

-- | Test that the given text is the same as what we find in the given golden file
pureGoldenTextFile :: FilePath -> Text -> GoldenTest Text
pureGoldenTextFile fp bs = goldenTextFile fp (pure bs)

-- | Test that the produced text is the same as what we find in the given golden file
goldenTextFile :: FilePath -> IO Text -> GoldenTest Text
goldenTextFile fp produceBS =
  GoldenTest
    { goldenTestRead = do
        resolvedFile <- resolveFile' fp
        forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromAbsFile resolvedFile),
      goldenTestProduce = produceBS,
      goldenTestWrite = \actual -> do
        resolvedFile <- resolveFile' fp
        ensureDir $ parent resolvedFile
        SB.writeFile (fromAbsFile resolvedFile) (TE.encodeUtf8 actual),
      goldenTestCompare = (==)
    }
