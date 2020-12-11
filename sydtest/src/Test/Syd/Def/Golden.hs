module Test.Syd.Def.Golden where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import Test.Syd.Run
import Text.Show.Pretty

-- | Test that the given bytestring is the same as what we find in the given golden file.
pureGoldenByteStringFile :: FilePath -> ByteString -> GoldenTest ByteString
pureGoldenByteStringFile fp bs = goldenByteStringFile fp (pure bs)

-- | Test that the produced bytestring is the same as what we find in the given golden file.
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

-- | Test that the given text is the same as what we find in the given golden file.
pureGoldenTextFile :: FilePath -> Text -> GoldenTest Text
pureGoldenTextFile fp bs = goldenTextFile fp (pure bs)

-- | Test that the produced text is the same as what we find in the given golden file.
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

-- | Test that the given string is the same as what we find in the given golden file.
pureGoldenStringFile :: FilePath -> String -> GoldenTest String
pureGoldenStringFile fp bs = goldenStringFile fp (pure bs)

-- | Test that the produced string is the same as what we find in the given golden file.
goldenStringFile :: FilePath -> IO String -> GoldenTest String
goldenStringFile fp produceBS =
  GoldenTest
    { goldenTestRead = do
        resolvedFile <- resolveFile' fp
        forgivingAbsence $ fmap T.unpack $ TE.decodeUtf8 <$> SB.readFile (fromAbsFile resolvedFile),
      goldenTestProduce = produceBS,
      goldenTestWrite = \actual -> do
        resolvedFile <- resolveFile' fp
        ensureDir $ parent resolvedFile
        SB.writeFile (fromAbsFile resolvedFile) (TE.encodeUtf8 (T.pack actual)),
      goldenTestCompare = (==)
    }

-- | Test that the show instance has not changed for the given value.
goldenShowInstance :: Show a => FilePath -> a -> GoldenTest String
goldenShowInstance fp a = pureGoldenStringFile fp (show a)

-- | Test that the show instance has not changed for the given value, via `ppShow`.
goldenPrettyShowInstance :: Show a => FilePath -> a -> GoldenTest String
goldenPrettyShowInstance fp a = pureGoldenStringFile fp (ppShow a)
