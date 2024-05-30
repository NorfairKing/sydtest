{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Autodocodec
  ( -- * Golden tests
    goldenYamlSchemaFileVia,
    pureGoldenYamlSchemaFileVia,
    pureGoldenYamlSchemaFileViaCodec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Data.Text (Text)
import Test.Syd

-- | Test that the Yaml schema of the produced 'Codec' is the same as what we find in the given golden file.
goldenYamlSchemaFileVia :: FilePath -> IO (ValueCodec input output) -> GoldenTest Text
goldenYamlSchemaFileVia fp produceCodec = goldenTextFile fp (renderColouredSchemaVia <$> produceCodec)

-- | Test that the Yaml schema of the given 'Codec' is the same as what we find in the given golden file.
pureGoldenYamlSchemaFileVia :: FilePath -> ValueCodec input output -> GoldenTest Text
pureGoldenYamlSchemaFileVia fp c = goldenYamlSchemaFileVia fp $ pure c

-- | Test that the Yaml schema of the 'Codec' of the given type is the same as what we find in the given golden file.
pureGoldenYamlSchemaFileViaCodec :: forall a. (HasCodec a) => FilePath -> GoldenTest Text
pureGoldenYamlSchemaFileViaCodec fp = pureGoldenYamlSchemaFileVia fp (codec @a)
