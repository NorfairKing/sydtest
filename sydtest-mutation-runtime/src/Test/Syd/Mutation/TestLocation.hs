{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.TestLocation
  ( TestLocation (..),
    encodeTestLocations,
    decodeTestLocations,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import GHC.Generics (Generic)
import Path
import Test.Syd.Mutation.Manifest (relFileCodec)
import Test.Syd.Mutation.TestId (TestId)

-- | The source location of one leaf test's @it@\/@prop@\/@specify@ call site,
-- as printed by the suite's @--mutation-coverage-list-locations@ mode and read
-- by the diff-scoped runner to map a changed test-source line back to the
-- tests defined there.
data TestLocation = TestLocation
  { testLocationTestId :: !TestId,
    testLocationFile :: !(Path Rel File),
    testLocationLine :: !Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec TestLocation)

instance HasCodec TestLocation where
  codec =
    object "TestLocation" $
      TestLocation
        <$> requiredField' "test_id" .= testLocationTestId
        <*> requiredFieldWith' "file" relFileCodec .= testLocationFile
        <*> requiredField' "line" .= testLocationLine

instance Validity TestLocation

instance GenValid TestLocation where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- | Encode a list of 'TestLocation's as a JSON array.
encodeTestLocations :: [TestLocation] -> LB.ByteString
encodeTestLocations = Aeson.encode

-- | Decode a JSON array of 'TestLocation's from the bytes of a
-- @\<suite\>.json@ listing.  Returns 'Nothing' on a decode failure.
decodeTestLocations :: B.ByteString -> Maybe [TestLocation]
decodeTestLocations = Aeson.decodeStrict
