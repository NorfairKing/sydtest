{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a mutation or coverage child measures of itself and hands back to
-- the parent.
--
-- The parent already knows how long a child took in wall-clock terms, but
-- that number lumps three very different costs together: spawning the
-- process and building the spec forest, setting up the suite's resources,
-- and running the tests.  Only the child can tell those apart, so it writes
-- this record to the file the parent names with @--mutation-timing-output@.
module Test.Syd.Mutation.Timing
  ( ChildTiming (..),
    readChildTimingFileIfExists,
    writeChildTimingFile,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.Word (Word64)
import GHC.Generics (Generic)
import Path
import Path.IO (doesFileExist)

-- | A child's own account of where its time went.
data ChildTiming = ChildTiming
  { -- | Nanoseconds spent inside the spec-forest run: the suite's
    -- @around@\/@aroundAll@ setup and teardown plus the tests themselves.
    -- Everything the child does outside this (RTS startup, reading the
    -- augmented manifest, building the spec forest, printing output) is the
    -- difference between this and the wall time the parent measured.
    childTimingForestNanos :: !Word64,
    -- | Nanoseconds summed over the leaf tests that ran.  The difference
    -- between this and 'childTimingForestNanos' is resource setup and
    -- teardown, which a mutation run pays once per child rather than once
    -- per suite.
    childTimingTestNanos :: !Word64,
    -- | How many leaf tests ran.  Under the mutation child's fail-fast this
    -- can be fewer than the mutation's covering-test count.
    childTimingTestsRun :: !Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec ChildTiming)

instance Validity ChildTiming

instance GenValid ChildTiming where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec ChildTiming where
  codec =
    object "ChildTiming" $
      ChildTiming
        <$> requiredField' "forest_nanos" .= childTimingForestNanos
        <*> requiredField' "test_nanos" .= childTimingTestNanos
        <*> requiredField' "tests_run" .= childTimingTestsRun

writeChildTimingFile :: FilePath -> ChildTiming -> IO ()
writeChildTimingFile path t = LB.writeFile path (encodeJSONViaCodec t)

-- | Read a child's timing file, returning the aeson error message on a parse
-- failure.
readChildTimingFile :: FilePath -> IO (Either String ChildTiming)
readChildTimingFile path =
  eitherDecodeJSONViaCodec . LB.fromStrict <$> SB.readFile path

-- | Read a child's timing file, returning 'Nothing' when it is absent or
-- unreadable.
--
-- A child that was killed on its timeout, or that crashed before it got as
-- far as writing, leaves no file behind.  Timing is diagnostic, so the
-- parent records the run without an inner breakdown rather than failing.
readChildTimingFileIfExists :: Path Abs File -> IO (Maybe ChildTiming)
readChildTimingFileIfExists path = do
  exists <- doesFileExist path
  if exists
    then either (const Nothing) Just <$> readChildTimingFile (fromAbsFile path)
    else pure Nothing
