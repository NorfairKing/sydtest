module Test.Syd.Path where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Path
import Path.IO
import System.IO
import Test.Syd.Def.SetupFunc
import Test.Syd.Def.TestDefM

-- | A test suite that has access to a temporary directory
tempDirSpec ::
  -- | Temporary directory name template
  String ->
  TestDefM outers (Path Abs Dir) result ->
  TestDefM outers () result
tempDirSpec template = setupAround $ tempDirSetupFunc template

-- | Setup function for a temporary directory
tempDirSetupFunc ::
  -- | Temporary directory name template
  String ->
  SetupFunc (Path Abs Dir)
tempDirSetupFunc template = makeSimpleSetupFunc $ withSystemTempDir template

tempBinaryFileWithContentsSetupFunc ::
  -- | Temporary directory name template
  String ->
  ByteString ->
  SetupFunc (Path Abs File)
tempBinaryFileWithContentsSetupFunc template contents =
  makeSimpleSetupFunc $ \func ->
    withSystemTempFile
      template
      ( \af h -> do
          SB.hPut h contents
          hFlush h
          hClose h
          func af
      )
