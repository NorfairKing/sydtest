module Test.Syd.Path where

import Path
import Path.IO
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
  SetupFunc () (Path Abs Dir)
tempDirSetupFunc template = makeSimpleSetupFunc $ withSystemTempDir template
