module Test.Syd.Def.Scenario (scenarioDir, scenarioDirRecur, scenarioDirOfDirs) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Path
import Path.IO
import qualified System.FilePath as FP
import Test.Syd.Def.Specify
import Test.Syd.Def.TestDefM
import Test.Syd.Expectation

-- | Define a test for each file in the given directory.
--
-- Subdirectories are ignored, use 'scenarioDirRecur' to descend into them or
-- 'scenarioDirOfDirs' to treat each of them as a scenario.
--
-- If the directory is empty or absent, this defines a single failing test
-- instead, because that usually means the scenario files were omitted by
-- accident.
--
-- Example:
--
-- >   scenarioDir "test_resources/even" $ \fp ->
-- >     it "contains an even number" $ do
-- >       s <- readFile fp
-- >       n <- readIO s
-- >       (n :: Int) `shouldSatisfy` even
scenarioDir :: FilePath -> (FilePath -> TestDefM outers inner ()) -> TestDefM outers inner ()
scenarioDir = scenarioDirHelper "files" (fmap (map fromRelFile . snd) . listDirRel)

-- | Define a test for each file in the given directory, recursively.
--
-- If the directory contains no files, or is absent, this defines a single
-- failing test instead, because that usually means the scenario files were
-- omitted by accident.
--
-- Example:
--
-- >   scenarioDirRecur "test_resources/odd" $ \fp ->
-- >     it "contains an odd number" $ do
-- >       s <- readFile fp
-- >       n <- readIO s
-- >       (n :: Int) `shouldSatisfy` odd
scenarioDirRecur :: FilePath -> (FilePath -> TestDefM outers inner ()) -> TestDefM outers inner ()
scenarioDirRecur = scenarioDirHelper "files" (fmap (map fromRelFile . snd) . listDirRecurRel)

-- | Define a test for each subdirectory of the given directory.
--
-- Use this when a single scenario consists of more than one file.  Files in
-- the given directory itself are ignored, and so is any nesting below the
-- subdirectories: each subdirectory is one scenario, whatever it contains.
--
-- If there are no subdirectories, or the directory is absent, this defines a
-- single failing test instead, because that usually means the scenarios were
-- omitted by accident.
--
-- Example:
--
-- >   scenarioDirOfDirs "test_resources/same" $ \fp ->
-- >     it "contains two files with the same contents" $ do
-- >       a <- readFile (fp </> "a")
-- >       b <- readFile (fp </> "b")
-- >       a `shouldBe` b
scenarioDirOfDirs :: FilePath -> (FilePath -> TestDefM outers inner ()) -> TestDefM outers inner ()
scenarioDirOfDirs =
  scenarioDirHelper
    "directories"
    (fmap (map (FP.dropTrailingPathSeparator . fromRelDir) . fst) . listDirRel)

scenarioDirHelper ::
  -- | What the lister looks for, for the description of the failing test that
  -- an empty scenario directory produces.
  String ->
  -- | The scenarios, relative to the given directory
  (Path Abs Dir -> IO [FilePath]) ->
  FilePath ->
  (FilePath -> TestDefM outers inner ()) ->
  TestDefM outers inner ()
scenarioDirHelper noun lister dp func =
  describe dp $ do
    ad <- liftIO $ resolveDir' dp
    ss <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ lister ad
    if null ss
      then it (unwords ["has scenario", noun]) $ \_ ->
        (expectationFailure $ unwords ["No scenario", noun, "found in", dp] :: IO ())
      else forM_ ss $ \s ->
        describe s $ func (dp FP.</> s)
