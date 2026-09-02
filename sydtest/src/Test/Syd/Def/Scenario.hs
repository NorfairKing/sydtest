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
-- The scenario is given relative to the directory, so it is the name to say the
-- test is about and joining it to the directory is what reads it. Neither has to
-- be recovered from the other.
--
-- Example:
--
-- >   let dir = [reldir|test_resources/even|]
-- >   scenarioDir dir $ \rf ->
-- >     it "contains an even number" $ do
-- >       s <- readFile (fromRelFile (dir </> rf))
-- >       n <- readIO s
-- >       (n :: Int) `shouldSatisfy` even
scenarioDir :: Path b Dir -> (Path Rel File -> TestDefM outers inner ()) -> TestDefM outers inner ()
scenarioDir = scenarioDirHelper "files" (fmap snd . listDirRel)

-- | Define a test for each file in the given directory, recursively.
--
-- If the directory contains no files, or is absent, this defines a single
-- failing test instead, because that usually means the scenario files were
-- omitted by accident.
--
-- Example:
--
-- >   let dir = [reldir|test_resources/odd|]
-- >   scenarioDirRecur dir $ \rf ->
-- >     it "contains an odd number" $ do
-- >       s <- readFile (fromRelFile (dir </> rf))
-- >       n <- readIO s
-- >       (n :: Int) `shouldSatisfy` odd
scenarioDirRecur :: Path b Dir -> (Path Rel File -> TestDefM outers inner ()) -> TestDefM outers inner ()
scenarioDirRecur = scenarioDirHelper "files" (fmap snd . listDirRecurRel)

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
-- >   let dir = [reldir|test_resources/same|]
-- >   scenarioDirOfDirs dir $ \rd ->
-- >     it "contains two files with the same contents" $ do
-- >       a <- readFile (fromRelFile (dir </> rd </> [relfile|a|]))
-- >       b <- readFile (fromRelFile (dir </> rd </> [relfile|b|]))
-- >       a `shouldBe` b
scenarioDirOfDirs :: Path b Dir -> (Path Rel Dir -> TestDefM outers inner ()) -> TestDefM outers inner ()
scenarioDirOfDirs = scenarioDirHelper "directories" (fmap fst . listDirRel)

scenarioDirHelper ::
  -- | What the lister looks for, for the description of the failing test that
  -- an empty scenario directory produces.
  String ->
  -- | The scenarios, relative to the given directory
  (Path Abs Dir -> IO [Path Rel t]) ->
  Path b Dir ->
  (Path Rel t -> TestDefM outers inner ()) ->
  TestDefM outers inner ()
scenarioDirHelper noun lister dir func =
  describe (described dir) $ do
    ad <- liftIO $ makeAbsolute dir
    ss <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ lister ad
    if null ss
      then it (unwords ["has scenario", noun]) $ \_ ->
        (expectationFailure $ unwords ["No scenario", noun, "found in", described dir] :: IO ())
      else forM_ ss $ \s ->
        describe (described s) $ func s

-- | A path as a test description.
--
-- The separator a directory's rendering ends in comes off, so that a scenario
-- reads the same whether it is a file or a directory, and so that these
-- descriptions are the ones they were before the scenarios became typed.
described :: Path b t -> String
described = FP.dropTrailingPathSeparator . toFilePath
