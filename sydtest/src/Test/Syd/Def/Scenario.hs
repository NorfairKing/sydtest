module Test.Syd.Def.Scenario (scenarioDir, scenarioDirRecur) where

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
scenarioDir = scenarioDirHelper listDirRel

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
scenarioDirRecur = scenarioDirHelper listDirRecurRel

scenarioDirHelper ::
  (Path Abs Dir -> IO ([Path Rel Dir], [Path Rel File])) ->
  FilePath ->
  (FilePath -> TestDefM outers inner ()) ->
  TestDefM outers inner ()
scenarioDirHelper lister dp func =
  describe dp $ do
    ad <- liftIO $ resolveDir' dp
    fs <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ snd <$> lister ad
    if null fs
      then it "has scenario files" $ \_ ->
        (expectationFailure $ unwords ["No scenario files found in", dp] :: IO ())
      else forM_ fs $ \rf -> do
        let fp = dp FP.</> fromRelFile rf
        describe (fromRelFile rf) $ func fp
