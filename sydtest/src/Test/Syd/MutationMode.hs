module Test.Syd.MutationMode (runMutationMode) where

import System.IO (hPutStrLn, stderr)
import Test.Syd.Def
import Test.Syd.Mutation.Runtime (parseMutationId, setActiveMutation)
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

-- | Run the spec once per mutation in the manifest, in-process.
--
-- For each mutation, activates it via 'setActiveMutation', runs the suite
-- synchronously, then deactivates it. Exit-fail means the mutation was killed.
--
-- Prints "Killed: N" and "Survived: M" so the Nix report derivation can parse them.
runMutationMode :: Settings -> FilePath -> Spec -> IO ()
runMutationMode settings manifestPath spec = do
  contents <- readFile manifestPath
  let manifestLines = filter (not . null) (lines contents)
  -- [check] The forest is built once and reused across mutations. Test bodies
  -- (IO actions) are re-executed each run, so ifMutation's NOINLINE protects
  -- them. However, any values computed via runIO during spec construction are
  -- memoized and will not reflect the active mutation. If that causes incorrect
  -- results, move execTestDefM inside runOne so the spec is re-evaluated per mutation.
  specForest <- execTestDefM settings spec
  (killed, survived) <- foldl (runOne specForest) (pure (0 :: Int, 0 :: Int)) manifestLines
  putStrLn $ "Killed: " ++ show killed
  putStrLn $ "Survived: " ++ show survived
  where
    tabToSlash c = if c == '\t' then '/' else c

    mutationSettings = settings {settingThreads = Synchronous}

    runOne specForest accIO tabLine = do
      (killed, survived) <- accIO
      let activeVal = map tabToSlash tabLine
      hPutStrLn stderr $ "mutation: testing " ++ activeVal
      setActiveMutation (parseMutationId activeVal)
      timedResult <- runSpecForestSynchronously mutationSettings specForest
      setActiveMutation Nothing
      if shouldExitFail mutationSettings (timedValue timedResult)
        then pure (killed + 1, survived)
        else pure (killed, survived + 1)
