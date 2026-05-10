{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.MutationMode (runMutationMode) where

import Data.Aeson (Value, decode, withArray, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LB
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Test.Syd.Def
import Test.Syd.Mutation.Runtime (MutationId (..), renderMutationId, setActiveMutation)
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

-- | Run the spec once per mutation in the manifest directory, in-process.
--
-- For each mutation, activates it via 'setActiveMutation', runs the suite
-- synchronously, then deactivates it. Exit-fail means the mutation was killed.
--
-- Prints "Killed: N" and "Survived: M" so the Nix report derivation can parse them.
runMutationMode :: Settings -> FilePath -> Spec -> IO ()
runMutationMode settings manifestDir spec = do
  allFiles <- listDirectory manifestDir
  let jsonFiles = filter (".json" `isSuffixOf`) allFiles
  mutations <- concat <$> mapM (readMutationsFromFile manifestDir) jsonFiles
  -- [check] The forest is built once and reused across mutations. Test bodies
  -- (IO actions) are re-executed each run, so ifMutation's NOINLINE protects
  -- them. However, any values computed via runIO during spec construction are
  -- memoized and will not reflect the active mutation. If that causes incorrect
  -- results, move execTestDefM inside runOne so the spec is re-evaluated per mutation.
  specForest <- execTestDefM settings spec
  (killed, survived) <- foldl (runOne specForest) (pure (0 :: Int, 0 :: Int)) mutations
  putStrLn $ "Killed: " ++ show killed
  putStrLn $ "Survived: " ++ show survived
  where
    mutationSettings = settings {settingThreads = Synchronous}

    runOne specForest accIO mid = do
      (killed, survived) <- accIO
      let activeVal = renderMutationId mid
      hPutStrLn stderr $ "mutation: testing " ++ activeVal
      setActiveMutation (Just mid)
      timedResult <- runSpecForestSynchronously mutationSettings specForest
      setActiveMutation Nothing
      if shouldExitFail mutationSettings (timedValue timedResult)
        then pure (killed + 1, survived)
        else pure (killed, survived + 1)

-- | Read all 'MutationId's from a single JSON manifest file.
readMutationsFromFile :: FilePath -> FilePath -> IO [MutationId]
readMutationsFromFile dir fileName = do
  bs <- LB.readFile (dir </> fileName)
  case decode bs of
    Nothing -> do
      hPutStrLn stderr $ "mutation: failed to decode " ++ fileName
      pure []
    Just val -> pure (parseMutationIds val)

parseMutationIds :: Value -> [MutationId]
parseMutationIds val = fromMaybe [] (parseMaybe parseArray val)
  where
    parseArray = withArray "mutations" $ \arr ->
      mapM parseEntry (foldr (:) [] arr)
    parseEntry = withObject "mutation" $ \o -> do
      parts <- o .: "id"
      pure (MutationId parts)
