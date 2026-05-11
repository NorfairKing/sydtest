{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.MutationMode (runMutationMode, formatMutationLog) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Path
import System.IO (hPutStrLn, stderr)
import Test.Syd.Def
import Test.Syd.Mutation.Manifest (MutationManifest (..), MutationRecord (..), readManifestDir)
import Test.Syd.Mutation.Runtime (MutationId (..), setActiveMutation)
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

-- | Run the spec once per mutation in the manifest directories, in-process.
--
-- For each mutation, activates it via 'setActiveMutation', runs the suite
-- synchronously, then deactivates it. Exit-fail means the mutation was killed.
--
-- Prints "Killed: N" and "Survived: M" so the Nix report derivation can parse them.
runMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runMutationMode settings manifestDirs spec = do
  MutationManifest records <- mconcat <$> mapM readManifestDir manifestDirs
  let recordMap = Map.fromList [(mutRecId r, r) | r <- records]
      mutations = map mutRecId records
  -- [check] The forest is built once and reused across mutations. Test bodies
  -- (IO actions) are re-executed each run, so ifMutation's NOINLINE protects
  -- them. However, any values computed via runIO during spec construction are
  -- memoized and will not reflect the active mutation. If that causes incorrect
  -- results, move execTestDefM inside runOne so the spec is re-evaluated per mutation.
  specForest <- execTestDefM settings spec
  (killed, survived) <- foldl (runOne recordMap specForest) (pure (0 :: Int, 0 :: Int)) mutations
  putStrLn $ "Killed: " ++ show killed
  putStrLn $ "Survived: " ++ show survived
  where
    mutationSettings = settings {settingThreads = Synchronous}

    runOne recordMap specForest accIO mid = do
      (killed, survived) <- accIO
      hPutStrLn stderr $ formatMutationLog mid (Map.lookup mid recordMap)
      setActiveMutation (Just mid)
      timedResult <- runSpecForestSynchronously mutationSettings specForest
      setActiveMutation Nothing
      if shouldExitFail mutationSettings (timedValue timedResult)
        then pure (killed + 1, survived)
        else pure (killed, survived + 1)

formatMutationLog :: MutationId -> Maybe MutationRecord -> String
formatMutationLog (MutationId parts) mRec =
  case (parts, mRec) of
    ( [modName, op, lineStr, colStartStr, colEndStr],
      Just MutationRecord {mutRecOriginal, mutRecReplacement, mutRecSourceFile, mutRecSourceLine, mutRecMutatedLine, mutRecContextBefore, mutRecContextAfter}
      ) ->
        let filePath = case mutRecSourceFile of
              Just p -> fromRelFile p
              Nothing -> moduleToFilePath modName
            header = "Testing mutation " ++ op ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr ++ ":"
         in case mutRecSourceLine of
              Nothing ->
                unlines
                  [ header,
                    "    - " ++ mutRecOriginal,
                    "    + " ++ mutRecReplacement
                  ]
              Just srcLine ->
                let lineNum = read lineStr :: Int
                    nBefore = length mutRecContextBefore
                    hunkHeader =
                      "@@ -"
                        ++ show (lineNum - nBefore)
                        ++ ","
                        ++ show (nBefore + 1 + length mutRecContextAfter)
                        ++ " +"
                        ++ show (lineNum - nBefore)
                        ++ ","
                        ++ show (nBefore + 1 + length mutRecContextAfter)
                        ++ " @@"
                    mutatedLine = fromMaybe srcLine mutRecMutatedLine
                 in T.unpack $
                      T.unlines $
                        map T.pack [header, hunkHeader]
                          ++ map (T.cons ' ') mutRecContextBefore
                          ++ [T.cons '-' srcLine, T.cons '+' mutatedLine]
                          ++ map (T.cons ' ') mutRecContextAfter
    _ ->
      "Testing mutation " ++ intercalate "/" parts
  where
    moduleToFilePath m = map (\c -> if c == '.' then '/' else c) m ++ ".hs"
