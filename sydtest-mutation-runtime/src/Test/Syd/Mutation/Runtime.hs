module Test.Syd.Mutation.Runtime
  ( MutationId (..),
    activeMutation,
    setActiveMutation,
    parseMutationId,
    renderMutationId,
    ifMutation,
    runMutationMode,
  )
where

import Data.IORef
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, waitForProcess)
import qualified System.Process as Process

-- | Identifies a single mutation site. The format of the strings is chosen by
-- the plugin; the runtime treats this as an opaque key.
newtype MutationId = MutationId [String]
  deriving (Eq, Ord, Show)

-- | Process-global IORef holding the currently active mutation, if any.
--
-- Initialised from the MUTATION_ACTIVE environment variable at process start;
-- the runner may also call 'setActiveMutation' directly.
{-# NOINLINE activeMutation #-}
activeMutation :: IORef (Maybe MutationId)
activeMutation = unsafePerformIO $ do
  env <- lookupEnv "MUTATION_ACTIVE"
  newIORef (parseMutationId =<< env)

-- | Parse a mutation id from the MUTATION_ACTIVE env var format (slash-separated).
parseMutationId :: String -> Maybe MutationId
parseMutationId s
  | null s = Nothing
  | otherwise = Just (MutationId (splitOn '/' s))
  where
    splitOn _ [] = [""]
    splitOn sep (c : cs)
      | c == sep = "" : splitOn sep cs
      | otherwise = case splitOn sep cs of
          [] -> [[c]]
          (w : ws) -> (c : w) : ws

-- | Render a 'MutationId' as the slash-separated string used in MUTATION_ACTIVE.
renderMutationId :: MutationId -> String
renderMutationId (MutationId parts) = intercalate "/" parts
  where
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs

-- | Set the active mutation. Call this from the runner before each test run.
setActiveMutation :: Maybe MutationId -> IO ()
setActiveMutation = writeIORef activeMutation

-- | Run the test executable once per mutation listed in the manifest file.
--
-- For each mutation, spawns the current executable with MUTATION_ACTIVE set to
-- the mutation id and --synchronous to avoid interleaved output. Exit code 0
-- means the mutation survived (all tests passed); non-zero means it was killed.
--
-- Prints "Killed: N" and "Survived: M" to stdout so the Nix assertMutationScore
-- derivation can parse the report.
runMutationMode :: FilePath -> IO ()
runMutationMode manifestPath = do
  contents <- readFile manifestPath
  let manifestLines = filter (not . null) (lines contents)
  self <- getExecutablePath
  (killed, survived) <- foldl (runOne self) (pure (0 :: Int, 0 :: Int)) manifestLines
  putStrLn $ "Killed: " ++ show killed
  putStrLn $ "Survived: " ++ show survived
  where
    tabToSlash c = if c == '\t' then '/' else c

    runOne self accIO tabLine = do
      (killed, survived) <- accIO
      let activeVal = map tabToSlash tabLine
      hPutStrLn stderr $ "mutation: testing " ++ activeVal
      (_, _, _, ph) <-
        createProcess
          (proc self ["--synchronous"])
            { Process.env = Just [("MUTATION_ACTIVE", activeVal)]
            }
      code <- waitForProcess ph
      case code of
        ExitSuccess -> pure (killed, survived + 1)
        ExitFailure _ -> pure (killed + 1, survived)

-- | Emitted at every mutation site by the plugin.
--
-- When @mid@ is the active mutation, evaluates to @mutated@; otherwise to
-- @original@.
--
-- 'NOINLINE' prevents GHC from floating the 'readIORef' out of the call site
-- or caching a stale result across mutation runs.
{-# NOINLINE ifMutation #-}
ifMutation :: MutationId -> a -> a -> a
ifMutation mid mutated original =
  unsafePerformIO $ do
    active <- readIORef activeMutation
    pure $ if active == Just mid then mutated else original
