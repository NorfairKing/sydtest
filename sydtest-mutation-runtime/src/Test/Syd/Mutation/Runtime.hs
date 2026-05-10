module Test.Syd.Mutation.Runtime
  ( MutationId (..),
    activeMutation,
    setActiveMutation,
    parseMutationId,
    renderMutationId,
    ifMutation,
  )
where

import Data.IORef
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

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
