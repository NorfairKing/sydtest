module Test.Syd.Mutation.Runtime
  ( MutationId (..),
    activeMutation,
    setActiveMutation,
    parseMutationId,
    renderMutationId,
    ifMutation,
    coverageSlot,
    withCoverageSlot,
  )
where

import Control.Exception (finally)
import Data.GenValidity
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (arbitraryPrintableChar, listOf)

-- | Identifies a single mutation site. The format of the strings is chosen by
-- the plugin; the runtime treats this as an opaque key.
newtype MutationId = MutationId [String]
  deriving (Eq, Ord, Show)

instance Validity MutationId where
  validate = trivialValidation

instance GenValid MutationId where
  genValid = MutationId <$> listOf (listOf arbitraryPrintableChar)
  shrinkValid (MutationId parts) = MutationId <$> shrinkValid parts

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
-- @original@.  When no mutation is active but a coverage slot is installed,
-- records @mid@ as covered.
--
-- 'NOINLINE' prevents GHC from floating the 'readIORef' out of the call site
-- or caching a stale result across mutation runs.
{-# NOINLINE ifMutation #-}
ifMutation :: MutationId -> a -> a -> a
ifMutation mid mutated original =
  unsafePerformIO $ do
    active <- readIORef activeMutation
    case active of
      Just aid -> pure $ if aid == mid then mutated else original
      Nothing -> do
        mSlot <- readIORef coverageSlot
        case mSlot of
          Nothing -> pure ()
          Just ref -> modifyIORef' ref (Set.insert mid)
        pure original

-- | Process-global slot for per-test coverage collection.
--
-- When 'Just ref' is installed, every 'ifMutation' call (with no active
-- mutation) inserts its 'MutationId' into @ref@.  Install and remove via
-- 'withCoverageSlot'.
{-# NOINLINE coverageSlot #-}
coverageSlot :: IORef (Maybe (IORef (Set MutationId)))
coverageSlot = unsafePerformIO (newIORef Nothing)

-- | Run @action@ with @ref@ installed as the coverage accumulator.
-- The slot is cleared (set back to 'Nothing') when the action finishes,
-- even if it throws.
withCoverageSlot :: IORef (Set MutationId) -> IO a -> IO a
withCoverageSlot ref action = do
  writeIORef coverageSlot (Just ref)
  action `finally` writeIORef coverageSlot Nothing
