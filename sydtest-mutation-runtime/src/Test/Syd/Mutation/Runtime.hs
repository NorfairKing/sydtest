{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Autodocodec
import Control.Exception (finally)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.IORef
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (listOf1, suchThat)

-- | Identifies a single mutation site. The format of the strings is chosen by
-- the plugin; the runtime treats this as an opaque key.
--
-- Parts must be non-empty and contain no @\'/\'@: a 'MutationId' is rendered
-- as the slash-separated concatenation of its parts and parsed back by
-- splitting on @\'/\'@. The empty list and empty parts would render to a
-- string that 'parseMutationId' cannot round-trip; @\'/\'@ in a part would
-- be split on parse.
newtype MutationId = MutationId [String]
  deriving (Eq, Ord, Show, Generic)

instance Validity MutationId where
  validate (MutationId parts) =
    mconcat
      [ declare "the parts list is non-empty" (not (null parts)),
        decorateList parts $ \part ->
          mconcat
            [ declare "the part is non-empty" (not (null part)),
              declare "the part contains no '/'" ('/' `notElem` part)
            ]
      ]

instance GenValid MutationId where
  genValid =
    MutationId
      <$> listOf1 (T.unpack <$> genValidPart)
    where
      genValidPart =
        genValid `suchThat` \t ->
          not (T.null t) && not (T.any (== '/') t)
  shrinkValid (MutationId parts) =
    filter isValid $
      map (MutationId . map T.unpack) (shrinkValid (map T.pack parts))

instance HasCodec MutationId where
  codec = dimapCodec MutationId (\(MutationId parts) -> parts) codec

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
--
-- Rejects inputs that would produce an invalid 'MutationId' (e.g. the empty
-- string, or strings containing empty segments).
parseMutationId :: String -> Maybe MutationId
parseMutationId s =
  let mid_ = MutationId (splitOn '/' s)
   in if isValid mid_ then Just mid_ else Nothing
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

-- | Set the active mutation. Call this from the runner before each test run.
setActiveMutation :: Maybe MutationId -> IO ()
setActiveMutation = writeIORef activeMutation

-- | Emitted at every mutation site by the plugin.
--
-- When @mid@ is the active mutation, evaluates to @mutated@; otherwise to
-- @original@.  When no mutation is active but a coverage slot is installed,
-- records @mid@ as covered.
--
-- 'NOINLINE' prevents GHC from floating the 'readIORef' on 'coverageSlot' out
-- of the call site or CSE-ing it across calls.  Each 'ifMutation' call must
-- read 'coverageSlot' afresh so that 'withCoverageSlot' can install and
-- uninstall the slot between tests in the coverage phase.  The
-- 'activeMutation' read is constant within a process (mutations run in
-- separate processes, each set by MUTATION_ACTIVE at start), but the
-- 'coverageSlot' read is not.
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
