# Plan: Subprocess-per-mutation with augmented manifest

## Goal

Run each mutation in a child process. Document the reasons in a comment at
the top of `runMutationMode`. The three reasons are:

1. **Memory limit**: a mutation can cause unbounded allocation (e.g. turning a
   termination condition into a no-op). The RTS `-M` flag terminates the
   process when the heap cap is hit — this is not a catchable exception. In a
   subprocess, only the child dies; the parent records it as killed and moves on.

2. **Laziness / memoisation**: the spec forest is built once and reused across
   mutations. Values computed via `runIO` during spec construction are
   memoised in the parent's heap and will not reflect the active mutation.
   Subprocesses get a fresh heap each time, so there is no risk of a
   memoised value masking a mutation.

3. **Parallelism**: because `activeMutation` is a process-global `IORef`,
   mutations must run serially within a single process. Subprocesses are
   independent and can be run in parallel (disjoint mutation sets) in a future
   extension without any changes to the child or the manifest format.

A single `manifest-augmented.json` file per manifest directory carries both
the mutation records and the coverage data so the child only needs one file
path.

## Feedback loops

1. `stack build sydtest sydtest-mutation-runtime --pedantic`
2. `stack test sydtest-test sydtest-mutation --pedantic`
3. `nix flake check`

---

## Files to create

- `sydtest-mutation-runtime/src/Test/Syd/Mutation/AugmentedManifest.hs`

## Files to modify

- `sydtest-mutation-runtime/package.yaml` — add `AugmentedManifest` to exposed-modules
- `sydtest/src/Test/Syd/MutationMode.hs` — `runCoverageMode` writes augmented manifest; `runMutationMode` becomes subprocess parent loop
- `sydtest/src/Test/Syd/OptParse.hs` — add `--mutation-one <mid>` and `--mutation-augmented-manifest-dir <dir>` flags
- `sydtest/src/Test/Syd.hs` — wire new settings into `sydTest`

The Nix files (`nix/mutation-checks.nix`, `nix/compileMutationReport.nix`) do
not need changes — the two-phase shell invocations are unchanged; everything
new happens inside the Haskell binary.

---

## Step 1 — New module: `Test.Syd.Mutation.AugmentedManifest`

### Types

```haskell
data AugmentedMutationRecord = AugmentedMutationRecord
  { augmentedMutationRecordId            :: MutationId
  , augmentedMutationRecordOperator      :: String
  , augmentedMutationRecordOriginal      :: String
  , augmentedMutationRecordReplacement   :: String
  , augmentedMutationRecordSourceFile    :: Maybe (Path Rel File)
  , augmentedMutationRecordSourceLine    :: Maybe Text
  , augmentedMutationRecordMutatedLine   :: Maybe Text
  , augmentedMutationRecordContextBefore :: [Text]
  , augmentedMutationRecordContextAfter  :: [Text]
  , augmentedMutationRecordCoveringTests :: [TestId]   -- never Nothing; coverage always present
  }

newtype AugmentedManifest = AugmentedManifest [AugmentedMutationRecord]
  deriving (Show)

instance Semigroup AugmentedManifest
instance Monoid    AugmentedManifest
instance ToJSON    AugmentedManifest
instance FromJSON  AugmentedManifest
instance ToJSON    AugmentedMutationRecord
instance FromJSON  AugmentedMutationRecord
```

JSON field names are identical to `MutationRecord`; `covering_tests` is always
a JSON array (never null).

### Functions

```haskell
-- Write to <dir>/manifest-augmented.json (one file per dir, all modules combined).
-- Uses path-io's ensureDir.
writeAugmentedManifestFile :: Path Abs Dir -> AugmentedManifest -> IO ()

-- Read from <dir>/manifest-augmented.json.
readAugmentedManifestFile :: Path Abs Dir -> IO AugmentedManifest

-- O(n) lookup by MutationId.
lookupAugmentedMutationRecord :: MutationId -> AugmentedManifest -> Maybe AugmentedMutationRecord

-- Convert a fully-annotated MutationRecord (mutRecCoveringTests = Just ts) to
-- an AugmentedMutationRecord.  Records with mutRecCoveringTests = Nothing are
-- dropped (coverage was not collected for them).
fromMutationRecord :: MutationRecord -> Maybe AugmentedMutationRecord
```

---

## Step 2 — New CLI flags in `OptParse.hs`

```haskell
-- Already exists:
settingMutation         :: [Path Abs Dir]   -- --mutation <dir>
settingMutationCoverage :: [Path Abs Dir]   -- --mutation-coverage <dir>

-- New:
settingMutationAugmentedManifestDir :: Path Abs Dir
  -- --mutation-augmented-manifest-dir <dir>
  -- defaults to: current working directory

settingMutationOne :: Maybe MutationId
  -- --mutation-one <rendered-mid>
  -- used only by child processes; not shown in normal --help output
```

---

## Step 3 — `runCoverageMode` writes the augmented manifest

Current behaviour: writes `<module>.coverage.json` per module into the
manifest dir.

New behaviour (additional, not replacing): after collecting coverage, also
write `manifest-augmented.json` to `settingMutationAugmentedManifestDir` via
`writeAugmentedManifestFile`. This file contains all records across all
manifest dirs with `augmentedMutationRecordCoveringTests` populated.

The `*.coverage.json` files can be kept or dropped — they are no longer read
by `runMutationMode`. Simplest: stop writing them (remove `writeCoverageFile`
call) since nothing reads them once we have the augmented manifest.

---

## Step 4 — `runMutationMode` becomes the subprocess parent

```haskell
runMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runMutationMode settings manifestDirs _spec = do
  augmented <- readAugmentedManifestFile (settingMutationAugmentedManifestDir settings)
  let AugmentedManifest records = augmented
  exe <- getExecutablePath
  (killed, survived, uncovered) <- foldM (runOne exe) (0, 0, 0) records
  putStrLn $ "Killed: "    ++ show killed
  putStrLn $ "Survived: "  ++ show survived
  putStrLn $ "Uncovered: " ++ show uncovered
  where
    runOne exe acc record =
      case augmentedMutationRecordCoveringTests record of
        [] -> pure (acc & uncovered +1)
        _  -> do
          let mid  = augmentedMutationRecordId record
              args = [ "--mutation",     fromAbsDir (head manifestDirs)
                     , "--mutation-one", renderMutationId mid
                     , "--mutation-augmented-manifest-dir"
                     , fromAbsDir (settingMutationAugmentedManifestDir settings)
                     ]
          exitCode <- runProcess (proc exe args)   -- typed-process
          case exitCode of
            ExitSuccess   -> pure (acc & survived +1)
            ExitFailure _ -> pure (acc & killed   +1)
```

The parent does not build the `Spec` forest at all — `_spec` is unused. The
child does all the real work.

---

## Step 5 — Child entry point in `sydTest`

In `sydTest` (or wherever `runMutationMode`/`runCoverageMode` is dispatched),
add a new branch:

```
when (isJust settingMutationOne && not (null settingMutation)):
  runSingleMutationMode settings manifestDirs spec
```

```haskell
runSingleMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runSingleMutationMode settings manifestDirs spec = do
  let mid = fromJust (settingMutationOne settings)
  augmented <- readAugmentedManifestFile
                 (settingMutationAugmentedManifestDir settings)
  specForest <- execTestDefM settings spec
  let coveringTests = maybe [] augmentedMutationRecordCoveringTests
                        (lookupAugmentedMutationRecord mid augmented)
      forest = case coveringTests of
        [] -> specForest   -- uncovered: run full suite (shouldn't happen; parent skips these)
        ts -> filterTestForestByTrie (testIdTrieFromList ts) specForest
  bracket_ (setActiveMutation (Just mid)) (setActiveMutation Nothing) $ do
    timedResult <- runSpecForestSynchronously
                     (settings { settingThreads = Synchronous }) forest
    if shouldExitFail settings (timedValue timedResult)
      then exitWith (ExitFailure 1)
      else exitSuccess
```

---

## Step 6 — Tests to add

- **Roundtrip JSON test** for `AugmentedMutationRecord` in
  `sydtest-mutation-runtime` (or `sydtest-test`): `jsonSpec @AugmentedMutationRecord`
  using `genvalidity-sydtest-aeson`. Requires `GenValid` and `Validity`
  instances (similar to how `TestId` has them in `MutationSpec.hs`).
- **End-to-end**: existing `mutation-really-safe-money` and
  `mutation-safe-coloured-text` Nix checks serve as integration tests.

---

## What does NOT change

- `MutationRecord`, `MutationManifest`, `readManifestDir` — untouched.
- The GHC plugin — still writes `*.json` manifest files.
- The coverage collection logic inside `runCoverageMode` — unchanged.
- The Nix shell scripts — both phases still invoked the same way.
- `*.coverage.json` write can be removed as a cleanup (nothing reads it after
  this change) but is not required for correctness.
