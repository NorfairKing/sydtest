# Feature: Per-test coverage manifest for mutation testing

## Request
Improve mutation testing speed by using per-test coverage information.
For each mutation, only run the tests that actually cover it.

## Approved plan

### Data model
- `TestId` moves from `sydtest-mutation` to `sydtest-mutation-runtime`
- `MutationRecord` gains `coveringTests :: Maybe [TestId]`
- Coverage phase writes `<manifestDir>/<Module>.coverage.json`
- Mutation phase joins plugin files + coverage files on `MutationId`

### Coverage mechanism
- `coverageSlot :: IORef (Maybe (IORef (Set MutationId)))` in runtime
- `ifMutation` records into slot when `activeMutation = Nothing`
- `withCoverageSlot` helper wraps each leaf test during coverage phase

### Workflow
1. Build (plugin writes `<Module>.json`, `coveringTests = Nothing`)
2. `--mutation-coverage <dir>` → writes `<Module>.coverage.json`
3. `--mutation <dir>` → reads both, filters per mutation

### Files to change
- `sydtest-mutation-runtime/Runtime.hs`: move TestId here, add coverageSlot
- `sydtest-mutation-runtime/Manifest.hs`: coveringTests field, coverage file I/O
- `sydtest-mutation/Mutation.hs`: re-export TestId, add collectCoverage
- `sydtest/OptParse.hs`: --mutation-coverage flag
- `sydtest/MutationMode.hs`: runCoverageMode, update runMutationMode
- Nix: chain coverage phase

## Feedback loops
1. `stack build sydtest-mutation-runtime --pedantic`
2. `stack build sydtest-mutation --pedantic`
3. `stack build sydtest --pedantic`
4. `stack test sydtest-mutation-example --pedantic`
5. `nix flake check`

## Progress
- [x] Step 0: Prepare git
- [ ] Step 4: Implement
- [ ] Step 5: Self-review
- [ ] Step 6: CI
- [ ] Step 9: Push

## After any context compaction
Re-read both `feature-progress.md` AND the `/feature` skill file to reload
the full workflow instructions.
