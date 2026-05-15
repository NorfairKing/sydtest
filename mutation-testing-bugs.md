# Open bugs in the mutation-testing harness

Bugs and rough edges discovered while wiring `mutationCheck` up against a
real project (nix-ci). Capture-as-you-go; fix in the order most painful
for downstream users.

## 1. `<<loop>>` at the boundary between coverage and mutation phases

**Symptom.** With a real project (≈1500 tests, many spawning tmp-postgres),
the harness's outer test process crashes with

```
nix-ci-leader-gen-test: <<loop>>
HasCallStack backtrace: …
```

after the last `coverage (N/N): done …` line and before the first
`mutation-nix: running mutations` line. No mutant has been executed yet
at this point — only coverage discovery has run.

GHC's `<<loop>>` is `BlockedIndefinitelyOnMVar` or the related "thunk
under evaluation depends on itself" runtime exception. The likely
suspect is the augmented-manifest aggregation step (writing per-suite
coverage results into the `augmented/` directory). Possibly a
strictness/ordering issue when many test suites contribute coverage
records.

**Repro.** `nix build .#checks.x86_64-linux.mutation` in nix-ci after
removing `DisableMutations` from one production module. ~5 minute
runtime per attempt. About 20–30% of runs `<<loop>>`; the rest get to
the mutation phase.

**Wanted fix.** Pin down where the loop is (likely in the coverage
finalisation that runs between phases) and force the relevant data.

**Workaround for callers.** Retry the build. Document the retry as a
known flake (done — see nix-ci's `mutation-testing.md`).

## 2. `<<loop>>` inside a mutation child is not classified as "killed"

**Symptom.** Related to #1: when a *mutant* run causes `<<loop>>`, the
harness currently surfaces it as an error rather than as a killed
mutation. Conceptually a mutant that makes the test process loop is
indistinguishable from a mutant that makes it crash — both mean the
test detected the mutation. It should count as killed.

**Wanted fix.** Treat any `<<loop>>` (and any non-zero exit that isn't
specifically the timeout exit) from a mutation child as `killed`, not
as a harness error.

## 3. tmp-postgres `StartPostgresFailed (ExitFailure 1)` flakes under coverage parallelism

**Symptom.** With `coverageJobs = 8` and ~1500 tests, the leader-data
or cache test suite's tmp-postgres setup intermittently fails:

```
nix-ci-leader-data-test: ExpectationFailed "StartPostgresFailed (ExitFailure 1)"
…
nix-ci-leader-data-test: user error (coverage child for NixCI\.Leader\.CancelSuitesOnRefSpec.getRepositoryIdByGitHub.… exited with code 1)
```

The build then dies, having lost all the coverage work done so far.

This is largely a tmp-postgres / resource-contention problem, not a
sydtest bug per se, but the harness exits the entire coverage phase on
the first failure rather than retrying the child.

**Wanted fix (in harness).** A `--coverage-retry` option: if a coverage
child exits non-zero, retry it N times before giving up. Defaults to
3. Costs nothing on the green path; saves a 5-minute restart on the
flake path.

## 4. `testToolDepends` not propagated to harness PATH (FIXED in this branch)

**Symptom (pre-fix).** Test suites that depend on test-only tools via
`addTestToolDepends pkg [final.postgresql]` work fine in their own
`nix flake check`, but the mutation harness spawns those test
executables from a *different* package's checkPhase. The original
package's `testToolDepends` are gone from PATH at that point, so the
spawned test exe fails with e.g. `initdb: posix_spawnp: does not
exist`.

**Fix (committed locally).** `mutationCheck.nix` now collects
`getCabalDeps.testToolDepends` from every test package in `tests` and
adds them to the harness derivation's `nativeBuildInputs`.

Caveat: only works when the test package is built with `doCheck=true`
in the haskell-modules builder (the harness applies `doCheck` itself,
so this is satisfied for the packages it builds). Worth a note in
mutationCheck.nix in case a future caller passes a pre-doCheck package.

## 5. Mutations on values that only feed into observability are unkillable without prometheus assertions

**Symptom.** A `Bool` value whose only downstream use is a prometheus
counter increment or histogram observation cannot be killed by tests
that don't read prometheus. After moving the prometheus call into a
`DisableMutations`-annotated helper, the mutation moves to the
*argument expression at the call site* — still in instrumented code —
and remains a survivor.

Example from nix-ci's `PostJobRequest`:

```haskell
wasAbandoned <- case mAbandonedRunOutput of …  -- has DB side effects
pure $ Claimed $ mkClaimedJob wasAbandoned … ownerHadOther
                                ^^^^^^^^^^^                ^^^^^^^^^^^
                                ConstBool/Negate survives here
```

`mkClaimedJob` is annotated `DisableMutations`. Inside it the bool is
only stored in a record field, and the record's only consumer is also
annotated. So the bool's value is genuinely unobservable from outside
prometheus. The mutation operator has no way to know that and reports
a survivor.

**Why this matters.** This is a fundamental limitation of "exempt
observability code" by annotation. If we want zero survivors AND no
prometheus tests, the plugin needs to do some form of transitive
analysis: a value whose only sinks are inside `DisableMutations` scopes
should be exempt itself. That's a real piece of work.

**Possible directions.**

- A function-level `DisableMutations` annotation that exempts an
  argument-position binding: callers that pass certain args don't get
  those args mutated. Mechanically: tag the `mkClaimedJob` arguments
  themselves as observability-only, e.g.
  `{-# ANN mkClaimedJob ("DisableMutationArgs: 1, 6" :: String) #-}`.
- Or extend the annotation grammar to accept positional-arg or
  named-arg masks: `"DisableMutations: argument wasAbandoned"`.
- A coarser hammer: `"DisableMutationsTransitively"` — disable
  mutations on any expression whose value flows (per simple def-use
  analysis) only into a `DisableMutations` scope. Probably too much
  static analysis for the plugin.

For the moment downstream callers can either (a) accept these
survivors, (b) write prometheus-reading tests, or (c) restructure code
so the bool is constructed *inside* the disabled helper (not always
practical if the construction has real side effects).

## 6. Same surviving mutation is reported multiple times with identical signature

**Symptom.** The report often lists the same `LL:CC-CC Operator: original -> replacement`
twice or three times. Example from nix-ci:

```
L216:29-30 ListLit: 8 elements -> 7 elements
L216:29-30 ListLit: 8 elements -> 7 elements
L216:29-30 ListLit: 8 elements -> 0 elements
```

It's not actually three identical mutants — `ListLit` drops different
elements — but the report's textual line doesn't disambiguate which
element. Makes it hard to know which sub-mutant survived.

**Wanted fix.** Include the index / dropped-element / removed-alternative
identifier in the report's human-readable line. The JSON's `id` field
already disambiguates (the last component differs), so it's just
formatting.

## 7. No way to mark a specific mutation as intentionally accepted

**Symptom.** The harness has two granularities of opt-out:

- Per-module: `{-# ANN module ("DisableMutations" :: String) #-}`
- Per-binding: `{-# ANN funcName ("DisableMutations" :: String) #-}`

Both are too coarse. Real code regularly has individual mutations that
are genuinely unobservable from tests (and can't easily be made
observable):

- A `Bool` whose only sink is a `DisableMutations`-annotated helper
  (issue #5). Mutating it at the call site changes nothing the test
  layer can see.
- Intermediate-state writes that get immediately overwritten by the
  happy path (e.g. `DB.update runId [RunReady := True]` that is
  followed two lines later by `DB.update runId [RunReady := False]` on
  success).
- Defensive code: error messages whose exact text is not part of the
  contract (`unwords ["Job production failed, …"]`).

Today the only ways to silence these are to either tag the whole
function `DisableMutations` (losing all coverage of useful mutations
there) or to introduce a trivial helper just to hold the annotation.
That's gratuitous refactoring driven by the test framework.

**Wanted feature.** An *acceptance list* of specific mutations that
won't fail the build. Concretely:

1. Each surviving mutation has a stable ID (the `id` array in
   `report.json`: `[module, operator, line, col_start, col_end, replStr]`,
   plus the disambiguation tail when present).
2. The check should accept a path to a JSON or YAML file listing
   accepted mutation IDs (or just their stable hashes). Mutations in
   that list count as expected survivors and don't fail the build.
3. The file is checked into the repo. A diff in it makes the intent
   visible at review time.
4. Bonus: if an accepted mutation is now killed by tests (because tests
   improved), the harness flags it as a "stale acceptance" — encouraging
   the file to shrink, not grow.

**Sketch of `mutationCheck.nix` API:**

```nix
mutationCheck {
  name = "nix-ci";
  …
  acceptedMutations = ./mutation-accepted.json;
}
```

`acceptedMutations` defaults to no file (current behaviour). The JSON
schema would be e.g.

```json
{
  "version": 1,
  "accepted": [
    {
      "id": ["NixCI.Leader.APIServer.Handler.PostJobRequest", "ConstBool", "164", "51", "63", "False"],
      "reason": "Bool only feeds into the prometheus observation in observeClaimedJobMetrics; killing it requires prometheus-reading tests."
    }
  ]
}
```

Mandatory `reason` field so the file is grep-able and reviewable.

**Why we want this before continuing the nix-ci work.** Without it,
the only way to make the `mutation` check pass on a module that has any
"this can't be tested" mutation is to keep the whole module behind
`DisableMutations`, which defeats the point. We need a way to opt out
the truly unkillable handful and assert the kill-rate on everything
else.

## 8. `report.json` schema has `survivors` (array) but `uncovered` (count) + `uncovered_mutations` (array)

**Symptom.** Asymmetric schema:

- `killed`: count
- `survived`: count
- `survivors`: array of survivor records
- `uncovered`: count
- `uncovered_mutations`: array of uncovered records
- `timed_out`: count
- `timed_out_mutations`: array

Either `survived` + `survivors` should rename to `survived` + `survived_mutations`,
or `uncovered_mutations` should rename to `uncovered`. (The latter
shadows the count, so first rename is safer.) Tools that do
`jq '.uncovered[]'` naively fail with "Cannot iterate over number".

Trivially a schema-version bump.
