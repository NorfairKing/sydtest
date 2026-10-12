# Bug progress: sydtest issue 110

## Bug report

https://github.com/NorfairKing/sydtest/issues/110 "Asynchronous sydTest hangs"

Original report (Mansindam) was a `-xc` backtrace showing a thunk exception
inside `runSpecForestInterleavedWithOutputAsynchronously`, with the note that
everything works with `settingThreads = Synchronous`.

The actionable minimal repro came from bristermitten (2026-10-11):

```hs
data Boom = Boom deriving Show
instance Exception Boom where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException

main = sydTest $ it "boom" (throwIO Boom :: IO ())
```

Asynchronous mode:
```
thread blocked indefinitely in an MVar operation
HasCallStack backtrace:
  throwIO, called at ./Control/Concurrent/Async/Internal.hs:681:24 in async-...
```

Synchronous mode correctly reports the uncaught exception.

This comes up in practice with Effectful's `Error` effect, which uses the same
`asyncExceptionToException` design.

## Workflow steps

### Step 0: Prepare git
Branch `async-exception-hang` off `development`, pushed before starting work.
Small frequent commits are fine.

### Step 1: Understand the bug
Research the codebase to understand the area where the bug occurs.
Read relevant code, trace the control flow, form a hypothesis about root cause.

### Step 2: Write a regression test
Before making any fix, write a test that reproduces the bug.

### Step 3: Falsify
Run the test and confirm that it fails. If it does not fail, go back to step 1.
Once it fails, commit it in a separate commit before making any fix.

### Step 4: Ask questions
Ask all questions at once.

### Step 5: Fix the bug
Implement the simplest fix that addresses the root cause.

### Step 6: Confirm the fix
Run the regression test again and confirm that it passes.

### Step 7: Get CI to pass
Use the /ci skill.

### Step 8: Commit, Push, and PR
Remove bug-progress.md, final commit, push, PR onto development with summary
of bug / test / root cause / fix / things to check manually.

## Notes

After any context compaction, re-read both `bug-progress.md` AND the `/fix`
skill file to reload the full workflow instructions.

### Research findings

- `sydtest/src/Test/Syd/Run.hs:475` `exceptionHandlers` deliberately re-throws
  `SomeAsyncException` (issue #80), so an async exception from a test escapes
  the test function entirely instead of becoming a test result.
- `sydtest/src/Test/Syd/Runner/Asynchronous.hs`:
  - Each test's result is delivered via `putMVar var result` inside `job`.
  - `job`s run on worker threads spawned by `withJobQueueWorkers`:
    `withAsync (mapConcurrently (jobQueueWorker jobQueue) [...]) (\_ -> func)`.
    `withAsync` does NOT link, so an exception that kills a worker is swallowed.
  - Hypothesis: the async exception kills the worker thread before
    `putMVar var result`, the workers' async dies silently, `blockUntilDone`
    returns (the `bracket` released the working count), the runner finishes,
    and the `printer`/`waiter` thread is left blocked forever on
    `takeMVar var` -> `BlockedIndefinitelyOnMVar` (which is exactly what the
    reporter saw), or a true hang.
