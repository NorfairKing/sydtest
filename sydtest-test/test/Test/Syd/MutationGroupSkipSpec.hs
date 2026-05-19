{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.MutationGroupSkipSpec (spec) where

import qualified Control.Exception as Exception
import Data.IORef
import qualified Data.Map.Strict as Map
import Path ()
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    SkippedMutation (..),
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
  )
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.MutationMode
  ( MutationResult (..),
    runOneGroup,
  )

spec :: Spec
spec = describe "runOneGroup" $ do
  it "runs every record when none fail" $ do
    let mkRec name =
          AugmentedMutationRecord
            { augmentedMutationRecordId = MutationId [name],
              augmentedMutationRecordOperator = "Op",
              augmentedMutationRecordOriginal = "orig",
              augmentedMutationRecordReplacement = "repl",
              augmentedMutationRecordModule = "M",
              augmentedMutationRecordLine = 1,
              augmentedMutationRecordEndLine = 1,
              augmentedMutationRecordColStart = 1,
              augmentedMutationRecordColEnd = 2,
              augmentedMutationRecordSourceFile = Nothing,
              augmentedMutationRecordSourceLines = [],
              augmentedMutationRecordMutatedLines = [],
              augmentedMutationRecordContextBefore = [],
              augmentedMutationRecordContextAfter = [],
              augmentedMutationRecordCoveringTests = Map.empty,
              augmentedMutationRecordTimeoutMicros = 30000000
            }
        a = mkRec "a"
        b = mkRec "b"
        c = mkRec "c"
        records = [a, b, c]
    resultsRef <- newIORef []
    ranRef <- newIORef []
    let runOne r = do
          modifyIORef' ranRef (r :)
          pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    runOneGroup False runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    results `shouldBe` [MutationKilled a, MutationKilled b, MutationKilled c]
    ran `shouldBe` [a, b, c]

  it "skips remaining records after a survivor (global fail-fast off)" $ do
    let mkRec name =
          AugmentedMutationRecord
            { augmentedMutationRecordId = MutationId [name],
              augmentedMutationRecordOperator = "Op",
              augmentedMutationRecordOriginal = "orig",
              augmentedMutationRecordReplacement = "repl",
              augmentedMutationRecordModule = "M",
              augmentedMutationRecordLine = 1,
              augmentedMutationRecordEndLine = 1,
              augmentedMutationRecordColStart = 1,
              augmentedMutationRecordColEnd = 2,
              augmentedMutationRecordSourceFile = Nothing,
              augmentedMutationRecordSourceLines = [],
              augmentedMutationRecordMutatedLines = [],
              augmentedMutationRecordContextBefore = [],
              augmentedMutationRecordContextAfter = [],
              augmentedMutationRecordCoveringTests = Map.empty,
              augmentedMutationRecordTimeoutMicros = 30000000
            }
        a = mkRec "a"
        b = mkRec "b"
        c = mkRec "c"
        records = [a, b, c]
    resultsRef <- newIORef []
    ranRef <- newIORef []
    let runOne r = do
          modifyIORef' ranRef (r :)
          case augmentedMutationRecordId r of
            MutationId ["b"] ->
              pure $
                MutationSurvived
                  SurvivedMutation
                    { survivedMutationRecord = r,
                      survivedMutationLogFile = Nothing
                    }
            _ -> pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    runOneGroup False runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    results
      `shouldBe` [ MutationKilled a,
                   MutationSurvived
                     SurvivedMutation
                       { survivedMutationRecord = b,
                         survivedMutationLogFile = Nothing
                       },
                   MutationSkipped
                     SkippedMutation
                       { skippedMutationRecord = c,
                         skippedMutationCause = augmentedMutationRecordId b
                       }
                 ]
    ran `shouldBe` [a, b]

  it "skips remaining records after an uncovered mutation" $ do
    let mkRec name =
          AugmentedMutationRecord
            { augmentedMutationRecordId = MutationId [name],
              augmentedMutationRecordOperator = "Op",
              augmentedMutationRecordOriginal = "orig",
              augmentedMutationRecordReplacement = "repl",
              augmentedMutationRecordModule = "M",
              augmentedMutationRecordLine = 1,
              augmentedMutationRecordEndLine = 1,
              augmentedMutationRecordColStart = 1,
              augmentedMutationRecordColEnd = 2,
              augmentedMutationRecordSourceFile = Nothing,
              augmentedMutationRecordSourceLines = [],
              augmentedMutationRecordMutatedLines = [],
              augmentedMutationRecordContextBefore = [],
              augmentedMutationRecordContextAfter = [],
              augmentedMutationRecordCoveringTests = Map.empty,
              augmentedMutationRecordTimeoutMicros = 30000000
            }
        a = mkRec "a"
        b = mkRec "b"
        records = [a, b]
    resultsRef <- newIORef []
    ranRef <- newIORef []
    let runOne r = do
          modifyIORef' ranRef (r :)
          case augmentedMutationRecordId r of
            MutationId ["a"] -> pure (MutationUncovered (UncoveredMutation r))
            _ -> pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    runOneGroup False runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    results
      `shouldBe` [ MutationUncovered (UncoveredMutation a),
                   MutationSkipped
                     SkippedMutation
                       { skippedMutationRecord = b,
                         skippedMutationCause = augmentedMutationRecordId a
                       }
                 ]
    ran `shouldBe` [a]

  it "does not skip after a timed-out mutation" $ do
    let mkRec name =
          AugmentedMutationRecord
            { augmentedMutationRecordId = MutationId [name],
              augmentedMutationRecordOperator = "Op",
              augmentedMutationRecordOriginal = "orig",
              augmentedMutationRecordReplacement = "repl",
              augmentedMutationRecordModule = "M",
              augmentedMutationRecordLine = 1,
              augmentedMutationRecordEndLine = 1,
              augmentedMutationRecordColStart = 1,
              augmentedMutationRecordColEnd = 2,
              augmentedMutationRecordSourceFile = Nothing,
              augmentedMutationRecordSourceLines = [],
              augmentedMutationRecordMutatedLines = [],
              augmentedMutationRecordContextBefore = [],
              augmentedMutationRecordContextAfter = [],
              augmentedMutationRecordCoveringTests = Map.empty,
              augmentedMutationRecordTimeoutMicros = 30000000
            }
        a = mkRec "a"
        b = mkRec "b"
        c = mkRec "c"
        records = [a, b, c]
    resultsRef <- newIORef []
    ranRef <- newIORef []
    let runOne r = do
          modifyIORef' ranRef (r :)
          case augmentedMutationRecordId r of
            MutationId ["a"] ->
              pure $
                MutationTimedOut
                  TimedOutMutation
                    { timedOutMutationRecord = r,
                      timedOutMutationElapsedMicros = 1,
                      timedOutMutationLogFile = Nothing
                    }
            _ -> pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    runOneGroup False runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    results
      `shouldBe` [ MutationTimedOut
                     TimedOutMutation
                       { timedOutMutationRecord = a,
                         timedOutMutationElapsedMicros = 1,
                         timedOutMutationLogFile = Nothing
                       },
                   MutationKilled b,
                   MutationKilled c
                 ]
    ran `shouldBe` [a, b, c]

  it "with global fail-fast on, throws after recording the survivor and remaining records as skipped" $ do
    let mkRec name =
          AugmentedMutationRecord
            { augmentedMutationRecordId = MutationId [name],
              augmentedMutationRecordOperator = "Op",
              augmentedMutationRecordOriginal = "orig",
              augmentedMutationRecordReplacement = "repl",
              augmentedMutationRecordModule = "M",
              augmentedMutationRecordLine = 1,
              augmentedMutationRecordEndLine = 1,
              augmentedMutationRecordColStart = 1,
              augmentedMutationRecordColEnd = 2,
              augmentedMutationRecordSourceFile = Nothing,
              augmentedMutationRecordSourceLines = [],
              augmentedMutationRecordMutatedLines = [],
              augmentedMutationRecordContextBefore = [],
              augmentedMutationRecordContextAfter = [],
              augmentedMutationRecordCoveringTests = Map.empty,
              augmentedMutationRecordTimeoutMicros = 30000000
            }
        a = mkRec "a"
        b = mkRec "b"
        c = mkRec "c"
        records = [a, b, c]
    resultsRef <- newIORef []
    ranRef <- newIORef []
    let runOne r = do
          modifyIORef' ranRef (r :)
          case augmentedMutationRecordId r of
            MutationId ["b"] ->
              pure $
                MutationSurvived
                  SurvivedMutation
                    { survivedMutationRecord = r,
                      survivedMutationLogFile = Nothing
                    }
            _ -> pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    -- runOneGroup throws MutationFailFast after every remaining record in
    -- the group has been recorded as 'MutationSkipped'.  Catch that
    -- exception so the test can inspect the partial state.  The third
    -- record is not run, but it is recorded as skipped so the partial
    -- report reflects the entire group.
    _ <-
      Exception.try @Exception.SomeException $
        runOneGroup True runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    results
      `shouldBe` [ MutationKilled a,
                   MutationSurvived
                     SurvivedMutation
                       { survivedMutationRecord = b,
                         survivedMutationLogFile = Nothing
                       },
                   MutationSkipped
                     SkippedMutation
                       { skippedMutationRecord = c,
                         skippedMutationCause = augmentedMutationRecordId b
                       }
                 ]
    ran `shouldBe` [a, b]
