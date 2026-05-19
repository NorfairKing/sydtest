{-# LANGUAGE LambdaCase #-}
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
import Test.Syd.Mutation.Runtime (MutationId (..), renderMutationId)
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
        records = [mkRec "a", mkRec "b", mkRec "c"]
    resultsRef <- newIORef []
    ranRef <- newIORef []
    let runOne r = do
          modifyIORef' ranRef (r :)
          pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    runOneGroup False runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    length results `shouldBe` 3
    map (\case MutationKilled _ -> True; _ -> False) results
      `shouldBe` [True, True, True]
    map augmentedMutationRecordId ran
      `shouldBe` [MutationId ["a"], MutationId ["b"], MutationId ["c"]]

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
        records = [mkRec "a", mkRec "b", mkRec "c"]
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
    length results `shouldBe` 3
    map (\case MutationKilled _ -> True; _ -> False) results
      `shouldBe` [True, False, False]
    map (\case MutationSurvived _ -> True; _ -> False) results
      `shouldBe` [False, True, False]
    map (\case MutationSkipped _ -> True; _ -> False) results
      `shouldBe` [False, False, True]
    map augmentedMutationRecordId ran
      `shouldBe` [MutationId ["a"], MutationId ["b"]]
    case results of
      [_, _, MutationSkipped sm] ->
        renderMutationId (skippedMutationCause sm) `shouldBe` "b"
      _ -> expectationFailure $ "expected 3 results, got " <> show (length results)

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
        records = [mkRec "a", mkRec "b"]
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
    map (\case MutationUncovered _ -> True; _ -> False) results
      `shouldBe` [True, False]
    map (\case MutationSkipped _ -> True; _ -> False) results
      `shouldBe` [False, True]
    map augmentedMutationRecordId ran `shouldBe` [MutationId ["a"]]

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
        records = [mkRec "a", mkRec "b", mkRec "c"]
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
    map (\case MutationTimedOut _ -> True; _ -> False) results
      `shouldBe` [True, False, False]
    map (\case MutationSkipped _ -> True; _ -> False) results
      `shouldBe` [False, False, False]
    -- All three records were actually run; nothing was skipped.
    length ran `shouldBe` 3

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
        records = [mkRec "a", mkRec "b", mkRec "c"]
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
    -- Recall runOneGroup throws MutationFailFast after the failing result is
    -- appended; catch that exception so the test can inspect the partial
    -- state.  All three results should still be recorded (1 killed, 1
    -- survived) before the throw, and the third record should not have been
    -- spawned.
    _ <-
      Exception.try @Exception.SomeException $
        runOneGroup True runOne onResult records
    results <- reverse <$> readIORef resultsRef
    ran <- reverse <$> readIORef ranRef
    -- Only the first two results made it (the survivor triggers the throw).
    length results `shouldBe` 2
    map (\case MutationKilled _ -> True; _ -> False) results
      `shouldBe` [True, False]
    map (\case MutationSurvived _ -> True; _ -> False) results
      `shouldBe` [False, True]
    map augmentedMutationRecordId ran
      `shouldBe` [MutationId ["a"], MutationId ["b"]]

  it "preserves source order of results in a fully-killed group" $ do
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
        records = [mkRec "a", mkRec "b", mkRec "c"]
    resultsRef <- newIORef []
    let runOne r = pure (MutationKilled r)
        onResult r = modifyIORef' resultsRef (r :)
    runOneGroup False runOne onResult records
    results <- reverse <$> readIORef resultsRef
    let recordOf = \case
          MutationKilled r -> r
          MutationSurvived sm -> survivedMutationRecord sm
          MutationTimedOut tm -> timedOutMutationRecord tm
          MutationUncovered um -> uncoveredMutationRecord um
          MutationSkipped sk -> skippedMutationRecord sk
    map (renderMutationId . augmentedMutationRecordId . recordOf) results
      `shouldBe` ["a", "b", "c"]
