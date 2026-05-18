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

-- | Build a synthetic record whose only distinguishing feature for these
-- tests is its 'MutationId'.
mkRec :: String -> AugmentedMutationRecord
mkRec name =
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

-- | Build a 'MutationResult' that is one of: killed, survived, timed-out,
-- uncovered.  Used to script the injected runner.
data ScriptedOutcome
  = SKilled
  | SSurvived
  | STimedOut
  | SUncovered
  deriving (Eq, Show)

asResult :: AugmentedMutationRecord -> ScriptedOutcome -> MutationResult
asResult r = \case
  SKilled -> MutationKilled r
  SSurvived ->
    MutationSurvived
      SurvivedMutation
        { survivedMutationRecord = r,
          survivedMutationLogFile = Nothing
        }
  STimedOut ->
    MutationTimedOut
      TimedOutMutation
        { timedOutMutationRecord = r,
          timedOutMutationElapsedMicros = 1,
          timedOutMutationLogFile = Nothing
        }
  SUncovered -> MutationUncovered (UncoveredMutation r)

-- | Run a group with a scripted per-record outcome.  Returns the produced
-- 'MutationResult's in source order and the list of records actually passed
-- to the runner (so we can assert which records were *not* spawned because
-- they were skipped).
runScripted ::
  Bool ->
  [(AugmentedMutationRecord, ScriptedOutcome)] ->
  IO ([MutationResult], [AugmentedMutationRecord])
runScripted globalFailFast script = do
  let scriptMap =
        Map.fromList
          [(augmentedMutationRecordId r, o) | (r, o) <- script]
      records = map fst script
  resultsRef <- newIORef []
  ranRef <- newIORef []
  let runOne r = do
        modifyIORef' ranRef (r :)
        case Map.lookup (augmentedMutationRecordId r) scriptMap of
          Just o -> pure (asResult r o)
          Nothing -> fail "runScripted: record not in script"
      onResult r = modifyIORef' resultsRef (r :)
  _ <-
    Exception.try @Exception.SomeException $
      runOneGroup globalFailFast runOne onResult records
  results <- reverse <$> readIORef resultsRef
  ran <- reverse <$> readIORef ranRef
  pure (results, ran)

isSkipped :: MutationResult -> Bool
isSkipped = \case
  MutationSkipped _ -> True
  _ -> False

isSurvived :: MutationResult -> Bool
isSurvived = \case
  MutationSurvived _ -> True
  _ -> False

isKilled :: MutationResult -> Bool
isKilled = \case
  MutationKilled _ -> True
  _ -> False

isUncovered :: MutationResult -> Bool
isUncovered = \case
  MutationUncovered _ -> True
  _ -> False

isTimedOut :: MutationResult -> Bool
isTimedOut = \case
  MutationTimedOut _ -> True
  _ -> False

skippedCauseId :: MutationResult -> Maybe String
skippedCauseId = \case
  MutationSkipped sm -> Just (renderMutationId (skippedMutationCause sm))
  _ -> Nothing

spec :: Spec
spec = describe "runOneGroup" $ do
  let a = mkRec "a"
      b = mkRec "b"
      c = mkRec "c"

  it "runs every record when none fail" $ do
    (results, ran) <- runScripted False [(a, SKilled), (b, SKilled), (c, SKilled)]
    length results `shouldBe` 3
    map isKilled results `shouldBe` [True, True, True]
    map augmentedMutationRecordId ran `shouldBe` [MutationId ["a"], MutationId ["b"], MutationId ["c"]]

  it "skips remaining records after a survivor (global fail-fast off)" $ do
    (results, ran) <- runScripted False [(a, SKilled), (b, SSurvived), (c, SKilled)]
    length results `shouldBe` 3
    map isKilled results `shouldBe` [True, False, False]
    map isSurvived results `shouldBe` [False, True, False]
    map isSkipped results `shouldBe` [False, False, True]
    map augmentedMutationRecordId ran `shouldBe` [MutationId ["a"], MutationId ["b"]]
    case results of
      [_, _, r] -> skippedCauseId r `shouldBe` Just "b"
      _ -> expectationFailure $ "expected 3 results, got " <> show (length results)

  it "skips remaining records after an uncovered mutation" $ do
    (results, ran) <- runScripted False [(a, SUncovered), (b, SKilled)]
    map isUncovered results `shouldBe` [True, False]
    map isSkipped results `shouldBe` [False, True]
    map augmentedMutationRecordId ran `shouldBe` [MutationId ["a"]]

  it "does not skip after a timed-out mutation" $ do
    (results, ran) <- runScripted False [(a, STimedOut), (b, SKilled), (c, SKilled)]
    map isTimedOut results `shouldBe` [True, False, False]
    map isSkipped results `shouldBe` [False, False, False]
    -- All three records were actually run; nothing was skipped.
    length ran `shouldBe` 3

  it "with global fail-fast on, throws after recording the survivor and remaining records as skipped" $ do
    -- Recall runOneGroup throws MutationFailFast after the failing result is
    -- appended; runScripted catches that exception.  All three results
    -- should still be recorded (1 killed, 1 survived) before the throw,
    -- and the third record should not have been spawned.
    (results, ran) <- runScripted True [(a, SKilled), (b, SSurvived), (c, SKilled)]
    -- Only the first two results made it (the survivor triggers the throw).
    length results `shouldBe` 2
    map isKilled results `shouldBe` [True, False]
    map isSurvived results `shouldBe` [False, True]
    map augmentedMutationRecordId ran `shouldBe` [MutationId ["a"], MutationId ["b"]]

  it "preserves source order of results in a fully-killed group" $ do
    (results, _) <- runScripted False [(a, SKilled), (b, SKilled), (c, SKilled)]
    map (renderMutationId . augmentedMutationRecordId) (recordsOf results)
      `shouldBe` ["a", "b", "c"]
  where
    recordsOf :: [MutationResult] -> [AugmentedMutationRecord]
    recordsOf =
      map
        ( \case
            MutationKilled r -> r
            MutationSurvived sm -> survivedMutationRecord sm
            MutationTimedOut tm -> timedOutMutationRecord tm
            MutationUncovered um -> uncoveredMutationRecord um
            MutationSkipped sk -> skippedMutationRecord sk
        )
