{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression test for bug #2: when a mutation child loops or otherwise
-- causes a synchronous exception in the parent's per-suite runner, the
-- harness must classify it as 'SuiteKilled' rather than letting the
-- exception escape and crash the whole 'runMutationMode'.
--
-- A mutation that makes the child unrunnable is conceptually
-- indistinguishable from a mutation that makes the child crash — both
-- mean the test detected the mutation.  The exception classifier is
-- factored out as 'classifySyncExceptionAsKilled' so it can be tested
-- directly without spawning a real subprocess.
module Test.Syd.ClassifySyncExceptionAsKilledSpec (spec) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception
  ( ArithException (DivideByZero),
    AsyncException (UserInterrupt),
    BlockedIndefinitelyOnMVar (..),
    ErrorCall (..),
    SomeException,
    throwIO,
    try,
  )
import Test.Syd
import Test.Syd.MutationMode (SuiteOutcome (..), classifySyncExceptionAsKilled)

spec :: Spec
spec = describe "classifySyncExceptionAsKilled (bug #2)" $ do
  it "passes through SuiteKilled unchanged" $ do
    outcome <- classifySyncExceptionAsKilled (pure SuiteKilled)
    outcome `shouldBe` SuiteKilled

  it "passes through SuiteSurvived unchanged" $ do
    outcome <- classifySyncExceptionAsKilled (pure (SuiteSurvived Nothing))
    outcome `shouldBe` SuiteSurvived Nothing

  it "passes through SuiteTimedOut unchanged" $ do
    outcome <- classifySyncExceptionAsKilled (pure (SuiteTimedOut 1234 Nothing))
    outcome `shouldBe` SuiteTimedOut 1234 Nothing

  it "treats BlockedIndefinitelyOnMVar (the <<loop>> case) as SuiteKilled" $ do
    outcome <- classifySyncExceptionAsKilled (throwIO BlockedIndefinitelyOnMVar)
    outcome `shouldBe` SuiteKilled

  it "treats ErrorCall as SuiteKilled" $ do
    outcome <- classifySyncExceptionAsKilled (throwIO (ErrorCall "boom"))
    outcome `shouldBe` SuiteKilled

  it "treats ArithException as SuiteKilled" $ do
    outcome <- classifySyncExceptionAsKilled (throwIO DivideByZero)
    outcome `shouldBe` SuiteKilled

  it "re-throws SomeAsyncException (so Ctrl-C still cancels the runner)" $ do
    -- 'UserInterrupt' is an 'AsyncException', which is a child of
    -- 'SomeAsyncException'.  The classifier must re-throw it so the user's
    -- Ctrl-C still terminates the parent process.
    result <-
      try $
        classifySyncExceptionAsKilled $ do
          tid <- myThreadId
          throwTo tid UserInterrupt
          pure SuiteKilled
    case result :: Either SomeException SuiteOutcome of
      Right _ -> expectationFailure "expected the async exception to be re-thrown"
      Left _ -> pure ()
