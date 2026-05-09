-- | Regression test for bug #3: a flaky coverage child should be retried
-- N times before the harness gives up, so a transient failure doesn't
-- discard the rest of the coverage phase.
--
-- The retry logic is factored out as 'retryingIO' so it can be tested
-- without spawning real subprocesses.
module Test.Syd.RetryingIOSpec (spec) where

import Data.IORef
import Test.Syd
import Test.Syd.MutationMode (retryingIO)

spec :: Spec
spec = describe "retryingIO (bug #3)" $ do
  it "returns immediately when the first attempt succeeds" $ do
    actionScript <- newIORef [Right (42 :: Int)]
    retryLog <- newIORef ([] :: [(String, Word)])
    let scripted = do
          remaining <- readIORef actionScript
          case remaining of
            [] -> error "ran out of pre-programmed outcomes"
            (x : xs) -> writeIORef actionScript xs >> pure x
    result <-
      retryingIO
        3
        (\reason left -> modifyIORef retryLog ((reason, left) :))
        scripted
    result `shouldBe` Right 42
    readIORef retryLog `shouldReturn` []

  it "retries once when the first attempt fails and the second succeeds" $ do
    actionScript <- newIORef [Left "boom", Right (7 :: Int)]
    retryLog <- newIORef ([] :: [(String, Word)])
    let scripted = do
          remaining <- readIORef actionScript
          case remaining of
            [] -> error "ran out of pre-programmed outcomes"
            (x : xs) -> writeIORef actionScript xs >> pure x
    result <-
      retryingIO
        3
        (\reason left -> modifyIORef retryLog ((reason, left) :))
        scripted
    result `shouldBe` Right 7
    -- One retry happened, with 2 retries remaining afterwards.
    readIORef retryLog `shouldReturn` [("boom", 2)]

  it "uses up all retries when every attempt fails" $ do
    let attempts = replicate 4 (Left "boom" :: Either String Int)
    actionScript <- newIORef attempts
    retryLog <- newIORef ([] :: [(String, Word)])
    let scripted = do
          remaining <- readIORef actionScript
          case remaining of
            [] -> error "ran out of pre-programmed outcomes"
            (x : xs) -> writeIORef actionScript xs >> pure x
    result <-
      retryingIO
        3
        (\reason left -> modifyIORef retryLog ((reason, left) :))
        scripted
    result `shouldBe` Left "boom"
    -- 3 retries -> 3 onRetry callbacks (oldest at the back of the list).
    readIORef retryLog `shouldReturn` [("boom", 0), ("boom", 1), ("boom", 2)]
    -- All 4 attempts consumed; nothing left in the script.
    readIORef actionScript `shouldReturn` []

  it "retriesLeft = 0 means one attempt and no retries" $ do
    actionScript <- newIORef [Left ("only one chance" :: String) :: Either String Int]
    retryLog <- newIORef ([] :: [(String, Word)])
    let scripted = do
          remaining <- readIORef actionScript
          case remaining of
            [] -> error "ran out of pre-programmed outcomes"
            (x : xs) -> writeIORef actionScript xs >> pure x
    result <-
      retryingIO
        0
        (\reason left -> modifyIORef retryLog ((reason, left) :))
        scripted
    result `shouldBe` Left "only one chance"
    readIORef retryLog `shouldReturn` []

  it "stops retrying as soon as an attempt succeeds (does not exhaust retries)" $ do
    actionScript <- newIORef [Left "a", Left "b", Right (1 :: Int), Right 99]
    retryLog <- newIORef ([] :: [(String, Word)])
    let scripted = do
          remaining <- readIORef actionScript
          case remaining of
            [] -> error "ran out of pre-programmed outcomes"
            (x : xs) -> writeIORef actionScript xs >> pure x
    result <-
      retryingIO
        3
        (\reason left -> modifyIORef retryLog ((reason, left) :))
        scripted
    result `shouldBe` Right 1
    -- Two retries before the success.
    readIORef retryLog `shouldReturn` [("b", 1), ("a", 2)]
    -- The remaining Right 99 is still in the script (untouched).
    readIORef actionScript `shouldReturn` [Right 99]

  it "passes the failure reason through to the onRetry callback" $ do
    actionScript <- newIORef [Left "first failure", Left "second failure", Right (0 :: Int)]
    retryLog <- newIORef ([] :: [(String, Word)])
    let scripted = do
          remaining <- readIORef actionScript
          case remaining of
            [] -> error "ran out of pre-programmed outcomes"
            (x : xs) -> writeIORef actionScript xs >> pure x
    _ <-
      retryingIO
        3
        (\reason left -> modifyIORef retryLog ((reason, left) :))
        scripted
    -- Logged in reverse-chronological order.
    log_ <- readIORef retryLog
    map fst log_ `shouldBe` ["second failure", "first failure"]
