{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.TimingSpec (spec) where

import Control.Concurrent
import Control.Exception (try)
import GHC.IO.Exception (ExitCode)
import System.IO.Unsafe
import System.Timeout (timeout)
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = doNotRandomiseExecutionOrder $ do
  it "takes at least 10 milliseconds (pure)" $
    unsafePerformIO take10ms `seq`
      True
  it "takes at least 10 milliseconds (IO)" $ do
    threadDelay 10_000
  it "takes at least 10 milliseconds (property) " $
    property $ \() -> do
      threadDelay 100
  it "takes at least 100 milliseconds (pure)" $
    unsafePerformIO take100ms `seq`
      True
  it "takes at least 100 milliseconds (IO)" $ do
    threadDelay 100_000
  it "takes at least 100 milliseconds (property) " $
    property $ \() -> do
      threadDelay 10_000
  -- Ensure that long diff timeouts
  -- See https://github.com/NorfairKing/sydtest/issues/92
  it "long diff timing is bounded" $ do
    -- with n = 1000 it takes 30s on my laptop, so 10k is enough to trigger the
    -- 2s timeout.
    let n = 10_000
    let test = do
          res <- try $ sydTest $ do
            it "test" $ do
              let numbers = [0 :: Int, 1 .. n]
                  numbers' = reverse numbers

              numbers `shouldBe` numbers'

          case res of
            Right () -> fail "The subtest must fail"
            Left (_ :: ExitCode) -> pure ()

    -- We timeout after 10s. If the test timeouts by itself, it means that the
    -- diff timeout worked after 2seconds.
    --
    -- Why 10s? Because it's more than 2s, and gives enough room for scheduling
    -- and additional operations (such as generating the random numbers)
    -- without too much risk of generating a flaky test.
    timeout 10_000_000 test `shouldReturn` Just ()

{-# NOINLINE take10ms #-}
take10ms :: IO ()
take10ms = threadDelay 10_000

{-# NOINLINE take100ms #-}
take100ms :: IO ()
take100ms = threadDelay 100_000
