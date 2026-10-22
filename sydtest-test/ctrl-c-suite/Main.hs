{-# LANGUAGE NumericUnderscores #-}

-- | A test suite for 'Test.Syd.InterruptSpec' to interrupt.
--
-- It announces that a test has started, so the interrupt can be sent while one
-- is running rather than during startup, and then waits long enough that any
-- suite still alive afterwards is still alive because it ignored the
-- interrupt.
module Main (main) where

import Control.Concurrent (threadDelay)
import System.IO
import Test.Syd

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  sydTest $
    it "says it is running a test and then waits to be interrupted" $ do
      putStrLn "sydtest-ctrl-c-suite is running a test"
      hFlush stdout
      threadDelay 600_000_000
