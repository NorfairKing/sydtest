{-# LANGUAGE NumericUnderscores #-}

module Lib (runMisbehavedTestSuite) where

import Control.Concurrent
import Control.Monad
import Test.QuickCheck
import Test.Syd

runMisbehavedTestSuite :: IO ()
runMisbehavedTestSuite = do
  putStrLn $
    unlines
      [ "This test suite is not intended to pass, it is intended to practice debug behaviour.",
        "Running the entire test suite makes litle sense, instead you should practice by running individual tests and checking that you can discover the bug.",
        "Hint: Running the test suite with --debug should give you great hints.",
        "",
        unwords
          [ "--match=infinite-loop",
            ": A test that waits indefinitely. You can tell because with --debug you see that the test starts but does not finish, and with htop you can see that it uses no CPU."
          ],
        unwords
          [ "--match=infinite-evaluation",
            ": A test that has an infinite loop in pure code. You can tell because with --debug you see that the test starts but does not finish, and with htop you can see that it uses all CPU"
          ],
        unwords
          [ "--match=infinite-generator",
            ": A test that uses a generator that has an infinite loop."
          ],
        unwords
          [ "--match=self-shrinker",
            ": A test that uses a shrinking function that returns the same element, thus turning shrinking into an infinite process. You can tell because the failure uses as many shrinks as you'll allow (100 by default)."
          ],
        unwords
          [ "--match=infinite-shrinker",
            ": A test that uses a shrinking function that has an infinite loop. You can tell because with --debug, you see that the example has finished but no next one starts."
          ]
      ]

  let infiniteWaiting :: IO ()
      infiniteWaiting = forever (threadDelay 1_000_000) :: IO ()
  let pureInfiniteLoop :: a
      pureInfiniteLoop =
        let x = y
            y = x
         in x
  sydTest $ do
    it "infinite-loop" infiniteWaiting
    it "infinite-evalution" (pureInfiniteLoop :: Bool)
    it "infinite-generator" $
      let infiniteGenerator = (pure True :: Gen Bool) `suchThat` (\_ -> False)
       in forAll infiniteGenerator id
    it "self-shrinker" $
      let selfShrinker b = [b]
       in forAllShrink (pure False) selfShrinker id
    it "infinite-shrinker" $
      let infiniteShrinker :: Bool -> [Bool]
          infiniteShrinker b = infiniteShrinker (not b)
       in forAllShrink (pure False) infiniteShrinker id
