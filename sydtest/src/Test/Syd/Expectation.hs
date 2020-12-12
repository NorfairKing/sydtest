{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module defines all the functions you will use to define your tests
module Test.Syd.Expectation where

import Control.Exception
import Control.Monad.Reader
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Text.Show.Pretty

-- | Assert that two values are equal according to `==`.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldBe`

-- | Assert that two values are not equal according to `==`.
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldNotBe actual expected = unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldNotBe`

-- | Assert that a value satisfies the given predicate.
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy actual p = unless (p actual) $ throwIO $ PredicateFailedButShouldHaveSucceeded (ppShow actual)

infix 1 `shouldSatisfy`

-- | Assert that a value does not satisfy the given predicate.
shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldNotSatisfy actual p = when (p actual) $ throwIO $ PredicateSucceededButShouldHaveFailed (ppShow actual)

infix 1 `shouldNotSatisfy`

-- | Assert that computation returns the given value (according to `==`).
shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
shouldReturn computeActual expected = do
  actual <- computeActual
  unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldReturn`

-- | Assert that computation returns the given value (according to `==`).
shouldNotReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
shouldNotReturn computeActual expected = do
  actual <- computeActual
  unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldNotReturn`

-- | Make a test fail
expectationFailure :: String -> IO ()
expectationFailure = throwIO . ExpectationFailed

-- | For easy hspec migration
type Expectation = IO ()
