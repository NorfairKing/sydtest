{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Expectation where

import Control.Exception
import Control.Monad.Reader
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Text.Show.Pretty

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)

shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldNotBe actual expected = unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected)

shouldSatisfy :: (HasCallStack, Show a, Eq a) => a -> (a -> Bool) -> IO ()
shouldSatisfy actual p = unless (p actual) $ throwIO $ PredicateFailedButShouldHaveSucceeded (ppShow actual)

shouldNotSatisfy :: (HasCallStack, Show a, Eq a) => a -> (a -> Bool) -> IO ()
shouldNotSatisfy actual p = unless (p actual) $ throwIO $ PredicateSucceededButShouldHaveFailed (ppShow actual)
