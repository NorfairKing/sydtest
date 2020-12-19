{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module defines all the functions you will use to define your tests
module Test.Syd.Expectation where

import Control.Exception
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Text.Show.Pretty

-- | Assert that two values are equal according to `==`.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected) Nothing

infix 1 `shouldBe`

-- | Assert that two values are equal according to `==`, with a context.
shouldBeWith :: (HasCallStack, Show a, Eq a) => a -> a -> String -> IO ()
shouldBeWith actual expected context = unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected) (Just context)

-- | Assert that two values are not equal according to `==`.
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldNotBe actual expected = unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected) Nothing

infix 1 `shouldNotBe`

-- | Assert that two values are not equal according to `==`, with a context
shouldNotBeWith :: (HasCallStack, Show a, Eq a) => a -> a -> String -> IO ()
shouldNotBeWith actual expected context = unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected) (Just context)

-- | Assert that a value satisfies the given predicate.
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy actual p = unless (p actual) $ throwIO $ PredicateFailedButShouldHaveSucceeded (ppShow actual) Nothing

infix 1 `shouldSatisfy`

-- | Assert that a value satisfies the given predicate, with a context.
shouldSatisfyWith :: (HasCallStack, Show a) => a -> (a -> Bool) -> String -> IO ()
shouldSatisfyWith actual p context = unless (p actual) $ throwIO $ PredicateFailedButShouldHaveSucceeded (ppShow actual) (Just context)

-- | Assert that a value does not satisfy the given predicate.
shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldNotSatisfy actual p = when (p actual) $ throwIO $ PredicateSucceededButShouldHaveFailed (ppShow actual) Nothing

infix 1 `shouldNotSatisfy`

-- | Assert that a value does not satisfy the given predicate, with a context
shouldNotSatisfyWith :: (HasCallStack, Show a) => a -> (a -> Bool) -> String -> IO ()
shouldNotSatisfyWith actual p context = when (p actual) $ throwIO $ PredicateSucceededButShouldHaveFailed (ppShow actual) (Just context)

-- | Assert that computation returns the given value (according to `==`).
shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
shouldReturn computeActual expected = do
  actual <- computeActual
  unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected) Nothing

infix 1 `shouldReturn`

-- | Assert that computation returns the given value (according to `==`), with a context.
shouldReturnWith :: (HasCallStack, Show a, Eq a) => IO a -> a -> String -> IO ()
shouldReturnWith computeActual expected context = do
  actual <- computeActual
  unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected) (Just context)

-- | Assert that computation returns the given value (according to `==`).
shouldNotReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
shouldNotReturn computeActual expected = do
  actual <- computeActual
  unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected) Nothing

infix 1 `shouldNotReturn`

-- | Assert that computation returns the given value (according to `==`), with a context.
shouldNotReturnWith :: (HasCallStack, Show a, Eq a) => IO a -> a -> String -> IO ()
shouldNotReturnWith computeActual expected context = do
  actual <- computeActual
  unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected) (Just context)

-- | Assert that two 'String's are equal according to `==`.
stringShouldBe :: HasCallStack => String -> String -> IO ()
stringShouldBe actual expected = unless (actual == expected) $ throwIO $ stringsNotEqualButShouldHaveBeenEqual actual expected Nothing

-- | Assert that two 'String's are equal according to `==`, with a context.
stringShouldBeWith :: HasCallStack => String -> String -> String -> IO ()
stringShouldBeWith actual expected context = unless (actual == expected) $ throwIO $ stringsNotEqualButShouldHaveBeenEqual actual expected (Just context)

-- | Assert that two 'Text's are equal according to `==`.
textShouldBe :: HasCallStack => Text -> Text -> IO ()
textShouldBe actual expected = unless (actual == expected) $ throwIO $ textsNotEqualButShouldHaveBeenEqual actual expected Nothing

-- | Assert that two 'Text's are equal according to `==`, with a context.
textShouldBeWith :: HasCallStack => Text -> Text -> String -> IO ()
textShouldBeWith actual expected context = unless (actual == expected) $ throwIO $ NotEqualButShouldHaveBeenEqual (T.unpack actual) (T.unpack expected) (Just context)

-- | An assertion that says two 'String's should have been equal according to `==`.
stringsNotEqualButShouldHaveBeenEqual :: String -> String -> Maybe String -> Assertion
stringsNotEqualButShouldHaveBeenEqual actual expected mContext = NotEqualButShouldHaveBeenEqual actual expected mContext

-- | An assertion that says two 'Text's should have been equal according to `==`.
textsNotEqualButShouldHaveBeenEqual :: Text -> Text -> Maybe String -> Assertion
textsNotEqualButShouldHaveBeenEqual actual expected mContext = NotEqualButShouldHaveBeenEqual (T.unpack actual) (T.unpack expected) mContext

-- | An assertion that says two 'ByteString's should have been equal according to `==`.
bytestringsNotEqualButShouldHaveBeenEqual :: ByteString -> ByteString -> Maybe String -> Assertion
bytestringsNotEqualButShouldHaveBeenEqual actual expected mContext = NotEqualButShouldHaveBeenEqual (show actual) (show expected) mContext

-- | Make a test fail
--
-- Note that this is mostly backward compatible, but it has return type 'a' instead of '()' because execution will not continue beyond this function.
-- In this way it is not entirely backward compatible with hspec because now there could be an ambiguous type error.
expectationFailure :: String -> IO a
expectationFailure = throwIO . ExpectationFailed

-- | For easy hspec migration
type Expectation = IO ()
