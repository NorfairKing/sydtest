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

-- | Assert that two 'String's are equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
stringShouldBe :: HasCallStack => String -> String -> IO ()
stringShouldBe actual expected = unless (actual == expected) $ throwIO $ stringsNotEqualButShouldHaveBeenEqual actual expected

-- | Assert that two 'Text's are equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
textShouldBe :: HasCallStack => Text -> Text -> IO ()
textShouldBe actual expected = unless (actual == expected) $ throwIO $ textsNotEqualButShouldHaveBeenEqual actual expected

-- | An assertion that says two 'String's should have been equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
stringsNotEqualButShouldHaveBeenEqual :: String -> String -> Assertion
stringsNotEqualButShouldHaveBeenEqual actual expected = NotEqualButShouldHaveBeenEqual actual expected

-- | An assertion that says two 'Text's should have been equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
textsNotEqualButShouldHaveBeenEqual :: Text -> Text -> Assertion
textsNotEqualButShouldHaveBeenEqual actual expected = NotEqualButShouldHaveBeenEqual (T.unpack actual) (T.unpack expected)

-- | An assertion that says two 'ByteString's should have been equal according to `==`.
bytestringsNotEqualButShouldHaveBeenEqual :: ByteString -> ByteString -> Assertion
bytestringsNotEqualButShouldHaveBeenEqual actual expected = NotEqualButShouldHaveBeenEqual (show actual) (show expected)

-- | Make a test fail
--
-- Note that this is mostly backward compatible, but it has return type 'a' instead of '()' because execution will not continue beyond this function.
-- In this way it is not entirely backward compatible with hspec because now there could be an ambiguous type error.
expectationFailure :: String -> IO a
expectationFailure = throwIO . ExpectationFailed

-- | Annotate a given action with a context, for contextual assertions
context :: String -> IO a -> IO a
context s action = (action >>= evaluate) `catch` (\a -> throwIO (Context a s))

-- | For easy hspec migration
type Expectation = IO ()
