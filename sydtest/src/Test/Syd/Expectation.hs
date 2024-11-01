{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module defines all the functions you will use to define your tests
module Test.Syd.Expectation where

import Control.Exception
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad (unless, when)
#endif
import Control.DeepSeq (force)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Vector as V
import GHC.Stack
import Myers.Diff (Diff, getTextDiff)
import System.Timeout (timeout)
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Text.Colour (Chunk)
import Text.Show.Pretty

-- | Assert that two values are equal according to `==`.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ throwIO =<< mkNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldBe`

-- | Assert that two values are not equal according to `==`.
shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldNotBe actual expected = unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldNotBe`

-- | Assert that a value satisfies the given predicate.
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy actual p = unless (p actual) $ throwIO $ PredicateFailedButShouldHaveSucceeded (ppShow actual) Nothing

-- | Assert that a value satisfies the given predicate with the given predicate name.
shouldSatisfyNamed :: (HasCallStack, Show a) => a -> String -> (a -> Bool) -> IO ()
shouldSatisfyNamed actual name p = unless (p actual) $ throwIO $ PredicateFailedButShouldHaveSucceeded (ppShow actual) (Just name)

infix 1 `shouldSatisfy`

-- | Assert that a value does not satisfy the given predicate.
shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldNotSatisfy actual p = when (p actual) $ throwIO $ PredicateSucceededButShouldHaveFailed (ppShow actual) Nothing

infix 1 `shouldNotSatisfy`

-- | Assert that a value does not satisfy the given predicate with the given predicate name.
shouldNotSatisfyNamed :: (HasCallStack, Show a) => a -> String -> (a -> Bool) -> IO ()
shouldNotSatisfyNamed actual name p = when (p actual) $ throwIO $ PredicateSucceededButShouldHaveFailed (ppShow actual) (Just name)

-- | Assert that computation returns the given value (according to `==`).
shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
shouldReturn computeActual expected = do
  actual <- computeActual
  unless (actual == expected) $ throwIO =<< mkNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldReturn`

-- | Assert that computation returns the given value (according to `==`).
shouldNotReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
shouldNotReturn computeActual expected = do
  actual <- computeActual
  unless (actual /= expected) $ throwIO $ EqualButShouldNotHaveBeenEqual (ppShow actual) (ppShow expected)

infix 1 `shouldNotReturn`

-- | Assert that the given list has the given prefix
shouldStartWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldStartWith a i = shouldSatisfyNamed a ("has prefix\n" <> ppShow i) (isPrefixOf i)

infix 1 `shouldStartWith`

-- | Assert that the given list has the given suffix
shouldEndWith :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldEndWith a s = shouldSatisfyNamed a ("has suffix\n" <> ppShow s) (isSuffixOf s)

infix 1 `shouldEndWith`

-- | Assert that the given list has the given infix
shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldContain a i = shouldSatisfyNamed a ("has infix\n" <> ppShow i) (isInfixOf i)

infix 1 `shouldContain`

-- | Assert that the given list contains all elements from the other
-- given list and only them, perhaps in a different order.
shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Expectation
shouldMatchList a b = shouldSatisfyNamed a ("matches list\n" <> ppShow b) (matches b)
  where
    matches x y = null (x \\ y) && null (y \\ x)

-- | Assert that two 'String's are equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
stringShouldBe :: (HasCallStack) => String -> String -> IO ()
stringShouldBe actual expected = unless (actual == expected) $ throwIO =<< stringsNotEqualButShouldHaveBeenEqual actual expected

-- | Assert that two 'Text's are equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
textShouldBe :: (HasCallStack) => Text -> Text -> IO ()
textShouldBe actual expected = unless (actual == expected) $ throwIO =<< textsNotEqualButShouldHaveBeenEqual actual expected

-- | An assertion that says two 'String's should have been equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
stringsNotEqualButShouldHaveBeenEqual :: String -> String -> IO Assertion
stringsNotEqualButShouldHaveBeenEqual actual expected = mkNotEqualButShouldHaveBeenEqual actual expected

-- | An assertion that says two 'Text's should have been equal according to `==`.
--
-- Note that using function could mess up the colours in your terminal if the Texts contain ANSI codes.
-- In that case you may want to `show` your values first or use `shouldBe` instead.
textsNotEqualButShouldHaveBeenEqual :: Text -> Text -> IO Assertion
textsNotEqualButShouldHaveBeenEqual actual expected = mkNotEqualButShouldHaveBeenEqual (T.unpack actual) (T.unpack expected)

-- | An assertion that says two 'ByteString's should have been equal according to `==`.
bytestringsNotEqualButShouldHaveBeenEqual :: ByteString -> ByteString -> IO Assertion
bytestringsNotEqualButShouldHaveBeenEqual actual expected = mkNotEqualButShouldHaveBeenEqual (show actual) (show expected)

-- | Make a test fail
--
-- Note that this is mostly backward compatible, but it has return type 'a' instead of '()' because execution will not continue beyond this function.
-- In this way it is not entirely backward compatible with hspec because now there could be an ambiguous type error.
expectationFailure :: (HasCallStack) => String -> IO a
expectationFailure = throwIO . ExpectationFailed

-- | Annotate a given action with a context, for contextual assertions
--
-- This is a completely different function from the function with the same name in hspec.
-- In hspec, context is a synonym for describe, but in sydtest, context is used for contextual failures.
context :: String -> IO a -> IO a
context s action =
  (action >>= evaluate)
    `catch` (\(SomeException e) -> throwIO (addContextToException e s))

-- | For easy hspec migration
type Expectation = IO ()

-- | For easy hspec migration
type Selector a = (a -> Bool)

-- | Assert that a given IO action throws an exception that matches the given exception
shouldThrow :: forall e a. (HasCallStack, Exception e) => IO a -> Selector e -> Expectation
action `shouldThrow` p = do
  r <- try action
  case r of
    Right _ ->
      expectationFailure $
        "did not get expected exception: " ++ exceptionType
    Left e ->
      context ("predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")") $
        e `shouldSatisfy` p
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf @e) undefined

infix 1 `shouldThrow`

anyException :: Selector SomeException
anyException = const True

anyErrorCall :: Selector ErrorCall
anyErrorCall = const True

errorCall :: String -> Selector ErrorCall
errorCall s (ErrorCallWithLocation msg _) = s == msg

anyIOException :: Selector IOException
anyIOException = const True

anyArithException :: Selector ArithException
anyArithException = const True
