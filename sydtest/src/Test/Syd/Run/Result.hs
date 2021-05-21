{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines the 'IsTest' class and the different instances for it.
module Test.Syd.Run.Result where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.IO ()
import Test.QuickCheck.Property hiding (Result (..))
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck.Random
import Text.Printf

data TestRunResult = TestRunResult
  { testRunResultStatus :: !TestStatus,
    testRunResultException :: !(Maybe (Either String Assertion)),
    testRunResultNumTests :: !(Maybe Word),
    testRunResultNumShrinks :: !(Maybe Word),
    testRunResultFailingInputs :: [String],
    testRunResultLabels :: !(Maybe (Map [String] Int)),
    testRunResultClasses :: !(Maybe (Map String Int)),
    testRunResultTables :: !(Maybe (Map String (Map String Int))),
    testRunResultGoldenCase :: !(Maybe GoldenCase),
    testRunResultExtraInfo :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

-- | A special exception that sydtest knows about and can display nicely in the error output
--
-- This is exported outwards so that you can define golden tests for custom types.
--
-- You will probably not want to use this directly in everyday tests, use `shouldBe` or a similar function instead.
data Assertion
  = NotEqualButShouldHaveBeenEqual String String
  | EqualButShouldNotHaveBeenEqual String String
  | PredicateSucceededButShouldHaveFailed
      String -- Value
      (Maybe String) -- Name of the predicate
  | PredicateFailedButShouldHaveSucceeded
      String -- Value
      (Maybe String) -- Name of the predicate
  | ExpectationFailed String
  | Context Assertion String
  deriving (Show, Eq, Typeable, Generic)

instance Exception Assertion

data GoldenCase
  = GoldenNotFound
  | GoldenStarted
  | GoldenReset
  deriving (Show, Eq, Typeable, Generic)
