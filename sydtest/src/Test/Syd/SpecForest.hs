{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Syd.SpecForest where

import Control.Exception
import Data.Text (Text)
import Test.QuickCheck.IO ()

type SpecForest a = [SpecTree a]

data SpecTree a
  = SpecifyNode Text a -- A test with its description
  | PendingNode Text (Maybe Text)
  | DescribeNode Text (SpecForest a) -- A description
  | SubForestNode (SpecForest a)
  | -- | A resource around a group of tests that failed, named by the kind of
    -- resource it was.
    --
    -- This is not a test, and counts as neither a success nor a failure,
    -- because there is no test whose fault it is: a teardown that throws does
    -- so after the tests it was around have all run and reported.  It still
    -- fails the suite, because a resource that could not be torn down is
    -- something to go and look at.
    ResourceFailureNode Text SomeException
  deriving (Functor)

instance Foldable SpecTree where
  foldMap f = \case
    SpecifyNode _ a -> f a
    PendingNode _ _ -> mempty
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SubForestNode sts -> foldMap (foldMap f) sts
    ResourceFailureNode _ _ -> mempty

instance Traversable SpecTree where
  traverse func = \case
    SpecifyNode s a -> SpecifyNode s <$> func a
    PendingNode t mr -> pure $ PendingNode t mr
    DescribeNode s sf -> DescribeNode s <$> traverse (traverse func) sf
    SubForestNode sf -> SubForestNode <$> traverse (traverse func) sf
    ResourceFailureNode t e -> pure $ ResourceFailureNode t e

flattenSpecForest :: SpecForest a -> [([Text], a)]
flattenSpecForest = concatMap flattenSpecTree

flattenSpecTree :: SpecTree a -> [([Text], a)]
flattenSpecTree = \case
  SpecifyNode t a -> [([t], a)]
  PendingNode _ _ -> []
  DescribeNode t sf ->
    map (\(ts, a) -> (t : ts, a)) $
      flattenSpecForest sf
  SubForestNode sf -> flattenSpecForest sf
  ResourceFailureNode _ _ -> []

-- | Every resource failure in a forest, with the descriptions above it.
--
-- 'flattenSpecForest' leaves these out, because they are not tests and have no
-- result.  Output that works from the flattened forest needs them separately.
flattenResourceFailures :: SpecForest a -> [([Text], SomeException)]
flattenResourceFailures = concatMap go
  where
    go :: SpecTree a -> [([Text], SomeException)]
    go = \case
      SpecifyNode _ _ -> []
      PendingNode _ _ -> []
      DescribeNode t sf -> map (\(ts, e) -> (t : ts, e)) $ flattenResourceFailures sf
      SubForestNode sf -> flattenResourceFailures sf
      ResourceFailureNode t e -> [([t], e)]
