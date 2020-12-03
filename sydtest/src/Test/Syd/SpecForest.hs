{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Syd.SpecForest where

import Data.Text (Text)
import Test.QuickCheck.IO ()

type SpecForest a = [SpecTree a]

data SpecTree a
  = DescribeNode Text (SpecForest a) -- A description
  | SpecifyNode Text a -- A test with its description
  | SubForestNode (SpecForest a) -- A test with its description
  deriving (Functor)

instance Foldable SpecTree where
  foldMap f = \case
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SpecifyNode _ a -> f a
    SubForestNode sts -> foldMap (foldMap f) sts

instance Traversable SpecTree where
  traverse func = \case
    DescribeNode s sf -> DescribeNode s <$> traverse (traverse func) sf
    SpecifyNode s a -> SpecifyNode s <$> func a
    SubForestNode sf -> SubForestNode <$> traverse (traverse func) sf

flattenSpecForest :: SpecForest a -> [([Text], a)]
flattenSpecForest = concatMap flattenSpecTree

flattenSpecTree :: SpecTree a -> [([Text], a)]
flattenSpecTree = \case
  DescribeNode t sf -> map (\(ts, a) -> (t : ts, a)) $ flattenSpecForest sf
  SpecifyNode t a -> [([t], a)]
  SubForestNode sf -> flattenSpecForest sf
