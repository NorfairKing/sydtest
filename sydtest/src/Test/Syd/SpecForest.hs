{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Syd.SpecForest where

import Data.Text (Text)
import Test.QuickCheck.IO ()

type SpecForest a = [SpecTree a]

data SpecTree a
  = SpecifyNode Text a -- A test with its description
  | PendingNode Text (Maybe Text)
  | DescribeNode Text (SpecForest a) -- A description
  | SubForestNode (SpecForest a) -- A test with its description
  deriving (Functor)

instance Foldable SpecTree where
  foldMap f = \case
    SpecifyNode _ a -> f a
    PendingNode _ _ -> mempty
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SubForestNode sts -> foldMap (foldMap f) sts

instance Traversable SpecTree where
  traverse func = \case
    SpecifyNode s a -> SpecifyNode s <$> func a
    PendingNode t mr -> pure $ PendingNode t mr
    DescribeNode s sf -> DescribeNode s <$> traverse (traverse func) sf
    SubForestNode sf -> SubForestNode <$> traverse (traverse func) sf

flattenSpecForest :: SpecForest a -> [([Text], a)]
flattenSpecForest = concatMap flattenSpecTree

flattenSpecTree :: SpecTree a -> [([Text], a)]
flattenSpecTree = \case
  SpecifyNode t a -> [([t], a)]
  PendingNode _ _ -> []
  DescribeNode t sf -> map (\(ts, a) -> (t : ts, a)) $ flattenSpecForest sf
  SubForestNode sf -> flattenSpecForest sf
