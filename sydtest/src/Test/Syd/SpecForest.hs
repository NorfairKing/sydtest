{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Syd.SpecForest where

import Data.Text (Text)
import Test.QuickCheck.IO ()

type SpecForest a = [SpecTree a]

data SpecTree a
  = DescribeNode Text (SpecForest a) -- A description
  | SpecifyNode Text a -- A test with its description
  deriving (Show, Functor)

instance Foldable SpecTree where
  foldMap f = \case
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SpecifyNode _ a -> f a

instance Traversable SpecTree where
  traverse func = \case
    DescribeNode s sts -> DescribeNode s <$> traverse (traverse func) sts
    SpecifyNode s a -> SpecifyNode s <$> func a

flattenSpecForest :: SpecForest a -> [([Text], a)]
flattenSpecForest = concatMap flattenSpecTree

flattenSpecTree :: SpecTree a -> [([Text], a)]
flattenSpecTree = \case
  DescribeNode t sf -> map (\(ts, a) -> (t : ts, a)) $ flattenSpecForest sf
  SpecifyNode t a -> [([t], a)]
