{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Expectation where

import Control.Exception
import Control.Monad.Reader
import Test.QuickCheck.IO ()
import Test.Syd.Run

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ throwIO $ Equality (show actual) (show expected)
