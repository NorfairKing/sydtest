{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Expectation where

import Control.Exception
import Control.Monad.Reader
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Text.Show.Pretty

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ throwIO $ Equality (ppShow actual) (ppShow expected)
