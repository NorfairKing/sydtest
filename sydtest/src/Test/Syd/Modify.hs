{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines functions for declaring different test settings
module Test.Syd.Modify
  ( -- * Declaring different test settings
    modifyMaxSuccess,
    modifyMaxDiscardRatio,
    modifyMaxSize,
    modifyMaxShrinks,
    modifyRunSettings,
    TestRunSettings (..),

    -- * Declaring parallelism
    sequential,
    parallel,
    withParallelism,
    Parallelism (..),
  )
where

import Control.Monad.RWS.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Run
import Test.Syd.SpecDef

modifyRunSettings :: (TestRunSettings -> TestRunSettings) -> TestDefM a b c -> TestDefM a b c
modifyRunSettings = local

modifyMaxSuccess :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxSuccess func = modifyRunSettings $ \trs -> trs {testRunSettingMaxSuccess = func (testRunSettingMaxSuccess trs)}

modifyMaxDiscardRatio :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxDiscardRatio func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxSize :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxSize func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxShrinks :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxShrinks func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

-- | Declare that all tests below must be run sequentially
sequential :: TestDefM a b c -> TestDefM a b c
sequential = withParallelism Sequential

-- | Declare that all tests below may be run in parallel. (This is the default.)
parallel :: TestDefM a b c -> TestDefM a b c
parallel = withParallelism Parallel

withParallelism :: Parallelism -> TestDefM a b c -> TestDefM a b c
withParallelism p = censor ((: []) . DefParallelismNode p)
