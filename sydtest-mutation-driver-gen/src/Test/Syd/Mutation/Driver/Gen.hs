{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'GenValid' instances for the sydtest-mutation-driver settings
-- types.  Lives in a separate package so the generators aren't
-- compiled into the executable.
module Test.Syd.Mutation.Driver.Gen () where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Test.Syd.Mutation.Driver.OptParse

instance Validity SuiteConfig

instance GenValid SuiteConfig

instance Validity SuitePkgSpec

instance GenValid SuitePkgSpec

instance Validity MutationDriverSettings

instance GenValid MutationDriverSettings

instance Validity CoverageSettings

instance GenValid CoverageSettings
