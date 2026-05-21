{-# LANGUAGE TypeApplications #-}

module Test.Syd.TestLocationSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.TestLocation
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @TestLocation
  jsonSpec @TestLocation
