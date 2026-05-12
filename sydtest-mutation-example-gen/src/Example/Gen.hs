module Example.Gen (genInt) where

import Test.QuickCheck (Gen, arbitrary)

-- | Generate an arbitrary 'Int' for use in property tests.
genInt :: Gen Int
genInt = arbitrary
