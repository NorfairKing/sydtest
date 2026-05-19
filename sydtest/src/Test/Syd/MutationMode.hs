-- | Compatibility shim: parent-side mutation runners (previously
-- 'runCoverageMode' / 'runMutationMode') have moved to the
-- @sydtest-mutation-driver@ package.
--
-- This module now re-exports the shared utilities from
-- 'Test.Syd.MutationMode.Common' and the in-tree child runners.
module Test.Syd.MutationMode
  ( module Test.Syd.MutationMode.Common,
    module Test.Syd.MutationMode.SingleCoverage,
  )
where

import Test.Syd.MutationMode.Common
import Test.Syd.MutationMode.SingleCoverage
