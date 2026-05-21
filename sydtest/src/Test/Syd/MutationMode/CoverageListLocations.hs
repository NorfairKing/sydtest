-- | The child-process entry point that lists each leaf test's source
-- location.
--
-- Prints, as a JSON array, the source location of every leaf test's
-- @it@\/@prop@\/@specify@ call site, then exits.  Used by the diff-scoped
-- mutation runner to map a changed test-source line back to the tests defined
-- there.
module Test.Syd.MutationMode.CoverageListLocations
  ( runCoverageListLocationsMode,
  )
where

import qualified Data.ByteString.Lazy as LB
import GHC.Stack (getCallStack, srcLocFile, srcLocStartLine)
import Path
import Test.Syd.Def
import Test.Syd.Mutation.Forest (flattenTestForestWithIdsAndCallStacks)
import Test.Syd.Mutation.TestLocation (TestLocation (..), encodeTestLocations)
import Test.Syd.OptParse

-- | Child-side entry point that prints, as a JSON array, the source location
-- of every leaf test's @it@\/@prop@\/@specify@ call site, then exits.  Each
-- element is a 'TestLocation' (test id, source file, line).
--
-- A leaf whose 'CallStack' is empty (it carries no recorded frame), or whose
-- source file does not parse as a relative path, is omitted: it cannot be
-- mapped to a source line, so the diff-scoped runner has no use for it.
--
-- The most-recent ('head') frame of the 'CallStack' is the
-- @it@\/@prop@\/@specify@ call site, because those combinators use
-- 'withFrozenCallStack' to fix the user's call site as the top frame.
runCoverageListLocationsMode :: Settings -> Spec -> IO ()
runCoverageListLocationsMode sets spec = do
  specForest <- execTestDefM sets spec
  let leaves = flattenTestForestWithIdsAndCallStacks specForest
      locations =
        [ TestLocation
            { testLocationTestId = tid,
              testLocationFile = relFile,
              testLocationLine = fromIntegral (srcLocStartLine srcLoc)
            }
        | (tid, cs) <- leaves,
          (_, srcLoc) : _ <- [getCallStack cs],
          Just relFile <- [parseRelFile (srcLocFile srcLoc)]
        ]
  LB.putStr (encodeTestLocations locations)
