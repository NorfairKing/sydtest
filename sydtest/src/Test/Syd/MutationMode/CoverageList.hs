-- | The child-process entry point that enumerates a suite's leaf tests.
--
-- Prints every leaf test id on stdout, one per line, and exits.  Used by
-- @sydtest-mutation-driver@ to enumerate tests for the coverage phase.
module Test.Syd.MutationMode.CoverageList
  ( runCoverageListMode,
  )
where

import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Syd.Def
import Test.Syd.Mutation.Forest (flattenTestForestWithIds)
import Test.Syd.Mutation.TestId (renderTestId)
import Test.Syd.OptParse

-- | Child-side entry point that prints every leaf test id on stdout and
-- exits.  Used by 'sydtest-mutation-driver' to enumerate tests for the
-- coverage phase.
runCoverageListMode :: Settings -> Spec -> IO ()
runCoverageListMode sets spec = do
  specForest <- execTestDefM sets spec
  let leafIds = map fst (flattenTestForestWithIds specForest)
  -- Emit UTF-8 bytes, not 'putStrLn' which encodes through the handle's locale
  -- encoding: a test described with non-ASCII characters would otherwise crash
  -- this child in a C/POSIX-locale build sandbox.
  SB.putStr (TE.encodeUtf8 (T.unlines (map renderTestId leafIds)))
