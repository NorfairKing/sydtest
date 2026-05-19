{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Expand a list of 'SuitePkgSpec's (parsed from @--suite-pkg@ flags)
-- into a @Map Text SuiteConfig@ by walking each spec's
-- @<built-test-pkg-root>/test@ directory.
--
-- The 'walkSuitePkgs' function replaces the bash @suitesJsonScript@
-- the Nix harness used to inline: for each test-package it listed the
-- files in @<root>/test@, took the basename as the suite key, and
-- built a JSON object @{ "<name>": {exe, resourceDir}, ... }@ that
-- the driver then deserialised from YAML.
module Test.Syd.Mutation.Driver.SuitePkg
  ( walkSuitePkgs,
    SuitePkgWalkError (..),
  )
where

import Control.Exception (Exception, throwIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Path.IO (doesDirExist, listDirRel)
import Test.Syd.Mutation.Driver.OptParse (SuiteConfig (..), SuitePkgSpec (..))

-- | Thrown by 'walkSuitePkgs' when the walk cannot produce a usable
-- suite map.
data SuitePkgWalkError
  = -- | The @--suite-pkg@ flags expanded to zero installed
    -- test-suite executables across all listed packages.  The Nix
    -- harness used to detect this case explicitly so the user gets a
    -- friendly "you forgot to declare a test-suite" message instead
    -- of an opaque "no suites configured" further down.
    NoSuitesDeclared
  | -- | Two test-packages contributed an executable with the same
    -- basename.  This would silently overwrite the first in a 'Map';
    -- the bash version had the same hazard but it's worth a typed
    -- error here so a future caller does not silently shadow a suite.
    DuplicateSuiteName !Text
  deriving (Show)

instance Exception SuitePkgWalkError

-- | Walk each spec's @<root>/test@ directory and accumulate one
-- 'SuiteConfig' per installed test executable, keyed by the
-- executable's basename.
--
-- A spec whose @<root>/test@ directory does not exist contributes no
-- suites; that mirrors the bash @for exe in "$pkgTestDir"/*@ which
-- silently produced no iterations.
--
-- Throws 'NoSuitesDeclared' when every spec produced zero suites, and
-- 'DuplicateSuiteName' when two specs produced a suite with the same
-- key.
walkSuitePkgs :: [SuitePkgSpec] -> IO (Map.Map Text SuiteConfig)
walkSuitePkgs specs = do
  perSpec <- mapM walkOne specs
  let combined = foldr mergeNoOverwrite (Right Map.empty) perSpec
  case combined of
    Left dup -> throwIO (DuplicateSuiteName dup)
    Right m
      | Map.null m -> throwIO NoSuitesDeclared
      | otherwise -> pure m
  where
    walkOne :: SuitePkgSpec -> IO (Map.Map Text SuiteConfig)
    walkOne SuitePkgSpec {suitePkgSpecBuiltTestPkgRoot, suitePkgSpecResourceDir} = do
      let testDir = suitePkgSpecBuiltTestPkgRoot </> [reldir|test|]
      exists <- doesDirExist testDir
      if not exists
        then pure Map.empty
        else do
          (_, files) <- listDirRel testDir
          pure $
            Map.fromList
              [ ( T.pack (fromRelFile relFile),
                  SuiteConfig
                    { suiteConfigExe = testDir </> relFile,
                      suiteConfigResourceDir = Just suitePkgSpecResourceDir
                    }
                )
              | relFile <- files
              ]

    mergeNoOverwrite ::
      Map.Map Text SuiteConfig ->
      Either Text (Map.Map Text SuiteConfig) ->
      Either Text (Map.Map Text SuiteConfig)
    mergeNoOverwrite _ (Left dup) = Left dup
    mergeNoOverwrite m (Right acc) =
      case Map.lookupMin (Map.intersection m acc) of
        Just (k, _) -> Left k
        Nothing -> Right (Map.union m acc)
