{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin (plugin) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import GHC
import GHC.Driver.Env (Hsc, HscEnv (..))
import GHC.Driver.Plugins
import GHC.Driver.Session (WarningFlag (..), wopt_unset)
import GHC.Serialized (deserializeWithData)
import GHC.Tc.Types
import GHC.Types.Annotations (AnnTarget (..), findAnns)
import Path
import Path.IO (resolveDir')
import System.Environment (lookupEnv)
import Test.Syd.Mutation.Manifest (MutationManifest (..), writeManifestFile)
import Test.Syd.Mutation.Plugin.Instrument
import Test.Syd.Mutation.Plugin.Operators (allOperators)

data DisabledMutation
  = DisableAll
  | DisableNamed String
  deriving (Eq)

-- | Parse a list of @{-# ANN #-}@ string payloads into disabled-mutation specs.
--
-- Recognised formats:
--   @"DisableMutations"@              — disable all mutations on this scope
--   @"DisableMutations: Arith, BoolLit"@ — disable the listed mutation types
--   @"DisableMutation: Arith"@        — disable exactly one named mutation type
--
-- Spaces after the colon and after each comma are optional.
parseMutationAnnStrings :: [String] -> [DisabledMutation]
parseMutationAnnStrings = concatMap parse
  where
    parse s
      | s == "DisableMutations" = [DisableAll]
      | Just rest <- stripPrefix "DisableMutations:" s =
          map (DisableNamed . trim) (splitOnComma rest)
      | Just rest <- stripPrefix "DisableMutation:" s =
          [DisableNamed (trim rest)]
      | otherwise = []

trim :: String -> String
trim = dropWhile (== ' ')

splitOnComma :: String -> [String]
splitOnComma s = case break (== ',') s of
  (w, []) -> [w]
  (w, _ : rest) -> w : splitOnComma rest

plugin :: Plugin
plugin =
  defaultPlugin
    { -- We instrument at the typechecked stage (GhcTc) so that mutations can
      -- be type-directed (e.g. replace any expression of type 'Maybe a' with
      -- 'Nothing', or only mutate '+' when the operands are numeric).
      typeCheckResultAction = mutationTypeCheckAction,
      -- Add an import of Test.Syd.Mutation.Plugin.Runtime at the parsed stage so that
      -- ifMutation and MutationId are in tcg_rdr_env for the typecheck action.
      parsedResultAction = mutationAddRuntimeImport,
      -- Suppress -Wunused-imports for the injected import of Test.Syd.Mutation.Plugin.Runtime.
      driverPlugin = \_ hscEnv ->
        pure
          hscEnv
            { hsc_dflags =
                foldl
                  wopt_unset
                  (hsc_dflags hscEnv)
                  [ Opt_WarnUnusedImports,
                    -- Guard instrumentation wraps conditions in ifMutation, making the
                    -- exhaustiveness checker conservatively warn about patterns it can
                    -- no longer prove complete.
                    Opt_WarnIncompletePatterns,
                    Opt_WarnIncompleteUniPatterns
                  ]
            },
      pluginRecompile = impurePlugin
    }

-- | Inject @import Test.Syd.Mutation.Plugin.Runtime ()@ into every instrumented module.
-- This ensures sydtest-mutation-plugin is registered as used (it is already in
-- build-depends as the plugin package), and satisfies -Wunused-packages.
mutationAddRuntimeImport ::
  [CommandLineOption] ->
  ModSummary ->
  ParsedResult ->
  Hsc ParsedResult
mutationAddRuntimeImport opts ms pr = do
  let mn = moduleNameString (moduleName (ms_mod ms))
  let exceptions = mapMaybe (stripPrefix "--exception=") opts
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions
    then pure pr
    else do
      let pm = parsedResultModule pr
          lm = hpm_module pm
          runtimeImport = noLocA (simpleImportDecl (mkModuleName "Test.Syd.Mutation.Plugin.Runtime"))
          lm' = fmap (\m -> m {hsmodImports = runtimeImport : hsmodImports m}) lm
      pure pr {parsedResultModule = pm {hpm_module = lm'}}

mutationTypeCheckAction ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
mutationTypeCheckAction opts ms tcGblEnv = do
  let mn = moduleNameString (moduleName (tcg_mod tcGblEnv))
  let exceptions = mapMaybe (stripPrefix "--exception=") opts
  let manifestDirOpt = mapMaybe (stripPrefix "--manifest=") opts
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions
    then pure tcGblEnv
    else do
      let debug = "--debug" `elem` opts
      let disabledFromOpts = mapMaybe (stripPrefix "--disable-mutation=") opts
      let annEnv = tcg_ann_env tcGblEnv
      let modAnns = findAnns deserializeWithData annEnv (ModuleTarget (tcg_mod tcGblEnv)) :: [String]
      let disabledFromModAnns = parseMutationAnnStrings modAnns
      -- A "disable-mutations" annotation with no names means disable all
      if DisableAll `elem` disabledFromModAnns
        then pure tcGblEnv
        else do
          let disabledNames = disabledFromOpts ++ [n | DisableNamed n <- disabledFromModAnns]
          liftIO $ putStrLn $ "mutation: instrumenting " ++ mn
          let mSrcPath = ml_hs_file (ms_location ms)
          (binds', mutations) <-
            runInstrument tcGblEnv allOperators annEnv disabledNames mSrcPath debug $
              instrumentModule (tcg_binds tcGblEnv)
          -- The manifest dir comes from --manifest= plugin opt, or from the
          -- MUTATION_MANIFEST_DIR env var (used by the Nix build so the store path
          -- can be passed without shell expansion in configureFlags).
          envDir <- liftIO $ lookupEnv "MUTATION_MANIFEST_DIR"
          let rawDir = case manifestDirOpt of
                (d : _) -> Just d
                [] -> envDir
          liftIO $ do
            putStrLn $ "mutation: " ++ mn ++ " total mutations: " ++ show (length mutations)
            case rawDir of
              Nothing -> pure ()
              Just raw -> do
                dir <- resolveDir' raw
                writeModuleManifest dir mn mutations
          pure tcGblEnv {tcg_binds = binds'}

-- | Write a JSON manifest file for one module to @<dir>/<ModuleName>.json@.
-- Each module gets its own file, so no locking is needed.
writeModuleManifest :: Path Abs Dir -> String -> [MutationRecord] -> IO ()
writeModuleManifest dir mn mutations =
  writeManifestFile dir mn (MutationManifest mutations)
