{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Mutation.Plugin (plugin) where

import Control.Monad.IO.Class (liftIO)
import Data.Data (Data, cast, gmapQ)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as Map
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
import System.IO.Unsafe (unsafePerformIO)
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
      -- Recompile only when plugin flags change. We previously used
      -- 'impurePlugin' (always force recompile), but that prevents the
      -- two-step build in nix/addManifest.nix from working: the postBuild
      -- step that compiles test-suites/executables re-invokes 'Setup build'
      -- with the same plugin flags, and with 'impurePlugin' GHC would
      -- recompile the already-instrumented library un-instrumented (the
      -- env-var kill switch tells the plugin to instrument nothing).
      -- 'flagRecompile' fingerprints only the plugin's CLI options, so the
      -- library is not recompiled when the flags are identical between the
      -- two invocations. Source-level changes still trigger recompile via
      -- GHC's normal mechanism.
      pluginRecompile = flagRecompile
    }

-- | Inject @import Test.Syd.Mutation.Plugin.Runtime ()@ into every instrumented module.
-- This ensures sydtest-mutation-plugin is registered as used (it is already in
-- build-depends as the plugin package), and satisfies -Wunused-packages.
--
-- Also, when @--skip-th-splices@ is set, walk the parsed AST to collect
-- 'RealSrcSpan's covering every 'HsUntypedSplice', 'HsTypedSplice', and
-- declaration-level 'SpliceD'.  These are stored in a process-global IORef
-- keyed by module name and consulted by 'recordMutation' (via the
-- 'instrSpliceSpans' field of 'InstrumentEnv') to drop mutations whose own
-- span is contained inside any splice span.
--
-- Why parse-time: many top-level splices (e.g. @mkYesodData@,
-- @mkPersist [persistLowerCase| ... |]@) are evaluated during renaming and
-- their results are spliced into the typechecker as if they were original
-- code, so the typechecked AST no longer carries an 'ExpandedThingTc'
-- wrapper we could pattern-match on.  The original splice nodes are still
-- present in the parsed AST.
mutationAddRuntimeImport ::
  [CommandLineOption] ->
  ModSummary ->
  ParsedResult ->
  Hsc ParsedResult
mutationAddRuntimeImport opts ms pr = do
  let mn = moduleNameString (moduleName (ms_mod ms))
  let exceptions = mapMaybe (stripPrefix "--exception=") opts
  let skipThSplices = "--skip-th-splices" `elem` opts
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions
    then pure pr
    else do
      let pm = parsedResultModule pr
          lm = hpm_module pm
      liftIO $
        when skipThSplices $ do
          let spliceRanges = collectSpliceSpans lm
          atomicModifyIORef' spliceSpansRef $ \m ->
            (Map.insert mn spliceRanges m, ())
      let runtimeImport = noLocA (simpleImportDecl (mkModuleName "Test.Syd.Mutation.Plugin.Runtime"))
          lm' = fmap (\m -> m {hsmodImports = runtimeImport : hsmodImports m}) lm
      pure pr {parsedResultModule = pm {hpm_module = lm'}}

-- | Per-module splice spans collected by 'mutationAddRuntimeImport' when
-- @--skip-th-splices@ is set, read back by 'mutationTypeCheckAction' and
-- threaded through 'InstrumentEnv'.  Lives in a process-global IORef
-- because 'Hsc' and 'TcM' don't share state cleanly across compilation
-- units, and GHC may compile many modules in one process.
{-# NOINLINE spliceSpansRef #-}
spliceSpansRef :: IORef (Map.Map String [RealSrcSpan])
spliceSpansRef = unsafePerformIO (newIORef Map.empty)

when :: (Monad m) => Bool -> m () -> m ()
when True m = m
when False _ = pure ()

-- | Generic traversal that collects 'RealSrcSpan's of all parsed-AST
-- splice and quasi-quote nodes.  Uses 'Data' generics so we don't have
-- to enumerate every constructor of the AST.
collectSpliceSpans :: (Data a) => a -> [RealSrcSpan]
collectSpliceSpans x = here ++ concat (gmapQ collectSpliceSpans x)
  where
    here :: [RealSrcSpan]
    here =
      case (cast x :: Maybe (LHsExpr GhcPs)) of
        Just le | isSpliceLExpr le -> realSpan (getLocA le)
        _ -> case (cast x :: Maybe (LHsDecl GhcPs)) of
          Just ld | isSpliceLDecl ld -> realSpan (getLocA ld)
          _ -> []
    realSpan (RealSrcSpan rss _) = [rss]
    realSpan _ = []

isSpliceLExpr :: LHsExpr GhcPs -> Bool
isSpliceLExpr (L _ e) = case e of
  HsUntypedSplice _ _ -> True
  HsTypedSplice _ _ -> True
  _ -> False

isSpliceLDecl :: LHsDecl GhcPs -> Bool
isSpliceLDecl (L _ d) = case d of
  SpliceD _ _ -> True
  _ -> False

mutationTypeCheckAction ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
mutationTypeCheckAction opts ms tcGblEnv = do
  let mn = moduleNameString (moduleName (tcg_mod tcGblEnv))
  let exceptions = mapMaybe (stripPrefix "--exception=") opts
  let manifestDirOpt = mapMaybe (stripPrefix "--manifest=") opts
  -- Runtime kill switch: when MUTATION_PLUGIN_SKIP is set, the plugin loads
  -- but instruments nothing. The Nix build sets this when re-invoking
  -- 'Setup build' to compile non-library components (test-suites, executables,
  -- benchmarks) — those should not be instrumented, but we cannot drop the
  -- plugin flags from that invocation without making Cabal rebuild the
  -- already-instrumented library un-instrumented.
  skip <- liftIO $ lookupEnv "MUTATION_PLUGIN_SKIP"
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions || skip == Just "1"
    then pure tcGblEnv
    else do
      let debug = "--debug" `elem` opts
      let skipThSplices = "--skip-th-splices" `elem` opts
      let disabledFromOpts = mapMaybe (stripPrefix "--disable-mutation=") opts
      let annEnv = tcg_ann_env tcGblEnv
      let modAnns = findAnns deserializeWithData annEnv (ModuleTarget (tcg_mod tcGblEnv)) :: [String]
      let disabledFromModAnns = parseMutationAnnStrings modAnns
      -- A "disable-mutations" annotation with no names means disable all
      -- mutations for this module.  Skip both the AST walk and the splice-
      -- span lookup; the module's compiled artefacts are returned unchanged.
      if DisableAll `elem` disabledFromModAnns
        then do
          liftIO $ putStrLn $ "mutation: skipping " ++ mn ++ " (DisableMutations)"
          pure tcGblEnv
        else do
          let disabledNames = disabledFromOpts ++ [n | DisableNamed n <- disabledFromModAnns]
          liftIO $ putStrLn $ "mutation: instrumenting " ++ mn
          let mSrcPath = ml_hs_file (ms_location ms)
          spliceSpans <-
            if skipThSplices
              then liftIO $ Map.findWithDefault [] mn <$> readIORef spliceSpansRef
              else pure []
          (binds', mutations) <-
            runInstrument tcGblEnv allOperators annEnv disabledNames mSrcPath debug skipThSplices spliceSpans $
              instrumentModule (tcg_binds tcGblEnv)
          -- The manifest dir comes from --manifest= plugin opt, or from the
          -- MUTATION_MANIFEST_DIR env var (used by the Nix build so the store path
          -- can be passed without shell expansion in configureFlags).
          envDir <- liftIO $ lookupEnv "MUTATION_MANIFEST_DIR"
          let rawDir = case manifestDirOpt of
                (d : _) -> Just d
                [] -> envDir
          liftIO $ do
            putStrLn $ "added " ++ show (length mutations) ++ " mutations"
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
