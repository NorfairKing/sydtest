{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Mutation.Plugin
  ( plugin,
    ModuleMutationAnns (..),
    parseModuleMutationAnns,
    DeadModuleDisable (..),
    deadModuleDisables,
    renderDeadModuleDisable,
    deadModuleDisableSpan,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Data, cast, gmapQ)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Driver.Env (Hsc, HscEnv (..))
import GHC.Driver.Plugins
import GHC.Driver.Session (WarningFlag (..), gopt_unset, wopt_unset)
import GHC.Serialized (deserializeWithData)
import GHC.Tc.Errors.Types (mkTcRnUnknownMessage)
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (addErrAt)
import GHC.Types.Annotations (AnnTarget (..), findAnns)
import GHC.Types.Error (mkPlainError, noHints)
import GHC.Utils.Outputable (text)
import Path
import System.IO.Unsafe (unsafePerformIO)
import Test.Syd.Mutation.Manifest (MutationGroup (..), MutationManifest (..), writeManifestFile)
import Test.Syd.Mutation.Manifest.Render (writeManifestTxtFile)
import Test.Syd.Mutation.Plugin.Instrument
import Test.Syd.Mutation.Plugin.Operators (allOperators)
import Test.Syd.Mutation.Plugin.OptParse
  ( Settings (..),
    operatorsConfigDisabled,
    resolveSettings,
  )

-- | Parsed result of all mutation-related module-level
-- @{-# ANN module ... #-}@ annotations on one module.
data ModuleMutationAnns = ModuleMutationAnns
  { -- | What the module's annotations disable across the whole module.
    mmaDisable :: !MutationDisable,
    -- | Annotation strings that announced themselves as mutation disables but
    -- are none of the forms a module accepts.  A @DisableMutationsFor <name>@
    -- is one of these: it names a local binding of the scope it annotates,
    -- and a module has none.
    mmaMalformed :: ![String]
  }
  deriving (Eq, Show)

-- | Parse all the module-level @{-# ANN module #-}@ string payloads on one
-- module.  The binding-level counterpart is 'parseFunMutationAnns'.
parseModuleMutationAnns :: [String] -> ModuleMutationAnns
parseModuleMutationAnns = foldr combine (ModuleMutationAnns (DisableOps []) [])
  where
    combine :: String -> ModuleMutationAnns -> ModuleMutationAnns
    combine s soFar = case parseDisableAnn s of
      AnnSelf d -> soFar {mmaDisable = mergeDisables d (mmaDisable soFar)}
      AnnLocal _ _ -> soFar {mmaMalformed = s : mmaMalformed soFar}
      AnnMalformed _ -> soFar {mmaMalformed = s : mmaMalformed soFar}
      AnnUnrelated -> soFar

-- | A module-level mutation annotation that disables nothing.
data DeadModuleDisable
  = -- | An annotation that looks like a mutation disable but is none of the
    -- forms a module accepts.
    MalformedModuleAnnotation String
  | -- | What the module's annotations disable is inert.
    DeadModuleDisable DeadInScope
  deriving (Eq, Show)

-- | Everything a module's mutation annotations deserve to be told about.
--
-- @known@ is every operator name the plugin has and @fired@ the operators
-- that produce a mutation in the module with its module-level disables lifted
-- (but the configuration's disables still in force, so "disables nothing"
-- means "removing this annotation would not change the manifest").
deadModuleDisables :: Set String -> ModuleMutationAnns -> Set String -> [DeadModuleDisable]
deadModuleDisables known (ModuleMutationAnns disable malformed) fired =
  map MalformedModuleAnnotation malformed
    ++ map DeadModuleDisable (deadInScope known disable fired)

-- | The compile error one dead module-level annotation earns.
renderDeadModuleDisable :: Set String -> DeadModuleDisable -> String
renderDeadModuleDisable known = \case
  MalformedModuleAnnotation ann ->
    concat
      [ "Module-level mutation annotation `",
        ann,
        "` is none of the recognised module-level disable annotations, ",
        "so it disables no mutations. ",
        "Recognised forms are `DisableMutations`, `DisableMutation: <Operator>` ",
        "and `DisableMutations: <Operator>, <Operator>`."
      ]
  DeadModuleDisable dead ->
    concat
      [ "Module-level mutation disable annotation ",
        renderDeadInScope known "this module" dead
      ]

-- | Where to point a complaint about a module-level annotation.
--
-- @recorded@ pairs each module-level annotation payload the parsed AST saw
-- with the span of its pragma, so a complaint lands on the pragma it is
-- about.  @fallback@ is used when no recorded annotation accounts for the
-- complaint, which takes a payload that is not a literal string (and so is
-- invisible in the parsed AST, though 'findAnns' still sees it).
deadModuleDisableSpan :: SrcSpan -> [(String, SrcSpan)] -> DeadModuleDisable -> SrcSpan
deadModuleDisableSpan fallback recorded dead =
  let -- Every constructor is enumerated rather than defaulted, so that adding
      -- a way for an annotation to be inert forces a decision about which
      -- annotation a complaint about it points at.
      accountsFor :: DeadInScope -> MutationDisable -> Bool
      accountsFor scope disable = case disable of
        DisableAllOps -> case scope of
          ScopeDeadAll -> True
          ScopeControlOperator -> False
          ScopeDeadOperator _ -> False
          ScopeUnknownOperator _ -> False
        DisableOps ops -> case scope of
          ScopeDeadAll -> False
          ScopeControlOperator -> any namesControlOperator ops
          ScopeDeadOperator op -> op `elem` ops
          ScopeUnknownOperator op -> op `elem` ops

      accounts :: String -> Bool
      accounts payload = case dead of
        MalformedModuleAnnotation ann -> ann == payload
        DeadModuleDisable scope -> case parseDisableAnn payload of
          AnnSelf disable -> accountsFor scope disable
          -- A "...For <name>" payload and an unparsable one are themselves
          -- complained about, as malformed; neither contributes to the
          -- module's disable, so neither can account for one being inert.
          AnnLocal _ _ -> False
          AnnMalformed _ -> False
          AnnUnrelated -> False
   in case [sp | (payload, sp) <- recorded, accounts payload] of
        (sp : _) -> sp
        [] -> fallback

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
                -- ConstConstructor reads the unfolding of an imported binding
                -- to tell whether it is an alias for a nullary constructor
                -- (Data.Map.empty is Tip), so that it does not offer the
                -- constructor the expression already is.  -O0 implies
                -- -fignore-interface-pragmas, which drops unfoldings as
                -- interfaces are read, and instrumented builds are compiled
                -- at -O0 on purpose -- so without this the recognition would
                -- silently do nothing in exactly the configuration mutation
                -- testing runs in.  Reading unfoldings does not run the
                -- simplifier, so the compile-time blowup -O0 avoids does not
                -- come back with them.
                (`gopt_unset` Opt_IgnoreInterfacePragmas) $
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
-- Also walks the parsed AST for what only it can see, and records both in a
-- process-global IORef keyed by module name for 'mutationTypeCheckAction' to
-- read back:
--
--   * When @--skip-th-splices@ is set, 'RealSrcSpan's covering every
--     'HsUntypedSplice', 'HsTypedSplice', and declaration-level 'SpliceD'.
--     'recordMutation' consults these (via the 'instrumentEnvSpliceSpans'
--     field of 'InstrumentEnv') to drop mutations whose own span is contained
--     inside any splice span.
--   * The module-level annotation payloads with the spans of their pragmas,
--     so a complaint about one can point at it.
--
-- Why parse-time: many top-level splices (e.g. @mkYesodData@,
-- @mkPersist [persistLowerCase| ... |]@) are evaluated during renaming and
-- their results are spliced into the typechecker as if they were original
-- code, so the typechecked AST no longer carries an 'ExpandedThingTc'
-- wrapper we could pattern-match on.  The original splice nodes are still
-- present in the parsed AST.  Annotation spans are parse-time for a simpler
-- reason: 'findAnns' hands over payloads without any source location.
mutationAddRuntimeImport ::
  [CommandLineOption] ->
  ModSummary ->
  ParsedResult ->
  Hsc ParsedResult
mutationAddRuntimeImport opts ms pr = do
  let mn = moduleNameString (moduleName (ms_mod ms))
  Settings
    { settingExceptions = exceptions,
      settingSkipThSplices = skipThSplices
    } <-
    liftIO $ resolveSettings opts
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions
    then pure pr
    else do
      let pm = parsedResultModule pr
          lm = hpm_module pm
          info =
            ModuleParseInfo
              { mpiSpliceSpans = if skipThSplices then collectSpliceSpans lm else [],
                mpiModuleAnns = moduleAnnStrings (unLoc lm)
              }
      liftIO $ atomicModifyIORef' moduleParseInfoMap (\m -> (Map.insert mn info m, ()))
      let runtimeImport = noLocA (simpleImportDecl (mkModuleName "Test.Syd.Mutation.Plugin.Runtime"))
          lm' = fmap (\m -> m {hsmodImports = runtimeImport : hsmodImports m}) lm
      pure pr {parsedResultModule = pm {hpm_module = lm'}}

-- | The module-level @{-# ANN module ("..." :: String) #-}@ payloads in the
-- parsed module, each with the span of its pragma.
--
-- Only looks at module-level annotations (ignores @ANN someFunction ...@) so
-- it mirrors the typecheck-phase read of 'tcg_ann_env' with a 'ModuleTarget'.
moduleAnnStrings :: HsModule GhcPs -> [(String, SrcSpan)]
moduleAnnStrings m = concatMap annString (hsmodDecls m)
  where
    annString :: LHsDecl GhcPs -> [(String, SrcSpan)]
    annString ld = case unLoc ld of
      AnnD _ (HsAnnotation _ ModuleAnnProvenance {} expr) ->
        [(s, getLocA ld) | Just s <- [annExprString expr]]
      _ -> []

    -- The payload of {-# ANN module ("DisableMutations" :: String) #-}
    -- parses as @ExprWithTySig _ "DisableMutations" String@; strip
    -- parentheses and type signatures to find the underlying string.
    annExprString :: LHsExpr GhcPs -> Maybe String
    annExprString le = case unLoc (stripExpr le) of
      HsLit _ (HsString _ s) -> Just (unpackFS s)
      _ -> Nothing

    stripExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
    stripExpr le = case unLoc le of
      HsPar _ inner -> stripExpr inner
      ExprWithTySig _ inner _ -> stripExpr inner
      _ -> le

-- | What 'mutationAddRuntimeImport' saw in one parsed module that
-- 'mutationTypeCheckAction' cannot see for itself.
data ModuleParseInfo = ModuleParseInfo
  { mpiSpliceSpans :: ![RealSrcSpan],
    mpiModuleAnns :: ![(String, SrcSpan)]
  }

noModuleParseInfo :: ModuleParseInfo
noModuleParseInfo =
  ModuleParseInfo
    { mpiSpliceSpans = [],
      mpiModuleAnns = []
    }

-- | Per-module parse-stage findings, written by 'mutationAddRuntimeImport'
-- and read back by 'mutationTypeCheckAction'.  Lives in a process-global
-- IORef because 'Hsc' and 'TcM' don't share state cleanly across compilation
-- units, and GHC may compile many modules in one process.  Switched from
-- 'stm-containers' to 'IORef'+'atomicModifyIORef'' to avoid loading the
-- 'stm' package into the GHC-as-host process, which has been observed to
-- hang the plugin during the parsed-result action on real-world libraries
-- (e.g. safe-coloured-text).
{-# NOINLINE moduleParseInfoMap #-}
moduleParseInfoMap :: IORef (Map String ModuleParseInfo)
moduleParseInfoMap = unsafePerformIO (newIORef Map.empty)

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
  Settings
    { settingExceptions = exceptions,
      settingDisabledMutations = disabledFromConfig,
      settingIgnore = ignore,
      settingSkipThSplices = skipThSplices,
      settingOperators = operatorsConfig,
      settingDebug = debug,
      settingSkipInstrumentation = skipInstrumentation,
      settingManifestDir = manifestDir
    } <-
    liftIO $ resolveSettings opts
  -- Operators turned off via @operators.<Name>.enable: false@ are disabled
  -- exactly as if listed in @disabled-mutations@.  Operator-specific options
  -- ride along in each operator's config entry and are read by the operator.
  let disabledFromOperatorsConfig = operatorsConfigDisabled operatorsConfig
  if "Paths_" `isPrefixOf` mn || mn `elem` exceptions || skipInstrumentation
    then pure tcGblEnv
    else do
      let annEnv = tcg_ann_env tcGblEnv
      let modAnns = findAnns deserializeWithData annEnv (ModuleTarget (tcg_mod tcGblEnv)) :: [String]
      let moduleDisables = parseModuleMutationAnns modAnns
      let configDisabled = disabledFromConfig ++ disabledFromOperatorsConfig
      let mSrcPath = ml_hs_file (ms_location ms)
      parseInfo <- liftIO $ Map.findWithDefault noModuleParseInfo mn <$> readIORef moduleParseInfoMap
      let spliceSpans = if skipThSplices then mpiSpliceSpans parseInfo else []
      let walk :: InstrumentPurpose -> [String] -> TcM (LHsBinds GhcTc, [MutationGroup])
          walk purpose disabled =
            runInstrument tcGblEnv allOperators purpose annEnv disabled mSrcPath debug skipThSplices operatorsConfig spliceSpans ignore $
              instrumentModule (tcg_binds tcGblEnv)
      -- What the module-level annotations are worth: the operators that fire
      -- with only the configuration's disables in force.  Measured on a walk
      -- whose result is thrown away, and only when there is an annotation to
      -- judge, since the walk costs as much as instrumenting the module does.
      let measureModule :: TcM (Set String)
          measureModule = operatorNamesIn . snd <$> walk MeasureOnly configDisabled
      let reportDeadModuleDisables :: Set String -> TcM ()
          reportDeadModuleDisables fired =
            forM_ (deadModuleDisables knownOperators moduleDisables fired) $ \dead ->
              addErrAt (deadModuleDisableSpan (moduleStartSpan ms) (mpiModuleAnns parseInfo) dead) $
                mkTcRnUnknownMessage $
                  mkPlainError noHints $
                    text (renderDeadModuleDisable knownOperators dead)
      case mmaDisable moduleDisables of
        -- The module asks not to be mutated at all, so its compiled artefacts
        -- are returned unchanged.  It is still walked once with the
        -- annotation lifted, to find out whether the annotation is worth
        -- anything, and that walk is thrown away.  A module whose
        -- instrumentation itself misbehaves belongs in @exceptions@, which is
        -- checked above this and never walks the module at all.
        DisableAllOps -> do
          reportDeadModuleDisables =<< measureModule
          liftIO $ putStrLn $ "mutation: skipping " ++ mn ++ " (DisableMutations)"
          pure tcGblEnv
        DisableOps moduleAnnNames -> do
          fired <-
            if null moduleAnnNames
              then pure Set.empty
              else measureModule
          reportDeadModuleDisables fired
          liftIO $ putStrLn $ "mutation: instrumenting " ++ mn
          (binds', groups) <- walk Instrument (configDisabled ++ moduleAnnNames)
          let totalMutations = sum [length rs | MutationGroup rs <- groups]
          liftIO $ do
            putStrLn $ "added " ++ show totalMutations ++ " mutations in " ++ show (length groups) ++ " groups"
            case manifestDir of
              Nothing -> pure ()
              Just dir -> writeModuleManifest dir mn groups
          pure tcGblEnv {tcg_binds = binds'}

-- | Every operator name the plugin has, for judging a disable annotation that
-- names one.
knownOperators :: Set String
knownOperators = Set.fromList (map operatorName allOperators)

-- | A span at the start of the module's source file, for a complaint about an
-- annotation whose own span is not available.
moduleStartSpan :: ModSummary -> SrcSpan
moduleStartSpan ms =
  let file = fromMaybe (ms_hspp_file ms) (ml_hs_file (ms_location ms))
      loc = mkSrcLoc (mkFastString file) 1 1
   in mkSrcSpan loc loc

-- | Write the manifest for one module to @<dir>/<ModuleName>.json@ and a
-- coloured human-readable rendering to @<dir>/<ModuleName>.txt@.  Each
-- module gets its own pair of files, so no locking is needed.
--
-- The @.txt@ is what reviewers diff during code review; it uses the same
-- header + unified-diff layout as the runtime's surviving-mutation report.
writeModuleManifest :: Path Abs Dir -> String -> [MutationGroup] -> IO ()
writeModuleManifest dir mn groups = do
  let manifest = MutationManifest groups
  writeManifestFile dir mn manifest
  writeManifestTxtFile dir mn manifest
