{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Syd.Mutation.Plugin.Instrument
  ( MutationRecord (..),
    MutationOperator (..),
    SrcSpanDelta (..),
    InstrumentEnv (..),
    OpAppCtx (..),
    InstrM,
    liftTcM,
    runInstrument,
    instrumentModule,
    applySpanRemoval,
    parseFunMutationAnns,
    FunMutationAnns (..),
    LocalDisable (..),
    deadDisableTargets,
  )
where

import Control.Monad (filterM, foldM, forM_)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.ByteString as SB
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC
import GHC.Builtin.Types (charTy, mkListTy)
import GHC.Core.Predicate (isEvVar)
import GHC.Data.Bag (mapBagM)
import GHC.Data.FastString (mkFastString)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session (WarningFlag (..), wopt_set)
import GHC.HsToCore (deSugarExpr)
import GHC.Serialized (deserializeWithData)
import GHC.Tc.Errors.Types (mkTcRnUnknownMessage)
import GHC.Tc.Types
import GHC.Tc.Utils.Env (tcLookupDataCon, tcLookupId)
import GHC.Tc.Utils.Monad (addErrAt, getTopEnv)
import GHC.Types.Annotations (AnnEnv, AnnTarget (..), findAnns)
import GHC.Types.Basic (Origin, isGenerated)
import GHC.Types.Error (isEmptyMessages, mkPlainError, noHints)
import GHC.Types.Id (idName)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkDataOcc, mkVarOcc)
import GHC.Types.Name.Reader (GlobalRdrEnv, greName)
import GHC.Types.SourceText (SourceText (NoSourceText))
import GHC.Types.SrcLoc (combineRealSrcSpans)
import GHC.Utils.Outputable (text)
import Path
import Path.IO (forgivingAbsence, resolveFile')
import Test.Syd.Mutation.Manifest (MutationGroup (..), MutationRecord (..))
import Test.Syd.Mutation.Plugin.Operator.Util (opOccName)
import Test.Syd.Mutation.Runtime (MutationId (..))

-- ---------------------------------------------------------------------------
-- Source delta

-- | Describes the source-level change a mutation makes, for display purposes.
data SrcSpanDelta
  = -- | Replace the matched expression's span text with this exact text.
    TokenReplace Text
  | -- | Replace the text at a specific sub-span (within the matched
    -- expression) with this exact text. Used for operator-token swaps
    -- (e.g. @+@ → @-@), where the matched expression covers the whole
    -- operator application but the textual change is only the operator
    -- token itself.
    TokenReplaceAt RealSrcSpan Text
  | -- | Remove these source line ranges from within the outer expression's span.
    SpanRemoval [RealSrcSpan]
  | -- | Prepend this text at the start of the matched expression's span.
    PrependText Text
  | -- | Wrap the matched expression's span text with a prefix and suffix.
    -- Useful when the prefix alone would re-parse with the wrong precedence
    -- (e.g. @not n < 0@ parses as @(not n) < 0@, so we want @not (n < 0)@).
    WrapWithText Text Text
  | -- | Replace an arbitrary outer span (wider than the matched expression's
    -- own span) with the given text.  Used by the const-family operators
    -- when the matched expression sits at the operator position of an infix
    -- application: text-splicing the operator token alone would produce
    -- nonsense source (e.g. replacing @||@ with @\\_ _ -> True@ in
    -- @not a || b@), so we replace the whole @OpApp@ span with a prefix-form
    -- rewrite instead.
    ReplaceOuterSpan RealSrcSpan Text

-- ---------------------------------------------------------------------------
-- Operator

-- | A single mutation operator.
--
-- 'operatorMatch' inspects a type-checked expression and, if it is a candidate
-- for this operator, returns the mutated replacement together with the
-- human-readable original and replacement strings used in the manifest.
-- Returning 'Nothing' means "not a candidate".
--
-- The matched expression has already been recursively instrumented by the
-- time 'operatorMatch' is called, so the operator only needs to inspect the
-- top-level shape.
data MutationOperator = MutationOperator
  { operatorName :: String,
    operatorDescription :: String,
    -- | @Just action@ if this expression is a mutation candidate, @Nothing@ otherwise.
    -- The action runs in 'InstrM' so it can look up replacement operator ids via
    -- 'TcM' when needed (e.g. swapping @(+)@ for @(-)@).
    -- The action returns a list of @(ty, mutated, originalStr, replacementStr, delta)@
    -- tuples, one per distinct mutation to generate at this site.  Each gets its own
    -- 'MutationId' and is wrapped independently as a nested 'ifMutation' call.
    -- 'tryMutateWith' will validate each alternative via desugaring and silently drop
    -- any that produce diagnostics (e.g. overflowed literals).
    operatorMatch :: LHsExpr GhcTc -> Maybe (InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)])
  }

-- ---------------------------------------------------------------------------
-- Monad

data InstrumentEnv = InstrumentEnv
  { instrumentEnvModule :: Module,
    -- | The module's 'GlobalRdrEnv', used by operators to look up replacement ids.
    instrumentEnvRdrEnv :: GlobalRdrEnv,
    -- | Id for Test.Syd.Mutation.Runtime.ifMutation, looked up once per module.
    instrumentEnvIfMutationId :: Id,
    -- | DataCon for Test.Syd.Mutation.Runtime.MutationId, looked up once per module.
    instrumentEnvMutationIdCon :: DataCon,
    -- | Operators to try at each expression site.
    instrumentEnvOperators :: [MutationOperator],
    -- | Annotation environment for reading {-# ANN #-} annotations.
    instrumentEnvAnnEnv :: AnnEnv,
    -- | Mutation type names disabled at module or global scope.
    instrumentEnvDisabledMutations :: [String],
    -- | Source file (relative path) and pre-read lines, read once per module.
    instrumentEnvSourceFile :: Maybe (Path Rel File, [Text]),
    -- | Print each mutation site as it is recorded (enabled by --debug plugin opt).
    instrumentEnvDebug :: Bool,
    -- | When True, do not instrument the expanded body of TH splices and
    -- quasi-quotes.  Detected two ways: (a) matching
    -- @XExpr (ExpandedThingTc orig _)@ where @orig@ is an 'OrigExpr'
    -- wrapping a splice node — covers operator-style expansions; (b)
    -- checking whether a mutation's 'RealSrcSpan' lies inside any span in
    -- 'instrumentEnvSpliceSpans' — covers top-level splices like @mkYesodData@
    -- and quasi-quotes that are evaluated at rename time and leave no
    -- 'ExpandedThingTc' wrapper in the typechecked AST.  Enabled by
    -- @--skip-th-splices@.
    instrumentEnvSkipThSplices :: Bool,
    -- | Splice 'RealSrcSpan's collected from the parsed AST (one entry per
    -- @$(...)@, @[name|...|]@, or 'SpliceD').  Only populated when
    -- 'instrumentEnvSkipThSplices' is True; used by 'recordMutation' to drop
    -- mutations whose own span is contained in any splice span.
    instrumentEnvSpliceSpans :: [RealSrcSpan],
    -- | Unqualified identifier names whose calls are ignored: any expression
    -- whose syntactic head is one of these is left untouched by
    -- 'instrumentLExpr' (no operators tried, no recursion into children).
    -- Populated from the @ignore@ YAML config field.  Used to silence noisy
    -- mutations on calls like @logDebug "..."@ where dropping or mutating
    -- the call almost never affects observable behaviour, producing
    -- surviving mutants that are pure noise.
    instrumentEnvIgnore :: [String],
    -- | True when instrumenting a guard expression (BodyStmt inside a GRHS).
    -- Used by ConstBool to suppress the e->False alternative, which would make
    -- the guard non-exhaustive and throw an exception that tests can't catch.
    instrumentEnvInGuard :: Bool,
    -- | Per-local-binding mutation disables that apply inside the currently
    -- enclosing top-level binding. Keyed by the local binding's source-level
    -- name (its 'OccName' string).  Populated by 'withFunBindEnv' when an
    -- outer @{-# ANN funName ("DisableMutationsFor inner..." :: String) #-}@
    -- annotation is present, and consumed by 'instrumentBind' when entering
    -- a matching local 'FunBind' or 'VarBind'.  Cleared at the start of each
    -- top-level binding so disables don't leak across them.
    instrumentEnvLocalDisables :: Map String LocalDisable,
    -- | True when we are currently instrumenting a local binding inside a
    -- @let@ or @where@.  Gates 'withLocalDisable' so it never matches the
    -- top-level binding itself — which would happen at @XHsBindsLR
    -- AbsBinds@, whose inner 'FunBind' has the same source name as its poly
    -- export and would otherwise spuriously consume a 'DisableMutationsFor'
    -- entry intended for an identically named local.
    instrumentEnvInLocalLet :: Bool,
    -- | When 'Just', we are currently walking the typechecker's expansion
    -- of a source-level @OpApp@ (an infix operator application like
    -- @not a || b@).  The walker stashes the original 'OpApp' source spans
    -- and operand text here so const-family operators that fire on the
    -- bare operator (or a partial application carrying the operator's span)
    -- can emit a 'ReplaceOuterSpan' delta that rewrites the whole infix
    -- expression in prefix form, instead of text-splicing a non-operator
    -- replacement into the operator-token slot and producing nonsense
    -- source.  Cleared (set back to 'Nothing') when the walker leaves the
    -- expansion.
    instrumentEnvOpAppCtx :: Maybe OpAppCtx,
    -- | How many enclosing 'HsApp' function-position layers we are inside.
    --
    -- Incremented when the walker descends into the function side ('f') of
    -- an @HsApp f a@; reset to 0 when descending into the argument side
    -- ('a') or any non-application sub-expression (lambda body, case
    -- scrutinee, let body, list element, ...).  Preserved through
    -- 'HsPar', 'WrapExpr', and 'ExpandedThingTc' wrappers, which do not
    -- disrupt the "I am the function head" relationship.
    --
    -- The const-family operators ('ConstNothing' / 'ConstEmptyList' /
    -- 'ConstBool') consult this to skip arity-\>=1 mutations at sites where
    -- the matched expression is already saturated by enclosing
    -- applications: an arity-N const-fn mutant @\\_ ... _ -> v@ inserted
    -- under @N@ levels of 'HsApp' is dominated by the arity-0 mutation on
    -- the outermost saturated expression (both reduce to @v@), so emitting
    -- both is pure noise.
    instrumentEnvAppDepth :: Int
  }

-- | Original source information for an enclosing infix @OpApp@ — captured
-- when the walker enters the typechecker's expansion of one.  See
-- 'instrumentEnvOpAppCtx'.
data OpAppCtx = OpAppCtx
  { -- | The full source span of the @OpApp@ (from the start of LHS to the
    -- end of RHS).  Used as the outer span when rewriting the preview.
    opAppOuterSpan :: RealSrcSpan,
    -- | The source span of the operator token itself.  An operator at this
    -- exact span is the bare-operator mutation site.
    opAppOpSpan :: RealSrcSpan,
    -- | The exact source text of the LHS operand.
    opAppLhsText :: Text,
    -- | The exact source text of the RHS operand.
    opAppRhsText :: Text
  }

-- | The 'StateT' state: names of 'DisableMutationsFor' targets that
-- 'applyLocalDisables' has matched against a real local binding within the
-- body currently being walked.  'withFunBindEnv' brackets each declaring
-- binding by resetting this to empty, walking the body, and reading it back:
-- any declared target still absent named a binding that does not exist, so the
-- annotation is dead and we raise a compile error (see 'deadDisableTargets').
-- Bracketing per declaring binding keeps the bookkeeping correct even when
-- several bindings disable the same local name (e.g. an @inner@ in each),
-- which a single threaded set could not.
type InstrM = WriterT [MutationGroup] (StateT (Set String) (ReaderT InstrumentEnv TcM))

liftTcM :: TcM a -> InstrM a
liftTcM = lift . lift . lift

-- ---------------------------------------------------------------------------
-- Entry point

-- | Look up the Ids we need from the module's GlobalRdrEnv (populated by the
-- injected @import Test.Syd.Mutation.Plugin.Runtime@ from the parsed-stage plugin),
-- then run the instrumentation.
runInstrument ::
  TcGblEnv ->
  [MutationOperator] ->
  -- | Annotation environment for reading {-# ANN #-} annotations.
  AnnEnv ->
  -- | Mutation type names disabled globally or at module scope.
  [String] ->
  -- | Source file path for this module (used to read context lines once).
  Maybe FilePath ->
  -- | Print each mutation site as it is recorded.
  Bool ->
  -- | Skip TH splices and quasi-quotes.
  Bool ->
  -- | Splice spans collected at parse time, used to filter mutations.
  [RealSrcSpan] ->
  -- | Unqualified identifier names whose call expressions are ignored: no
  -- mutations are produced for them or anything inside their argument
  -- subtrees.
  [String] ->
  InstrM a ->
  TcM (a, [MutationGroup])
runInstrument tcGblEnv operators annEnv disabledMutations mSrcPath debug skipThSplices spliceSpans ignore action = do
  let rdrEnv = tcg_rdr_env tcGblEnv
      modul = tcg_mod tcGblEnv
  ifMutId <- lookupRdrEnvId rdrEnv "ifMutation"
  mutIdCon <- lookupRdrEnvDataCon rdrEnv "MutationId"
  mSrcFile <- liftIO $ case mSrcPath >>= parseRelFile of
    Nothing -> pure Nothing
    Just relFile -> do
      absFile <- resolveFile' (fromRelFile relFile)
      mbs <- forgivingAbsence (SB.readFile (fromAbsFile absFile))
      pure $ fmap (\bs -> (relFile, T.lines (TE.decodeUtf8Lenient bs))) mbs
  let activeOperators = filter (\op -> operatorName op `notElem` disabledMutations) operators
  flip
    runReaderT
    InstrumentEnv
      { instrumentEnvModule = modul,
        instrumentEnvRdrEnv = rdrEnv,
        instrumentEnvIfMutationId = ifMutId,
        instrumentEnvMutationIdCon = mutIdCon,
        instrumentEnvOperators = activeOperators,
        instrumentEnvAnnEnv = annEnv,
        instrumentEnvDisabledMutations = disabledMutations,
        instrumentEnvSourceFile = mSrcFile,
        instrumentEnvDebug = debug,
        instrumentEnvSkipThSplices = skipThSplices,
        instrumentEnvSpliceSpans = spliceSpans,
        instrumentEnvIgnore = ignore,
        instrumentEnvInGuard = False,
        instrumentEnvAppDepth = 0,
        instrumentEnvLocalDisables = Map.empty,
        instrumentEnvInLocalLet = False,
        instrumentEnvOpAppCtx = Nothing
      }
    $ evalStateT (runWriterT action) Set.empty

-- | Look up a value Id from the module's GlobalRdrEnv by OccName.
-- The name must be in scope via the injected import of Test.Syd.Mutation.Plugin.Runtime.
lookupRdrEnvId :: GlobalRdrEnv -> String -> TcM Id
lookupRdrEnvId rdrEnv occStr =
  case lookupOccEnv rdrEnv (mkVarOcc occStr) of
    Just (gre : _) -> tcLookupId (greName gre)
    _ ->
      liftIO $
        ioError $
          userError $
            "mutation: " ++ occStr ++ " not in scope (is Test.Syd.Mutation.Plugin.Runtime imported?)"

-- | Look up a DataCon from the module's GlobalRdrEnv by OccName.
lookupRdrEnvDataCon :: GlobalRdrEnv -> String -> TcM DataCon
lookupRdrEnvDataCon rdrEnv occStr =
  case lookupOccEnv rdrEnv (mkDataOcc occStr) of
    Just (gre : _) -> tcLookupDataCon (greName gre)
    _ ->
      liftIO $
        ioError $
          userError $
            "mutation: " ++ occStr ++ " not in scope (is Test.Syd.Mutation.Plugin.Runtime imported?)"

instrumentModule :: LHsBinds GhcTc -> InstrM (LHsBinds GhcTc)
instrumentModule = instrumentBinds

-- ---------------------------------------------------------------------------
-- Bind walkers

instrumentBinds :: LHsBinds GhcTc -> InstrM (LHsBinds GhcTc)
instrumentBinds = mapBagM (traverse instrumentBind)

instrumentBind :: HsBind GhcTc -> InstrM (HsBind GhcTc)
instrumentBind = \case
  -- Skip compiler-generated bindings entirely.  Stock/anyclass-derived
  -- instance methods and the default-method copies GHC fills into an explicit
  -- instance both land in 'tcg_binds' as 'FunBind's whose 'MatchGroup' carries
  -- a 'Generated' 'Origin', with source spans pointing at the @deriving@ clause
  -- or the instance head.  Instrumenting them is meaningless: the diffs are
  -- nonsense (e.g. @deriving (Show, (\\_ _ -> False), ...)@ or @(\\_ -> [])@
  -- replacing a whole instance head) and the mutants can never be killed by a
  -- test of the user's own source, so they are pure noise in the report.
  --
  -- User-written instance methods (e.g. an explicit @validate = ...@) keep a
  -- 'FromSource' origin and are still instrumented.  Note we must NOT apply
  -- this filter in 'instrumentMatchGroup', because the renamer's @do@-block
  -- expansion produces 'Generated' lambda 'MatchGroup's whose bodies are the
  -- user's own code and do need instrumenting.
  b@(FunBind _ _ mg) | isGenerated (matchGroupOrigin mg) -> pure b
  FunBind x name mg -> do
    let funName = idName (unLoc name)
    -- 'withLocalDisable' is a no-op outside an enclosing local-let scope (the
    -- 'instrumentEnvInLocalLet' flag is false at module top-level), so it only fires
    -- on truly local bindings.  Inside, 'withFunBindEnv' sees no annotations
    -- (locals can't be ANN'd) and leaves the disable map intact.
    mg' <- withLocalDisable funName (withFunBindEnv funName (instrumentMatchGroup mg))
    pure (FunBind x name mg')
  PatBind x pat mult rhs -> PatBind x pat mult <$> instrumentGRHSs rhs
  -- Skip evidence bindings.  The typechecker materialises the dictionaries an
  -- instance needs as @VarBind@s (e.g. @$dShow@, @$dEnum@) whose source spans
  -- point at the @deriving@ clause or the instance head and whose right-hand
  -- sides reference the (list- and bool-returning) method components of the
  -- dictionary.  Instrumenting them produces the same nonsense mutants as the
  -- derived methods themselves (@deriving (Show, (\\_ -> []), ...)@), so leave
  -- evidence alone.  User-written @x = e@ bindings are never evidence, so they
  -- are still instrumented.
  b@(VarBind _ var _) | isEvVar var -> pure b
  VarBind x var rhs -> VarBind x var <$> withLocalDisable (idName var) (instrumentLExpr rhs)
  PatSynBind x psb -> pure (PatSynBind x psb)
  -- At GhcTc, XHsBindsLR carries an AbsBinds (see XXHsBindsLR instance).
  -- {-# ANN f #-} annotations attach to the poly Id (abe_poly), but the inner
  -- FunBinds carry mono Ids.  Apply poly-name annotations here so they cover
  -- the inner binds, which withFunBindEnv would otherwise miss.
  XHsBindsLR ab@AbsBinds {abs_binds, abs_exports} -> do
    let polyNames = map (idName . abe_poly) abs_exports
    binds' <- foldr withFunBindEnv (mapBagM (traverse instrumentBind) abs_binds) polyNames
    pure (XHsBindsLR ab {abs_binds = binds'})

-- | If @bindName@'s source-level name is a key in 'instrumentEnvLocalDisables',
-- narrow 'instrumentEnvOperators' for the wrapped action and remove the entry from
-- the map (so the same disable doesn't re-fire on a shadowed inner binding).
--
-- This is the lookup half of @DisableMutationsFor <name>@: the outer
-- top-level binding's 'withFunBindEnv' installed the map; this is where
-- the local binding's RHS instrumentation consults it.
--
-- No-op when 'instrumentEnvInLocalLet' is False — the matching only applies inside a
-- @let@ or @where@, not to the top-level binding itself (whose inner FunBind
-- shares a source name with its poly export).
withLocalDisable :: Name -> InstrM a -> InstrM a
withLocalDisable bindName action = do
  InstrumentEnv {instrumentEnvInLocalLet} <- ask
  if instrumentEnvInLocalLet
    then applyLocalDisables [getOccString bindName] action
    else action

-- | Like 'withLocalDisable' but takes a list of 'Id's (typically the binders
-- of a @do@-block 'BindStmt' pattern) and matches any of them.
--
-- Always checks regardless of 'instrumentEnvInLocalLet': @<-@-bound names cannot
-- collide with the enclosing top-level binding the way the
-- @XHsBindsLR AbsBinds@ inner 'FunBind' can, so the gate is unnecessary
-- here.
withLocalDisableMany :: [Id] -> InstrM a -> InstrM a
withLocalDisableMany ids =
  applyLocalDisables (map (getOccString . idName) ids)

-- | Common implementation: look up any of @occs@ in 'instrumentEnvLocalDisables',
-- narrow operators by the merged disable list, and remove all matched
-- entries from the map for the wrapped action.
applyLocalDisables :: [String] -> InstrM a -> InstrM a
applyLocalDisables occs action = do
  InstrumentEnv {instrumentEnvLocalDisables, instrumentEnvOperators} <- ask
  let matches = [(occ, d) | occ <- occs, Just d <- [Map.lookup occ instrumentEnvLocalDisables]]
  case matches of
    [] -> action
    _ -> do
      modify' (Set.union (Set.fromList (map fst matches)))
      let disableAll = any ((== DisableAllOps) . snd) matches
          namedDisables = concat [ns | (_, DisableOps ns) <- matches]
          operators' =
            if disableAll
              then []
              else filter (\op -> operatorName op `notElem` namedDisables) instrumentEnvOperators
          remaining = foldr (Map.delete . fst) instrumentEnvLocalDisables matches
      local
        ( \env ->
            env
              { instrumentEnvOperators = operators',
                instrumentEnvLocalDisables = remaining
              }
        )
        action

-- | The 'DisableMutationsFor' targets that never matched a local binding.
--
-- @declared@ is the local-disable map a top-level binding's annotations
-- installed (keyed by target name); @consumed@ is the set of target names that
-- 'applyLocalDisables' actually matched against a real local binding while
-- walking that binding's body.  A declared target absent from @consumed@ named
-- a binding that does not exist (e.g. a typo or a since-renamed local), so the
-- annotation disables nothing and should be removed.
deadDisableTargets :: Map String LocalDisable -> Set String -> [String]
deadDisableTargets declared consumed =
  filter (`Set.notMember` consumed) (Map.keys declared)

-- | Run an instrumentation action with the operator list filtered by any
-- {-# ANN funName ("DisableMutations..." :: String) #-} annotations on the
-- given top-level name.
--
-- Also reads any @DisableMutationsFor <localName>...@ annotations on the
-- same top-level name and installs them in 'instrumentEnvLocalDisables' so that
-- 'instrumentBind' can scope them down to the right local binding.
--
-- @ANN@ targets only resolve for top-level names, so for local bindings
-- 'findAnns' returns @[]@; in that case this is the identity on the
-- environment (the existing 'instrumentEnvLocalDisables' from the enclosing
-- top-level binding is preserved).
withFunBindEnv :: Name -> InstrM a -> InstrM a
withFunBindEnv funName action = do
  InstrumentEnv {instrumentEnvAnnEnv, instrumentEnvOperators, instrumentEnvLocalDisables} <- ask
  let funAnns = findAnns deserializeWithData instrumentEnvAnnEnv (NamedTarget funName) :: [String]
      FunMutationAnns selfDisable localDisables = parseFunMutationAnns funAnns
      operators' = case selfDisable of
        DisableAllOps -> []
        DisableOps disabled -> filter (\op -> operatorName op `notElem` disabled) instrumentEnvOperators
      -- Only replace instrumentEnvLocalDisables when this binding actually contributes
      -- entries.  Local bindings have no ANN entries, so they return an empty
      -- map and we must keep the enclosing top-level binding's map intact.
      localDisables' =
        if Map.null localDisables
          then instrumentEnvLocalDisables
          else localDisables
  if Map.null localDisables
    then
      -- This binding contributes no 'DisableMutationsFor' targets, so it leaves
      -- the consumed-targets state alone: a local binding consumed inside it
      -- must still count for whichever outer binding declared the target.
      local (\env -> env {instrumentEnvOperators = operators'}) action
    else do
      -- This binding declares targets, so bracket the consumed-targets state:
      -- reset it to empty, walk the body, then read back exactly what this
      -- binding's body consumed.  A single threaded set would be wrong here,
      -- since several bindings can disable the same local name (e.g. an @inner@
      -- in each), and the first consumption would mask the others.
      saved <- get
      put Set.empty
      result <-
        local
          ( \env ->
              env
                { instrumentEnvOperators = operators',
                  instrumentEnvLocalDisables = localDisables'
                }
          )
          action
      consumed <- get
      -- Restore the enclosing scope's consumption, plus what this body added,
      -- so an outer declaring binding still sees targets consumed in here.
      put (saved `Set.union` consumed)
      -- Any declared target that was never consumed named a binding that does
      -- not exist, so the annotation disables nothing: raise a compile error
      -- asking for its removal.
      forM_ (deadDisableTargets localDisables consumed) $ \target ->
        liftTcM $
          addErrAt (nameSrcSpan funName) $
            mkTcRnUnknownMessage $
              mkPlainError noHints $
                text $
                  "Mutation DisableMutationsFor annotation on `"
                    ++ getOccString funName
                    ++ "` targets `"
                    ++ target
                    ++ "`, which is not a local binding in its body. "
                    ++ "It disables no mutations; remove it."
      pure result

-- | Parsed result of all mutation-related @{-# ANN funName ... #-}@
-- annotations on a single top-level binding.
data FunMutationAnns = FunMutationAnns
  { -- | What to do with mutations inside the binding itself.
    famSelfDisable :: !LocalDisable,
    -- | Disables targeted at specific local bindings within this top-level
    -- binding's body, keyed by the local binding's user-visible name.
    famLocalDisables :: !(Map String LocalDisable)
  }
  deriving (Eq, Show)

-- | What a single annotation disables on a scope.
data LocalDisable
  = -- | Disable all operators on this scope.
    DisableAllOps
  | -- | Disable exactly the listed operator names on this scope. An empty
    -- list means no disables apply at this scope.
    DisableOps [String]
  deriving (Eq, Show)

-- | Parse a list of @{-# ANN funName #-}@ string payloads.
--
-- Recognised forms (whitespace after the colon and commas is tolerated):
--
--   * @DisableMutations@                            — disable all operators on the binding.
--   * @DisableMutations: A, B@                      — disable the listed operators on the binding.
--   * @DisableMutation: A@                          — disable the single named operator on the binding.
--   * @DisableMutationsFor <name>@                  — disable all operators inside the
--                                                    local binding named @\<name\>@.
--   * @DisableMutationsFor <name>: A, B@            — disable the listed operators inside @\<name\>@.
--   * @DisableMutationFor <name>: A@                — disable the single named operator inside @\<name\>@.
--
-- @\<name\>@ matches the source-level identifier of a local binding inside
-- the annotated top-level function's body. Unrecognised strings are ignored.
parseFunMutationAnns :: [String] -> FunMutationAnns
parseFunMutationAnns =
  foldr combine (FunMutationAnns (DisableOps []) Map.empty) . concatMap parseOne
  where
    combine (Self d) (FunMutationAnns s ls) = FunMutationAnns (mergeDisable s d) ls
    combine (Local n d) (FunMutationAnns s ls) =
      FunMutationAnns s (Map.insertWith mergeDisable n d ls)

    parseOne :: String -> [ParsedAnn]
    parseOne s
      -- Try the "...For <name>" forms first so they don't get swallowed by
      -- the shorter prefixes.
      | Just rest <- stripPrefix "DisableMutationsFor " s =
          [Local n d | (n, d) <- splitForPayload rest DisableAllOps]
      | Just rest <- stripPrefix "DisableMutationFor " s =
          [Local n d | (n, d) <- splitForPayload rest (DisableOps [])]
      | s == "DisableMutations" = [Self DisableAllOps]
      | Just rest <- stripPrefix "DisableMutations:" s =
          [Self (DisableOps (map trim (splitOnComma rest)))]
      | Just rest <- stripPrefix "DisableMutation:" s =
          [Self (DisableOps [trim rest])]
      | otherwise = []

    -- After "DisableMutationsFor " (or "DisableMutationFor "), the rest is
    -- either "<name>"            (no colon → default disable)
    --        "<name>: A, B, ..."  (colon → DisableOps with named operators).
    -- For DisableMutationsFor without a colon, default = DisableAllOps.
    -- For DisableMutationFor without a colon, default = DisableOps [] (no-op),
    -- but we accept it for symmetry.
    splitForPayload :: String -> LocalDisable -> [(String, LocalDisable)]
    splitForPayload rest defaultDisable =
      case break (== ':') (trim rest) of
        (name, []) ->
          let n = trimTrailing name
           in [(n, defaultDisable) | not (null n)]
        (name, _ : opsRest) ->
          let n = trimTrailing name
              ops = map trim (splitOnComma opsRest)
           in [(n, DisableOps ops) | not (null n)]

    mergeDisable :: LocalDisable -> LocalDisable -> LocalDisable
    mergeDisable DisableAllOps _ = DisableAllOps
    mergeDisable _ DisableAllOps = DisableAllOps
    mergeDisable (DisableOps a) (DisableOps b) = DisableOps (a ++ b)

    trim = dropWhile (== ' ')
    trimTrailing = reverse . dropWhile (== ' ') . reverse . trim

data ParsedAnn
  = Self LocalDisable
  | Local String LocalDisable

splitOnComma :: String -> [String]
splitOnComma s = case break (== ',') s of
  (w, []) -> [w]
  (w, _ : rest) -> w : splitOnComma rest

-- | The 'Origin' (source vs compiler-generated) recorded on a typechecked
-- 'MatchGroup'.  Used by 'instrumentBind' to skip derived and default-method
-- bindings.
matchGroupOrigin :: MatchGroup GhcTc (LHsExpr GhcTc) -> Origin
matchGroupOrigin (MG x _) = mg_origin x

instrumentMatchGroup ::
  MatchGroup GhcTc (LHsExpr GhcTc) ->
  InstrM (MatchGroup GhcTc (LHsExpr GhcTc))
instrumentMatchGroup = \case
  MG x alts -> MG x <$> traverse (mapM (traverse instrumentMatch)) alts

instrumentMatch ::
  Match GhcTc (LHsExpr GhcTc) ->
  InstrM (Match GhcTc (LHsExpr GhcTc))
instrumentMatch = \case
  Match x ctx pats body -> Match x ctx pats <$> instrumentGRHSs body

instrumentGRHSs ::
  GRHSs GhcTc (LHsExpr GhcTc) ->
  InstrM (GRHSs GhcTc (LHsExpr GhcTc))
instrumentGRHSs = \case
  GRHSs x rhs localBinds ->
    GRHSs x <$> mapM (traverse instrumentGRHS) rhs <*> instrumentLocalBinds localBinds

instrumentGRHS ::
  GRHS GhcTc (LHsExpr GhcTc) ->
  InstrM (GRHS GhcTc (LHsExpr GhcTc))
instrumentGRHS = \case
  GRHS x guards body ->
    GRHS x
      <$> mapM instrumentStmt guards
      <*> instrumentLExpr body

instrumentLocalBinds :: HsLocalBinds GhcTc -> InstrM (HsLocalBinds GhcTc)
instrumentLocalBinds = \case
  HsValBinds x valBinds ->
    HsValBinds x
      <$> local (\env -> env {instrumentEnvInLocalLet = True}) (instrumentValBinds valBinds)
  lbs -> pure lbs

instrumentValBinds :: HsValBinds GhcTc -> InstrM (HsValBinds GhcTc)
instrumentValBinds = \case
  ValBinds x binds sigs ->
    ValBinds x <$> mapBagM (traverse instrumentBind) binds <*> pure sigs
  XValBindsLR (NValBinds binds sigs) ->
    XValBindsLR . flip NValBinds sigs
      <$> mapM (\(f, bag) -> (f,) <$> mapBagM (traverse instrumentBind) bag) binds

-- ---------------------------------------------------------------------------
-- Expression walkers

instrumentLExpr :: LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
instrumentLExpr le = do
  -- 'ignore' filter: if this expression's syntactic head is one of the
  -- configured names (e.g. "logDebug"), return it unchanged and do NOT
  -- recurse into children.  This silences mutations both on the call itself
  -- (the RemoveAction operator would otherwise turn @do { logDebug "x"; k }@
  -- into @do { k }@) and on every sub-expression of the arguments (so the
  -- string literal, an inner @show n@, etc. are also left alone).
  --
  -- 'opOccName' peels through HsApp/HsAppType/WrapExpr/HsPar wrappers to find
  -- the head identifier's unqualified 'OccName' string.  We match against the
  -- bare name (no module qualifier), so users configure e.g. "logDebug"
  -- regardless of whether it's imported qualified or unqualified.
  expressionToIgnore <- asks instrumentEnvIgnore
  case opOccName le of
    Just occ | occ `elem` expressionToIgnore -> pure le
    _ -> instrumentLExprGo le

instrumentLExprGo :: LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
instrumentLExprGo le = do
  -- ORDERING INVARIANT: instrument children before applying operators, but
  -- pass the ORIGINAL (pre-child-instrumentation) expression to the operators
  -- for matching and mutant construction.
  --
  -- Why this ordering matters
  -- -------------------------
  -- Each operator produces one or more mutant alternatives.  Those mutants
  -- are embedded as the "mutated" branch of an ifMutation call:
  --
  --   ifMutation id <mutant> <fallthrough>
  --
  -- If mutant alternatives were built from already-instrumented children, any
  -- child that appears in multiple mutant alternatives would be DUPLICATED in
  -- the AST.  The ListLit operator is the worst case: given a 6-element list
  --
  --   [e1, e2, e3, e4, e5, e6]
  --
  -- it generates three mutants (empty, drop-first, drop-last), so the final
  -- expression contains the list four times total.  If the elements are
  -- already instrumented, each ei' is itself a nested ifMutation tree, and
  -- elements e2..e5 each appear in three of the four copies.  With N elements
  -- each carrying K layers of ifMutation wrapping, the AST grows as O(N * K).
  --
  -- This blowup compounds across operators: ConstBool fires on every
  -- Bool-typed expression, wrapping even individual list elements in two more
  -- ifMutation layers before ListLit sees them.  For Text.Colour.Chunk, which
  -- has `and [isNothing f1, ..., isNothing f6]` and
  -- `catMaybes [e1, ..., e5]`, the duplicate-subtree explosion previously
  -- caused GHC to exceed 16 GB of heap even at -O0.
  --
  -- The fix: operators receive the original `le` (children not yet
  -- instrumented) and produce mutants whose sub-expressions are plain,
  -- undecorated AST nodes.  The fallthrough branch — the "run normally" path
  -- through all ifMutation guards — is `le'` (children fully instrumented),
  -- so every nested mutation site is still reachable when that specific
  -- mutation is not active.  This is semantically correct: a mutation is a
  -- change at one specific site; when that mutation is active we execute the
  -- mutant directly without needing to recurse into it for other mutations.
  le' <- traverse (instrumentExpr (getLocA le)) le
  InstrumentEnv {instrumentEnvOperators} <- ask
  tryMutateWith instrumentEnvOperators le le'

instrumentExpr :: SrcSpan -> HsExpr GhcTc -> InstrM (HsExpr GhcTc)
instrumentExpr _sp = \case
  -- Track HsApp function-position depth so const-family operators can skip
  -- arity-\>=1 mutations at saturated sites.  The function side ('f') sees
  -- depth+1 (it is one more level of "head of an application" than its
  -- parent); the argument side ('a') resets to 0 (a is its own root
  -- expression).
  HsApp x f a ->
    HsApp x
      <$> local (\env -> env {instrumentEnvAppDepth = instrumentEnvAppDepth env + 1}) (instrumentLExpr f)
      <*> local (\env -> env {instrumentEnvAppDepth = 0}) (instrumentLExpr a)
  HsLam x lv mg -> HsLam x lv <$> instrumentMatchGroup mg
  HsCase x scrut mg -> HsCase x <$> instrumentLExpr scrut <*> instrumentMatchGroup mg
  HsIf x c t e -> HsIf x <$> instrumentLExpr c <*> instrumentLExpr t <*> instrumentLExpr e
  HsLet x binds body -> HsLet x <$> instrumentLocalBinds binds <*> instrumentLExpr body
  HsDo x ctx stmts -> HsDo x ctx <$> traverse (mapM instrumentStmt) stmts
  ExplicitList x es -> ExplicitList x <$> mapM instrumentLExpr es
  HsPar x e -> HsPar x <$> instrumentLExpr e
  NegApp x e se -> NegApp x <$> instrumentLExpr e <*> pure se
  OpApp x l op r -> OpApp x <$> instrumentLExpr l <*> pure op <*> instrumentLExpr r
  ExplicitTuple x args bx -> ExplicitTuple x <$> mapM instrumentTupArg args <*> pure bx
  RecordCon x con flds -> RecordCon x con <$> instrumentRecordBinds flds
  -- XExpr nodes appear after typechecking for operator expansion etc.
  -- We instrument the expanded expression (what the desugarer sees), except
  -- when 'instrumentEnvSkipThSplices' is set and the original (pre-expansion) node
  -- was a TH splice or quasi-quote — those expansions are macro-generated
  -- code whose source location points at the splice site, so any mutations
  -- recorded inside would be unhelpful (every covering test would have to
  -- exercise the splice's expanded form, and the diff in the report would
  -- not line up with the source file).
  XExpr (ExpandedThingTc orig expanded) -> do
    skipSplices <- asks instrumentEnvSkipThSplices
    if skipSplices && isSpliceThing orig
      then pure (XExpr (ExpandedThingTc orig expanded))
      else case origBindStmtBinders orig of
        -- The expansion of @do { p <- rhs; rest }@ is @(>>=) rhs (\\p -> rest)@.
        -- Walk the expanded expression manually so we can apply the
        -- 'DisableMutationsFor' scope only to @rhs@ (the first argument of
        -- @(>>=)@), not to the continuation lambda's body — that body
        -- contains further expanded BindStmts whose RHS expressions belong
        -- to other names entirely.
        Just binders -> XExpr . ExpandedThingTc orig <$> instrumentBindExpansion binders expanded
        Nothing -> do
          -- If the expansion originated from a source-level @OpApp@ (an infix
          -- operator application like @not a || b@), stash the original
          -- spans and operand text in 'instrumentEnvOpAppCtx' for the
          -- duration of the inner walk so const-family operators that fire
          -- on the bare operator can rewrite their preview to prefix form.
          ctx <- mkOpAppCtx orig
          let updateEnv env = case ctx of
                Just c -> env {instrumentEnvOpAppCtx = Just c}
                Nothing -> env
          XExpr . ExpandedThingTc orig <$> local updateEnv (instrumentExpr _sp expanded)
  XExpr (WrapExpr (HsWrap co e)) ->
    XExpr . WrapExpr . HsWrap co <$> instrumentExpr _sp e
  e -> pure e

-- | True when an 'HsThingRn' wraps the unexpanded form of a TH splice or
-- quasi-quote.  GHC stores the pre-expansion node in 'ExpandedThingTc' so we
-- can recognise these without falling back to source-line heuristics.
isSpliceThing :: HsThingRn -> Bool
isSpliceThing = \case
  OrigExpr e -> isSpliceHsExpr e
  _ -> False

-- | If the original (pre-expansion) thing was a 'BindStmt' in a @do@-block,
-- return its pattern's binders.  GHC 9.10+ expands @do@-blocks in the
-- renamer so the typechecked AST sees @(>>=) e (\\p -> rest)@ rather than
-- the original 'BindStmt'; the original is preserved here for diagnostics
-- and we reuse it to drive 'DisableMutationsFor' scoping.
origBindStmtBinders :: HsThingRn -> Maybe [Name]
origBindStmtBinders = \case
  OrigStmt (L _ (BindStmt _ pat _)) ->
    Just (collectPatBinders CollNoDictBinders pat)
  _ -> Nothing

-- | If @orig@ is an 'OpApp' originating from source-level infix syntax,
-- extract the outer, operator-token, and operand source spans plus the
-- operand source text.  Returns 'Nothing' for any non-'OpApp' original or
-- when a needed span is 'UnhelpfulSpan' or when the source file isn't
-- available (so we can't read operand text).
mkOpAppCtx :: HsThingRn -> InstrM (Maybe OpAppCtx)
mkOpAppCtx orig = case orig of
  OrigExpr (OpApp _ lLhs lOp lRhs)
    | RealSrcSpan lhsSp _ <- getLocA lLhs,
      RealSrcSpan opSp _ <- getLocA lOp,
      RealSrcSpan rhsSp _ <- getLocA lRhs -> do
        InstrumentEnv {instrumentEnvSourceFile} <- ask
        case instrumentEnvSourceFile of
          Just (_, ls) -> do
            let outerSp = combineRealSrcSpans lhsSp rhsSp
                lhsText = textInSpan ls lhsSp
                rhsText = textInSpan ls rhsSp
            pure $
              Just
                OpAppCtx
                  { opAppOuterSpan = outerSp,
                    opAppOpSpan = opSp,
                    opAppLhsText = lhsText,
                    opAppRhsText = rhsText
                  }
          Nothing -> pure Nothing
  _ -> pure Nothing

-- | Extract the source text covered by a single 'RealSrcSpan' from a list
-- of source lines.  Handles multi-line spans by concatenating intermediate
-- lines with a single space, which is enough for the prefix-form preview
-- (operands inside an @OpApp@ are typically single-line).
textInSpan :: [Text] -> RealSrcSpan -> Text
textInSpan allLines rss =
  let startLine = srcSpanStartLine rss
      endLine = srcSpanEndLine rss
      colS = srcSpanStartCol rss
      colE = srcSpanEndCol rss
      lineN i = case drop (i - 1) allLines of
        (l : _) -> l
        [] -> T.empty
   in if startLine == endLine
        then T.take (colE - colS) (T.drop (colS - 1) (lineN startLine))
        else
          let firstPart = T.drop (colS - 1) (lineN startLine)
              middleParts = map lineN [startLine + 1 .. endLine - 1]
              lastPart = T.take (colE - 1) (lineN endLine)
           in T.intercalate " " (firstPart : middleParts ++ [lastPart])

-- | Walk a typechecked @(>>=) rhs (\\pat -> rest)@ application, applying
-- 'DisableMutationsFor' scoping (drawn from @binders@) only to the @rhs@
-- argument.  The continuation lambda body is instrumented normally so its
-- own nested BindStmt expansions can fire.
--
-- Falls back to a normal walk if the expansion does not match the expected
-- shape (which would be a GHC change worth noticing).
instrumentBindExpansion :: [Name] -> HsExpr GhcTc -> InstrM (HsExpr GhcTc)
instrumentBindExpansion binders = \case
  HsApp x1 (L l1 (HsApp x2 bindOp rhs)) lam ->
    HsApp x1
      <$> ( do
              rhs' <- withLocalDisableManyNames binders (instrumentLExpr rhs)
              pure (L l1 (HsApp x2 bindOp rhs'))
          )
      <*> instrumentLExpr lam
  other -> instrumentExpr noSrcSpan other

-- | Variant of 'withLocalDisableMany' that takes 'Name's directly (we
-- already extracted them from a renamed pattern, not a typechecked one).
withLocalDisableManyNames :: [Name] -> InstrM a -> InstrM a
withLocalDisableManyNames names = applyLocalDisables (map getOccString names)

isSpliceHsExpr :: HsExpr GhcRn -> Bool
isSpliceHsExpr = \case
  HsUntypedSplice _ _ -> True
  HsTypedSplice _ _ -> True
  _ -> False

instrumentStmt :: ExprLStmt GhcTc -> InstrM (ExprLStmt GhcTc)
instrumentStmt = traverse $ \case
  LastStmt x e mb se -> LastStmt x <$> instrumentLExpr e <*> pure mb <*> pure se
  -- A 'BindStmt' binds names on the left of @<-@. If any of those names match
  -- a 'DisableMutationsFor' entry, narrow operators for the RHS expression
  -- the same way we do for @let foo = …@ bindings. The bound 'Id's come from
  -- the pattern via 'collectPatBinders' so tuple and constructor patterns
  -- like @(x, y) <- …@ work too.
  -- A 'BindStmt' binds names on the left of @<-@. If any of those names match
  -- a 'DisableMutationsFor' entry, narrow operators for the RHS expression
  -- the same way we do for @let foo = …@ bindings. The bound 'Id's come from
  -- the pattern via 'collectPatBinders' so tuple and constructor patterns
  -- like @(x, y) <- …@ work too.
  --
  -- NB: in GHC 9.10+ the renamer expands @do { p <- e; …rest }@ into
  -- @(>>=) e (\\p -> rest)@ before typechecking, so we usually never get
  -- here — the expanded form lands in 'instrumentExpr' as an
  -- @XExpr (ExpandedThingTc …)@ and is handled there.  This case still
  -- runs for list comprehensions, monad comprehensions, and any flavour
  -- where the typechecker preserves the original 'BindStmt' shape.
  BindStmt x p e -> BindStmt x p <$> withLocalDisableMany (collectPatBinders CollNoDictBinders p) (instrumentLExpr e)
  BodyStmt x e se1 se2 ->
    BodyStmt x
      <$> local (\env -> env {instrumentEnvInGuard = True}) (instrumentLExpr e)
      <*> pure se1
      <*> pure se2
  LetStmt x lbs -> LetStmt x <$> instrumentLocalBinds lbs
  s -> pure s

instrumentTupArg :: HsTupArg GhcTc -> InstrM (HsTupArg GhcTc)
instrumentTupArg = \case
  Present x e -> Present x <$> instrumentLExpr e
  Missing x -> pure (Missing x)

instrumentRecordBinds :: HsRecordBinds GhcTc -> InstrM (HsRecordBinds GhcTc)
instrumentRecordBinds = \case
  HsRecFields flds md ->
    HsRecFields
      <$> mapM (traverse (\(HsFieldBind x l e pun) -> HsFieldBind x l <$> instrumentLExpr e <*> pure pun)) flds
      <*> pure md

-- ---------------------------------------------------------------------------
-- Mutation candidates

-- | Try every operator in turn, accumulating ifMutation wrappers.
--
-- @origExpr@ has uninstrumented children; operators match against it and
-- build their mutant alternatives from it, so mutants stay cheap (see the
-- ordering-invariant note on 'instrumentLExpr').  @fallthroughExpr@ has fully
-- instrumented children and becomes the innermost "original" branch of the
-- ifMutation nesting.
tryMutateWith :: [MutationOperator] -> LHsExpr GhcTc -> LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
tryMutateWith operators origExpr fallthroughExpr =
  foldM (\ft op -> applyOperator origExpr ft op) fallthroughExpr operators

-- | Try one operator.  @origExpr@ is matched and used for mutant construction
-- (uninstrumented children); @fallthrough@ is used as the "original" branch
-- (instrumented children).  See the ordering-invariant note on
-- 'instrumentLExpr' for why this split is necessary.
applyOperator :: LHsExpr GhcTc -> LHsExpr GhcTc -> MutationOperator -> InstrM (LHsExpr GhcTc)
applyOperator origExpr fallthrough op = case operatorMatch op origExpr of
  Nothing -> pure fallthrough
  Just action -> do
    alts <- action
    case alts of
      -- The operator's action chose to produce no alternatives (e.g.
      -- RemoveAction declining to remove an action whose head is on the
      -- 'ignore' list).  This is a deliberate non-candidate, not a
      -- validation failure, so it must be silent.
      [] -> pure fallthrough
      _ -> do
        hscEnv <- liftTcM getTopEnv
        validated <- liftTcM $ filterM (liftIO . validateAlt hscEnv) alts
        case validated of
          [] -> do
            liftTcM $
              liftIO $
                putStrLn $
                  "mutation: WARNING all replacements dropped for operator "
                    ++ operatorName op
                    ++ locStr (getLocA origExpr)
            pure fallthrough
          (x : xs) -> applyAlts (operatorName op) (x :| xs) fallthrough

-- | Desugar a mutated expression and accept it only when desugaring is silent.
--
-- Pattern-match warnings are re-enabled on the validation 'HscEnv' so the
-- desugarer's coverage checker ('pmcMatches') runs.  The plugin's driver hook
-- disables 'Opt_WarnIncompletePatterns' and 'Opt_WarnIncompleteUniPatterns'
-- on the host 'DynFlags' (see 'Test.Syd.Mutation.Plugin.plugin') because the
-- @ifMutation@ wrapper confuses the checker for the surrounding module
-- compilation.  Validation, by contrast, desugars an isolated mutated
-- sub-expression with no wrapper around it, so the checker's verdict is
-- reliable here.
--
-- Auto-killing non-exhaustive cases: a 'RemoveCase' mutation that drops the
-- only alternative covering a constructor (e.g. the @_@ wildcard in
-- @case xs of [] -> ...; [_] -> ...; _ -> ...@) leaves the case
-- non-exhaustive.  GHC emits 'DsNonExhaustivePatterns' for such a mutated
-- expression; 'isEmptyMessages' returns 'False'; the mutation is dropped.
-- Any test built with @-Werror=incomplete-patterns@ (or that exercises the
-- runtime @MatchFail@) would kill the mutant anyway, so producing it would
-- just add noise to the manifest.
validateAlt :: HscEnv -> (Type, LHsExpr GhcTc, String, String, SrcSpanDelta) -> IO Bool
validateAlt hscEnv (_, mutated, _, _, _) = do
  let dflags' =
        foldl
          wopt_set
          (hsc_dflags hscEnv)
          [ Opt_WarnIncompletePatterns,
            Opt_WarnIncompleteUniPatterns,
            Opt_WarnOverlappingPatterns
          ]
      hscEnv' = hscEnv {hsc_dflags = dflags'}
  (msgs, mcore) <- deSugarExpr hscEnv' mutated
  pure (isEmptyMessages msgs && isJust mcore)

locStr :: SrcSpan -> String
locStr = \case
  RealSrcSpan rss _ ->
    " at " ++ show (srcSpanStartLine rss) ++ ":" ++ show (srcSpanStartCol rss)
  UnhelpfulSpan _ -> ""

-- | True when the outer span fully contains the inner span (inclusive on
-- both ends).  Compares by (line, col) ordering so multi-line splices are
-- handled correctly.
containsSpan :: RealSrcSpan -> RealSrcSpan -> Bool
containsSpan outer inner =
  startsAfter && endsBefore
  where
    outerStart = (srcSpanStartLine outer, srcSpanStartCol outer)
    outerEnd = (srcSpanEndLine outer, srcSpanEndCol outer)
    innerStart = (srcSpanStartLine inner, srcSpanStartCol inner)
    innerEnd = (srcSpanEndLine inner, srcSpanEndCol inner)
    startsAfter = outerStart <= innerStart
    endsBefore = innerEnd <= outerEnd

-- Nest alternatives as: ifMutation id1 mut1 (ifMutation id2 mut2 original)
--
-- The 1-based @altIndex@ disambiguates alternatives that an operator emits
-- with identical @replStr@ text: e.g. ListLit's drop-first and drop-last on a
-- 3-element list both have replStr "2 elements", and without an index the
-- 'MutationId's would collide.  The index is appended as the last component
-- of the id.
applyAlts :: String -> NonEmpty (Type, LHsExpr GhcTc, String, String, SrcSpanDelta) -> LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
applyAlts opName alts original = do
  (wrapped, records) <- go 1 alts
  -- Every applyAlts call emits exactly one mutation group, even if a
  -- specific alternative was filtered out (TH splice / UnhelpfulSpan) and
  -- produced no record.  An empty group is harmless: no scheduling work, no
  -- report entries.
  tell [MutationGroup records]
  pure wrapped
  where
    go altIndex ((ty, mutated, origStr, replStr, delta) :| rest) = do
      (mid, mRec) <- recordMutation original opName origStr replStr delta altIndex
      (innerExpr, innerRecs) <- case rest of
        [] -> pure (original, [])
        (a : as) -> go (altIndex + 1) (a :| as)
      outer <- wrapWithIfMutation ty mid mutated innerExpr
      let recs = maybe innerRecs (: innerRecs) mRec
      pure (outer, recs)

-- | Record one mutation site, returning its 'MutationId' and (when the
-- mutation is kept) the corresponding 'MutationRecord'.  Returns
-- @(MutationId [], Nothing)@ when the mutation is filtered out (TH splice or
-- 'UnhelpfulSpan'); callers should treat the empty 'MutationId' as inert.
recordMutation ::
  LHsExpr GhcTc ->
  String ->
  String ->
  String ->
  SrcSpanDelta ->
  -- | 1-based index of this alternative within the operator's match.
  -- Appended to the id so alternatives with identical @replStr@ text do not
  -- collide.
  Int ->
  InstrM (MutationId, Maybe MutationRecord)
recordMutation le op origStr replStr delta altIndex = do
  InstrumentEnv {instrumentEnvModule, instrumentEnvSourceFile, instrumentEnvSkipThSplices, instrumentEnvSpliceSpans} <- ask
  let sp = getLocA le
  case sp of
    RealSrcSpan rss _
      | instrumentEnvSkipThSplices && any (`containsSpan` rss) instrumentEnvSpliceSpans ->
          -- Mutation is inside a TH splice or quasi-quote; drop it.
          pure (MutationId [], Nothing)
    RealSrcSpan rss _ -> do
      let mn = moduleNameString (moduleName instrumentEnvModule)
          lineNum = srcSpanStartLine rss
          colStart = srcSpanStartCol rss
          colEnd = srcSpanEndCol rss
          mid =
            MutationId
              [ mn,
                op,
                show lineNum,
                show colStart,
                show colEnd,
                replStr,
                show altIndex
              ]
          lineNumEnd = srcSpanEndLine rss
          -- The diff (source_lines / mutated_lines / context) covers the
          -- "display span": the matched expression's span normally, but
          -- widened to the outer span when the operator emitted a
          -- 'ReplaceOuterSpan'.  Keeping the diff aligned on the same span
          -- on both sides means it always parses as a single contiguous
          -- edit, never as "narrow original text vs wider mutated text".
          (displayStartLine, displayEndLine, displayColS, displayColE) =
            case delta of
              ReplaceOuterSpan outerRss _ ->
                ( srcSpanStartLine outerRss,
                  srcSpanEndLine outerRss,
                  srcSpanStartCol outerRss,
                  srcSpanEndCol outerRss
                )
              _ -> (lineNum, lineNumEnd, colStart, colEnd)
          (mSrcFile, ctxBefore, ctxAfter, spanLines, mutatedLines) =
            case instrumentEnvSourceFile of
              Nothing -> (Nothing, [], [], [], [])
              Just (relFile, ls) ->
                let startIdx = displayStartLine - 1
                    endIdx = displayEndLine - 1
                    before = reverse $ take 3 $ reverse $ take startIdx ls
                    after = take 3 $ drop (endIdx + 1) ls
                    srcSpanLines = [line | (i, line) <- zip [0 :: Int ..] ls, i >= startIdx, i <= endIdx]
                    mutLines = applyDelta ls displayStartLine displayEndLine displayColS displayColE delta srcSpanLines
                 in (Just relFile, before, after, srcSpanLines, mutLines)
      let record =
            MutationRecord
              { mutRecId = mid,
                mutRecOperator = T.pack op,
                mutRecOriginal = T.pack origStr,
                mutRecReplacement = T.pack replStr,
                mutRecModule = T.pack mn,
                mutRecLine = fromIntegral lineNum,
                mutRecEndLine = fromIntegral lineNumEnd,
                mutRecColStart = fromIntegral colStart,
                mutRecColEnd = fromIntegral colEnd,
                mutRecSourceFile = mSrcFile,
                mutRecSourceLines = spanLines,
                mutRecMutatedLines = mutatedLines,
                mutRecContextBefore = ctxBefore,
                mutRecContextAfter = ctxAfter,
                mutRecCoveringTests = Nothing
              }
      liftTcM $
        liftIO $ do
          let MutationId parts = mutRecId record
          case parts of
            (modName : _op : lineStr : colStartStr : colEndStr : _) ->
              let filePath = case mutRecSourceFile record of
                    Just p -> fromRelFile p
                    Nothing -> map (\c -> if c == '.' then '/' else c) modName ++ ".hs"
                  variantSuffix = case parts of
                    [_, _, _, _, _, _, altIdx] -> " #" ++ altIdx
                    _ -> ""
               in putStrLn $ "added mutation " ++ T.unpack (mutRecOperator record) ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr ++ variantSuffix
            _ -> putStrLn $ "added mutation " ++ show parts
      pure (mid, Just record)
    UnhelpfulSpan _ -> pure (MutationId [], Nothing)

-- | Apply a 'SrcSpanDelta' to compute the mutated lines.
applyDelta :: [Text] -> Int -> Int -> Int -> Int -> SrcSpanDelta -> [Text] -> [Text]
applyDelta allLines outerStart outerEnd colS colE delta spanLines = case delta of
  TokenReplace newText -> applyTokenReplace colS colE newText spanLines
  TokenReplaceAt subSpan newText ->
    applyTokenReplaceAt outerStart subSpan newText spanLines
  SpanRemoval rmSpans -> applySpanRemoval allLines outerStart outerEnd rmSpans
  PrependText prefix -> applyPrependText colS prefix spanLines
  WrapWithText prefix suffix -> applyWrapWithText colS colE prefix suffix spanLines
  -- 'ReplaceOuterSpan' has already widened the display span at the
  -- 'recordMutation' level, so by the time we are here the @colS@..@colE@
  -- bounds already cover the outer span and 'applyTokenReplace' does the
  -- right thing.
  ReplaceOuterSpan _ newText -> applyTokenReplace colS colE newText spanLines

-- | Wrap the matched span text with @prefix@ before and @suffix@ after.
-- For multi-line spans only the prefix lands on the first line and the
-- suffix on the last line; everything in between is unchanged.
applyWrapWithText :: Int -> Int -> Text -> Text -> [Text] -> [Text]
applyWrapWithText _ _ _ _ [] = []
applyWrapWithText colS colE prefix suffix [line] =
  let before = T.take (colS - 1) line
      middle = T.drop (colS - 1) (T.take (colE - 1) line)
      after = T.drop (colE - 1) line
   in [before <> prefix <> middle <> suffix <> after]
applyWrapWithText colS colE prefix suffix (firstLine : rest) =
  let beforeF = T.take (colS - 1) firstLine
      restOfF = T.drop (colS - 1) firstLine
      firstLine' = beforeF <> prefix <> restOfF
      (middle, lastLine) = case reverse rest of
        l : ms -> (reverse ms, l)
        [] -> ([], T.empty)
      beforeL = T.take (colE - 1) lastLine
      afterL = T.drop (colE - 1) lastLine
      lastLine' = beforeL <> suffix <> afterL
   in firstLine' : middle ++ [lastLine']

-- | Replace text at the columns covered by @subSpan@, relative to the outer
-- expression's span (whose first line is @outerStart@). Only single-line
-- sub-spans are handled — multi-line operator tokens don't occur in practice.
applyTokenReplaceAt :: Int -> RealSrcSpan -> Text -> [Text] -> [Text]
applyTokenReplaceAt outerStart subSpan newText spanLines =
  let subLine = srcSpanStartLine subSpan
      idx = subLine - outerStart
   in case splitAt idx spanLines of
        (before, line : after) ->
          let line' = applySingleLineReplace (srcSpanStartCol subSpan) (srcSpanEndCol subSpan) newText line
           in before ++ line' : after
        _ -> spanLines

applySingleLineReplace :: Int -> Int -> Text -> Text -> Text
applySingleLineReplace colS colE newText line =
  T.take (colS - 1) line <> newText <> T.drop (colE - 1) line

-- | Replace text at columns colS..colE on the first line of the span.
-- GHC colEnd is exclusive (one past the last character), so T.drop (colE-1) is correct.
--
-- Multi-line spans collapse to a single line: the prefix of the first line
-- (columns 1..colS-1), then the replacement text, then the suffix of the
-- last line (columns colE.. onwards).  Intermediate lines are dropped.  This
-- matters for operators that replace whole multi-line expressions with a
-- short token — e.g. @FunctionToEmptyList@ on @execWriter $ do { ... }@.
applyTokenReplace :: Int -> Int -> Text -> [Text] -> [Text]
applyTokenReplace colS colE newText lns = case NE.nonEmpty lns of
  Nothing -> []
  Just ne ->
    let before = T.take (colS - 1) (NE.head ne)
        after = T.drop (colE - 1) (NE.last ne)
     in [before <> newText <> after]

-- | Remove lines belonging to any of the given spans from the outer span's line range.
--
-- Lines outside the range @[1 .. length allLines]@ are silently dropped: this
-- happens when GHC source spans refer to a preprocessor-generated source
-- (e.g. via @-pgmF sydtest-discover@) while @allLines@ is the original
-- on-disk @.hs@ file, which is shorter.
applySpanRemoval :: [Text] -> Int -> Int -> [RealSrcSpan] -> [Text]
applySpanRemoval allLines outerStart outerEnd rmSpans =
  let removed = Set.fromList [l | rss <- rmSpans, l <- [srcSpanStartLine rss .. srcSpanEndLine rss]]
   in [ line
      | (i, line) <- zip [1 ..] allLines,
        i >= outerStart,
        i <= outerEnd,
        Set.notMember i removed
      ]

-- | Prepend text at the column position of the start of the span.
applyPrependText :: Int -> Text -> [Text] -> [Text]
applyPrependText _ _ [] = []
applyPrependText colS prefix (line : rest) =
  let (before, after) = T.splitAt (colS - 1) line
   in (before <> prefix <> after) : rest

-- ---------------------------------------------------------------------------
-- Building the ifMutation call

-- | Wrap as:  ifMutation @ty mutId mutated original
--
-- > ifMutation :: forall a. MutationId -> a -> a -> a
wrapWithIfMutation ::
  Type ->
  MutationId ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  InstrM (LHsExpr GhcTc)
wrapWithIfMutation ty mid mutatedExpr origExpr = do
  InstrumentEnv {instrumentEnvIfMutationId, instrumentEnvMutationIdCon} <- ask
  midExpr <- liftTcM $ buildMutationIdExpr instrumentEnvMutationIdCon mid
  let ifMutVar = nlHsTyApp instrumentEnvIfMutationId [ty]
      call = mkHsApp (mkHsApp (mkHsApp ifMutVar midExpr) mutatedExpr) origExpr
  pure call

-- | Build a 'MutationId' expression from a list of strings.
--
-- > MutationId ["module", "op", ...]
buildMutationIdExpr :: DataCon -> MutationId -> TcM (LHsExpr GhcTc)
buildMutationIdExpr con (MutationId parts) = do
  let strExprs = map mkHsStringExpr parts
      listExpr = noLocA (ExplicitList (mkListTy charTy) strExprs)
      conExpr = nlHsDataCon con
  pure (mkHsApp conExpr listExpr)

-- | Build a string literal expression of type 'String' (= '[Char]').
mkHsStringExpr :: String -> LHsExpr GhcTc
mkHsStringExpr s =
  noLocA (HsLit NoExtField (HsString NoSourceText (mkFastString s)))
