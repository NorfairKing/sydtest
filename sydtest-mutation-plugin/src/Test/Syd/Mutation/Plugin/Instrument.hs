{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Test.Syd.Mutation.Plugin.Instrument
  ( MutationRecord (..),
    MutationOperator (..),
    InstrumentEnv (..),
    InstrM,
    liftTcM,
    runInstrument,
    instrumentModule,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (filterM, foldM)
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import qualified Data.ByteString as SB
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC
import GHC.Builtin.Types (charTy, mkListTy)
import GHC.Data.Bag (mapBagM)
import GHC.Data.FastString (mkFastString)
import GHC.HsToCore (deSugarExpr)
import GHC.Serialized (deserializeWithData)
import GHC.Tc.Types
import GHC.Tc.Utils.Env (tcLookupDataCon, tcLookupId)
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Types.Annotations (AnnEnv, AnnTarget (..), findAnns)
import GHC.Types.Error (isEmptyMessages)
import GHC.Types.Id (idName)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkDataOcc, mkVarOcc)
import GHC.Types.Name.Reader (GlobalRdrEnv, greName)
import GHC.Types.SourceText (SourceText (NoSourceText))
import Path
import Path.IO (resolveFile')
import Test.Syd.Mutation.Manifest (MutationAddedEvent (..), MutationRecord (..), renderMutationAddedEvent)
import Test.Syd.Mutation.Runtime (MutationId (..))

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
    -- The action returns a list of @(ty, mutated, originalStr, replacementStr, srcTransform)@
    -- tuples, one per distinct mutation to generate at this site.  Each gets its own
    -- 'MutationId' and is wrapped independently as a nested 'ifMutation' call.
    -- 'tryMutateWith' will validate each alternative via desugaring and silently drop
    -- any that produce diagnostics (e.g. overflowed literals).
    -- 'srcTransform' maps the original source span text to the replacement source text
    -- for display in the diff; use @const (T.pack replStr)@ for simple token swaps.
    operatorMatch :: LHsExpr GhcTc -> Maybe (InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)])
  }

-- ---------------------------------------------------------------------------
-- Monad

data InstrumentEnv = InstrumentEnv
  { instrModule :: Module,
    -- | The module's 'GlobalRdrEnv', used by operators to look up replacement ids.
    instrRdrEnv :: GlobalRdrEnv,
    -- | Id for Test.Syd.Mutation.Runtime.ifMutation, looked up once per module.
    instrIfMutationId :: Id,
    -- | DataCon for Test.Syd.Mutation.Runtime.MutationId, looked up once per module.
    instrMutationIdCon :: DataCon,
    -- | Operators to try at each expression site.
    instrOperators :: [MutationOperator],
    -- | Annotation environment for reading {-# ANN #-} annotations.
    instrAnnEnv :: AnnEnv,
    -- | Mutation type names disabled at module or global scope.
    instrDisabledMutations :: [String],
    -- | Source file (relative path) and pre-read lines, read once per module.
    instrSourceFile :: Maybe (Path Rel File, [T.Text]),
    -- | Print each mutation site as it is recorded (enabled by --debug plugin opt).
    instrDebug :: Bool,
    -- | True when instrumenting a guard expression (BodyStmt inside a GRHS).
    -- Used by ConstBool to suppress the e->False alternative, which would make
    -- the guard non-exhaustive and throw an exception that tests can't catch.
    instrInGuard :: Bool
  }

type InstrM = WriterT [MutationRecord] (ReaderT InstrumentEnv TcM)

liftTcM :: TcM a -> InstrM a
liftTcM = lift . lift

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
  InstrM a ->
  TcM (a, [MutationRecord])
runInstrument tcGblEnv operators annEnv disabledMutations mSrcPath debug action = do
  let rdrEnv = tcg_rdr_env tcGblEnv
      modul = tcg_mod tcGblEnv
  ifMutId <- lookupRdrEnvId rdrEnv "ifMutation"
  mutIdCon <- lookupRdrEnvDataCon rdrEnv "MutationId"
  mSrcFile <- liftIO $ case mSrcPath >>= parseRelFile of
    Nothing -> pure Nothing
    Just relFile -> do
      absFile <- resolveFile' (fromRelFile relFile)
      mbs <- (Just <$> SB.readFile (fromAbsFile absFile)) `catch` ioErr
      pure $ fmap (\bs -> (relFile, T.lines (TE.decodeUtf8Lenient bs))) mbs
  let activeOperators = filter (\op -> operatorName op `notElem` disabledMutations) operators
  runReaderT
    (runWriterT action)
    InstrumentEnv
      { instrModule = modul,
        instrRdrEnv = rdrEnv,
        instrIfMutationId = ifMutId,
        instrMutationIdCon = mutIdCon,
        instrOperators = activeOperators,
        instrAnnEnv = annEnv,
        instrDisabledMutations = disabledMutations,
        instrSourceFile = mSrcFile,
        instrDebug = debug,
        instrInGuard = False
      }
  where
    ioErr :: IOException -> IO (Maybe SB.ByteString)
    ioErr _ = pure Nothing

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
  FunBind x name mg -> do
    mg' <- withFunBindEnv (idName (unLoc name)) (instrumentMatchGroup mg)
    pure (FunBind x name mg')
  PatBind x pat mult rhs -> PatBind x pat mult <$> instrumentGRHSs rhs
  VarBind x var rhs -> VarBind x var <$> instrumentLExpr rhs
  PatSynBind x psb -> pure (PatSynBind x psb)
  -- At GhcTc, XHsBindsLR carries an AbsBinds (see XXHsBindsLR instance).
  -- {-# ANN f #-} annotations attach to the poly Id (abe_poly), but the inner
  -- FunBinds carry mono Ids.  Apply poly-name annotations here so they cover
  -- the inner binds, which withFunBindEnv would otherwise miss.
  XHsBindsLR ab@AbsBinds {abs_binds, abs_exports} -> do
    let polyNames = map (idName . abe_poly) abs_exports
    binds' <- foldr withFunBindEnv (mapBagM (traverse instrumentBind) abs_binds) polyNames
    pure (XHsBindsLR ab {abs_binds = binds'})

-- | Run an instrumentation action with the operator list filtered by any
-- {-# ANN funName ("disable-mutation:..." :: String) #-} annotations on the
-- given top-level name.
withFunBindEnv :: Name -> InstrM a -> InstrM a
withFunBindEnv funName action = do
  InstrumentEnv {instrAnnEnv, instrOperators} <- ask
  let funAnns = findAnns deserializeWithData instrAnnEnv (NamedTarget funName) :: [String]
  case parseFunMutationAnns funAnns of
    Just [] -> action -- no annotation affecting this function
    Just disabled ->
      local (\env -> env {instrOperators = filter (\op -> operatorName op `notElem` disabled) instrOperators}) action
    Nothing -> local (\env -> env {instrOperators = []}) action -- disable all
  where
    -- Returns Nothing for "disable all", Just names for specific disables,
    -- Just [] when no relevant annotation is present.
    parseFunMutationAnns :: [String] -> Maybe [String]
    parseFunMutationAnns anns =
      let parsed = concatMap parseOne anns
       in if any isDisableAll parsed
            then Nothing
            else Just (concatMap getNames parsed)
    parseOne s
      | s == "DisableMutations" = [Left ()]
      | Just rest <- stripPrefix "DisableMutations:" s = [Right (map trim (splitOnComma rest))]
      | Just rest <- stripPrefix "DisableMutation:" s = [Right [trim rest]]
      | otherwise = []
    trim = dropWhile (== ' ')
    isDisableAll (Left ()) = True
    isDisableAll _ = False
    getNames (Right ns) = ns
    getNames _ = []

splitOnComma :: String -> [String]
splitOnComma s = case break (== ',') s of
  (w, []) -> [w]
  (w, _ : rest) -> w : splitOnComma rest

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
  HsValBinds x valBinds -> HsValBinds x <$> instrumentValBinds valBinds
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
  InstrumentEnv {instrOperators} <- ask
  tryMutateWith instrOperators le le'

instrumentExpr :: SrcSpan -> HsExpr GhcTc -> InstrM (HsExpr GhcTc)
instrumentExpr _sp = \case
  HsApp x f a -> HsApp x <$> instrumentLExpr f <*> instrumentLExpr a
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
  -- We instrument the expanded expression (what the desugarer sees).
  XExpr (ExpandedThingTc orig expanded) ->
    XExpr . ExpandedThingTc orig <$> instrumentExpr _sp expanded
  XExpr (WrapExpr (HsWrap co e)) ->
    XExpr . WrapExpr . HsWrap co <$> instrumentExpr _sp e
  e -> pure e

instrumentStmt :: ExprLStmt GhcTc -> InstrM (ExprLStmt GhcTc)
instrumentStmt = traverse $ \case
  LastStmt x e mb se -> LastStmt x <$> instrumentLExpr e <*> pure mb <*> pure se
  BindStmt x p e -> BindStmt x p <$> instrumentLExpr e
  BodyStmt x e se1 se2 ->
    BodyStmt x
      <$> local (\env -> env {instrInGuard = True}) (instrumentLExpr e)
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

validateAlt :: HscEnv -> (Type, LHsExpr GhcTc, String, String, T.Text -> T.Text) -> IO Bool
validateAlt hscEnv (_, mutated, _, _, _) = do
  (msgs, mcore) <- deSugarExpr hscEnv mutated
  pure (isEmptyMessages msgs && isJust mcore)

locStr :: SrcSpan -> String
locStr = \case
  RealSrcSpan rss _ ->
    " at " ++ show (srcSpanStartLine rss) ++ ":" ++ show (srcSpanStartCol rss)
  UnhelpfulSpan _ -> ""

-- Nest alternatives as: ifMutation id1 mut1 (ifMutation id2 mut2 original)
applyAlts :: String -> NonEmpty (Type, LHsExpr GhcTc, String, String, T.Text -> T.Text) -> LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
applyAlts opName ((ty, mutated, origStr, replStr, srcTransform) :| rest) original = do
  mid <- recordMutation original opName origStr replStr srcTransform
  inner <- case rest of
    [] -> pure original
    (a : as) -> applyAlts opName (a :| as) original
  wrapWithIfMutation ty mid mutated inner

-- | Record one mutation site and return its 'MutationId'.
recordMutation ::
  LHsExpr GhcTc ->
  String ->
  String ->
  String ->
  (T.Text -> T.Text) ->
  InstrM MutationId
recordMutation le op origStr replStr srcTransform = do
  InstrumentEnv {instrModule, instrSourceFile} <- ask
  let sp = getLocA le
  case sp of
    RealSrcSpan rss _ -> do
      let mn = moduleNameString (moduleName instrModule)
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
                replStr
              ]
          lineNumEnd = srcSpanEndLine rss
          (mSrcFile, srcLine, mutatedLine, ctxBefore, ctxAfter) =
            case instrSourceFile of
              Nothing -> (Nothing, Nothing, Nothing, [], [])
              Just (relFile, ls) ->
                let startIdx = lineNum - 1
                    endIdx = lineNumEnd - 1
                    before = reverse $ take 3 $ reverse $ take startIdx ls
                    after = take 3 $ drop (endIdx + 1) ls
                    -- Collect all source lines covered by the span.
                    spanLines =
                      [ ls !! i
                      | i <- [startIdx .. endIdx],
                        i >= 0,
                        i < length ls
                      ]
                    mLines = case spanLines of
                      [] -> Nothing
                      _ -> Just spanLines
                    mMutated = fmap (mutateSpan colStart colEnd srcTransform) mLines
                 in (Just relFile, T.intercalate (T.pack "\n") <$> mLines, T.intercalate (T.pack "\n") <$> mMutated, before, after)
      let record =
            MutationRecord
              { mutRecId = mid,
                mutRecOperator = T.pack op,
                mutRecOriginal = T.pack origStr,
                mutRecReplacement = T.pack replStr,
                mutRecModule = T.pack mn,
                mutRecLine = fromIntegral lineNum,
                mutRecColStart = fromIntegral colStart,
                mutRecColEnd = fromIntegral colEnd,
                mutRecSourceFile = mSrcFile,
                mutRecSourceLine = srcLine,
                mutRecMutatedLine = mutatedLine,
                mutRecContextBefore = ctxBefore,
                mutRecContextAfter = ctxAfter,
                mutRecCoveringTests = Nothing
              }
      liftTcM $
        liftIO $
          putStr $
            renderMutationAddedEvent MutationAddedEvent {mutationAddedRecord = record}
      tell [record]
      pure mid
    UnhelpfulSpan _ -> pure (MutationId [])

-- | Apply a source transformation to a multi-line span.
-- @colStart@ and @colEnd@ are 1-based columns on the first and last lines
-- respectively.  The original span text (extracted across all lines) is passed
-- to @srcTransform@; the result replaces the span in the source.
mutateSpan :: Int -> Int -> (T.Text -> T.Text) -> [T.Text] -> [T.Text]
mutateSpan colStart colEnd srcTransform ls = case ls of
  [] -> []
  [line] ->
    -- Single-line span: splice within one line.
    let origSpan = T.take (colEnd - colStart) (T.drop (colStart - 1) line)
        replSource = srcTransform origSpan
     in [T.take (colStart - 1) line <> replSource <> T.drop (colEnd - 1) line]
  (firstLine : rest) ->
    -- Multi-line span: extract from colStart on first line to colEnd on last line.
    let lastLine = last rest
        midLines = init rest
        nl = T.pack "\n"
        origSpan =
          T.intercalate nl $
            T.drop (colStart - 1) firstLine
              : midLines
              ++ [T.take (colEnd - 1) lastLine]
        replSource = srcTransform origSpan
        -- Replace the span: prefix of first line + replacement + suffix of last line.
        prefix = T.take (colStart - 1) firstLine
        suffix = T.drop (colEnd - 1) lastLine
        -- Split the replacement on newlines to produce multiple output lines.
        replLines = T.splitOn nl replSource
     in case replLines of
          [] -> [prefix <> suffix]
          [single] -> [prefix <> single <> suffix]
          (rFirst : rRest) ->
            (prefix <> rFirst) : init rRest ++ [last rRest <> suffix]

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
  InstrumentEnv {instrIfMutationId, instrMutationIdCon} <- ask
  midExpr <- liftTcM $ buildMutationIdExpr instrMutationIdCon mid
  let ifMutVar = nlHsTyApp instrIfMutationId [ty]
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
