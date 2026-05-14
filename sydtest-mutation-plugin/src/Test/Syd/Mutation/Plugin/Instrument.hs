{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Test.Syd.Mutation.Plugin.Instrument
  ( MutationRecord (..),
    MutationOperator (..),
    SrcSpanDelta (..),
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
import qualified Data.Set as Set
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
import Test.Syd.Mutation.Manifest (MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))

-- ---------------------------------------------------------------------------
-- Source delta

-- | Describes the source-level change a mutation makes, for display purposes.
data SrcSpanDelta
  = -- | Replace the matched expression's span text with this exact text.
    TokenReplace T.Text
  | -- | Replace the text at a specific sub-span (within the matched
    -- expression) with this exact text. Used for operator-token swaps
    -- (e.g. @+@ → @-@), where the matched expression covers the whole
    -- operator application but the textual change is only the operator
    -- token itself.
    TokenReplaceAt RealSrcSpan T.Text
  | -- | Remove these source line ranges from within the outer expression's span.
    SpanRemoval [RealSrcSpan]
  | -- | Prepend this text at the start of the matched expression's span.
    PrependText T.Text
  | -- | Wrap the matched expression's span text with a prefix and suffix.
    -- Useful when the prefix alone would re-parse with the wrong precedence
    -- (e.g. @not n < 0@ parses as @(not n) < 0@, so we want @not (n < 0)@).
    WrapWithText T.Text T.Text

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

validateAlt :: HscEnv -> (Type, LHsExpr GhcTc, String, String, SrcSpanDelta) -> IO Bool
validateAlt hscEnv (_, mutated, _, _, _) = do
  (msgs, mcore) <- deSugarExpr hscEnv mutated
  pure (isEmptyMessages msgs && isJust mcore)

locStr :: SrcSpan -> String
locStr = \case
  RealSrcSpan rss _ ->
    " at " ++ show (srcSpanStartLine rss) ++ ":" ++ show (srcSpanStartCol rss)
  UnhelpfulSpan _ -> ""

-- Nest alternatives as: ifMutation id1 mut1 (ifMutation id2 mut2 original)
applyAlts :: String -> NonEmpty (Type, LHsExpr GhcTc, String, String, SrcSpanDelta) -> LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
applyAlts opName ((ty, mutated, origStr, replStr, delta) :| rest) original = do
  mid <- recordMutation original opName origStr replStr delta
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
  SrcSpanDelta ->
  InstrM MutationId
recordMutation le op origStr replStr delta = do
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
          (mSrcFile, ctxBefore, ctxAfter, spanLines, mutatedLines) =
            case instrSourceFile of
              Nothing -> (Nothing, [], [], [], [])
              Just (relFile, ls) ->
                let startIdx = lineNum - 1
                    endIdx = lineNumEnd - 1
                    before = reverse $ take 3 $ reverse $ take startIdx ls
                    after = take 3 $ drop (endIdx + 1) ls
                    srcSpanLines = [ls !! i | i <- [startIdx .. endIdx], i >= 0, i < length ls]
                    mutLines = applyDelta ls lineNum lineNumEnd colStart colEnd delta srcSpanLines
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
               in putStrLn $ "added mutation " ++ T.unpack (mutRecOperator record) ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr
            _ -> putStrLn $ "added mutation " ++ show parts
      tell [record]
      pure mid
    UnhelpfulSpan _ -> pure (MutationId [])

-- | Apply a 'SrcSpanDelta' to compute the mutated lines.
applyDelta :: [T.Text] -> Int -> Int -> Int -> Int -> SrcSpanDelta -> [T.Text] -> [T.Text]
applyDelta allLines outerStart outerEnd colS colE delta spanLines = case delta of
  TokenReplace newText -> applyTokenReplace colS colE newText spanLines
  TokenReplaceAt subSpan newText ->
    applyTokenReplaceAt outerStart subSpan newText spanLines
  SpanRemoval rmSpans -> applySpanRemoval allLines outerStart outerEnd rmSpans
  PrependText prefix -> applyPrependText colS prefix spanLines
  WrapWithText prefix suffix -> applyWrapWithText colS colE prefix suffix spanLines

-- | Wrap the matched span text with @prefix@ before and @suffix@ after.
-- For multi-line spans only the prefix lands on the first line and the
-- suffix on the last line; everything in between is unchanged.
applyWrapWithText :: Int -> Int -> T.Text -> T.Text -> [T.Text] -> [T.Text]
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
applyTokenReplaceAt :: Int -> RealSrcSpan -> T.Text -> [T.Text] -> [T.Text]
applyTokenReplaceAt outerStart subSpan newText spanLines =
  let subLine = srcSpanStartLine subSpan
      idx = subLine - outerStart
   in case splitAt idx spanLines of
        (before, line : after) ->
          let line' = applySingleLineReplace (srcSpanStartCol subSpan) (srcSpanEndCol subSpan) newText line
           in before ++ line' : after
        _ -> spanLines

applySingleLineReplace :: Int -> Int -> T.Text -> T.Text -> T.Text
applySingleLineReplace colS colE newText line =
  T.take (colS - 1) line <> newText <> T.drop (colE - 1) line

-- | Replace text at columns colS..colE on the first line of the span.
-- GHC colEnd is exclusive (one past the last character), so T.drop (colE-1) is correct.
applyTokenReplace :: Int -> Int -> T.Text -> [T.Text] -> [T.Text]
applyTokenReplace _ _ _ [] = []
applyTokenReplace colS colE newText (line : rest) =
  let before = T.take (colS - 1) line
      after = T.drop (colE - 1) line
   in (before <> newText <> after) : rest

-- | Remove lines belonging to any of the given spans from the outer span's line range.
applySpanRemoval :: [T.Text] -> Int -> Int -> [RealSrcSpan] -> [T.Text]
applySpanRemoval allLines outerStart outerEnd rmSpans =
  let removed = Set.fromList [l | rss <- rmSpans, l <- [srcSpanStartLine rss .. srcSpanEndLine rss]]
   in [allLines !! (i - 1) | i <- [outerStart .. outerEnd], Set.notMember i removed]

-- | Prepend text at the column position of the start of the span.
applyPrependText :: Int -> T.Text -> [T.Text] -> [T.Text]
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
