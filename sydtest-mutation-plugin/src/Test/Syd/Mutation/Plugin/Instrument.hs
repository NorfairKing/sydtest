{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Test.Syd.Mutation.Plugin.Instrument
  ( MutationRecord (..),
    MutationOperator (..),
    runInstrument,
    instrumentModule,
  )
where

import Control.Monad (foldM)
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import GHC
import GHC.Builtin.Types (charTy, mkListTy)
import GHC.Data.Bag (mapBagM)
import GHC.Data.FastString (mkFastString)
import GHC.Tc.Types
import GHC.Tc.Utils.Env (tcLookupDataCon, tcLookupId)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkDataOcc, mkVarOcc)
import GHC.Types.Name.Reader (GlobalRdrEnv, greName)
import GHC.Types.SourceText (SourceText (NoSourceText))
import Test.Syd.Mutation.Manifest (MutationRecord (..))
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
    -- | @Just (ty, mutated, originalStr, replacementStr)@ or @Nothing@.
    -- The operator supplies the 'Type' because it already has it at hand when
    -- it matches the expression shape; this avoids a separate type-extraction
    -- step in the instrumenter.
    operatorMatch :: LHsExpr GhcTc -> Maybe (Type, LHsExpr GhcTc, String, String)
  }

-- ---------------------------------------------------------------------------
-- Monad

data InstrumentEnv = InstrumentEnv
  { instrModule :: Module,
    -- | Id for Test.Syd.Mutation.Runtime.ifMutation, looked up once per module.
    instrIfMutationId :: Id,
    -- | DataCon for Test.Syd.Mutation.Runtime.MutationId, looked up once per module.
    instrMutationIdCon :: DataCon,
    -- | Operators to try at each expression site.
    instrOperators :: [MutationOperator]
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
  InstrM a ->
  TcM (a, [MutationRecord])
runInstrument tcGblEnv operators action = do
  let rdrEnv = tcg_rdr_env tcGblEnv
      modul = tcg_mod tcGblEnv
  ifMutId <- lookupRdrEnvId rdrEnv "ifMutation"
  mutIdCon <- lookupRdrEnvDataCon rdrEnv "MutationId"
  runReaderT
    (runWriterT action)
    InstrumentEnv
      { instrModule = modul,
        instrIfMutationId = ifMutId,
        instrMutationIdCon = mutIdCon,
        instrOperators = operators
      }

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
  FunBind x name mg -> FunBind x name <$> instrumentMatchGroup mg
  PatBind x pat mult rhs -> PatBind x pat mult <$> instrumentGRHSs rhs
  VarBind x var rhs -> VarBind x var <$> instrumentLExpr rhs
  PatSynBind x psb -> pure (PatSynBind x psb)
  -- At GhcTc, XHsBindsLR carries an AbsBinds (see XXHsBindsLR instance).
  XHsBindsLR ab@AbsBinds {abs_binds} -> do
    binds' <- mapBagM (traverse instrumentBind) abs_binds
    pure (XHsBindsLR ab {abs_binds = binds'})

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
  GRHS x guards body -> GRHS x guards <$> instrumentLExpr body

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
  le' <- traverse (instrumentExpr (getLocA le)) le
  InstrumentEnv {instrOperators} <- ask
  tryMutateWith instrOperators le'

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
  BodyStmt x e se1 se2 -> BodyStmt x <$> instrumentLExpr e <*> pure se1 <*> pure se2
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

-- | The list of operators to try.  Imported from 'Test.Syd.Mutation.Plugin.Operators'.
-- Defined as a parameter here so that 'Instrument.hs' stays operator-agnostic.
-- 'instrumentModule' is called with the concrete list from 'Plugin.hs'.
tryMutateWith :: [MutationOperator] -> LHsExpr GhcTc -> InstrM (LHsExpr GhcTc)
tryMutateWith operators le =
  foldM applyOperator le operators
  where
    applyOperator expr op = case operatorMatch op expr of
      Nothing -> pure expr
      Just (ty, mutated, origStr, replStr) -> do
        mid <- recordMutation expr (operatorName op) origStr replStr
        wrapWithIfMutation ty mid mutated expr

-- | Record one mutation site and return its 'MutationId'.
recordMutation ::
  LHsExpr GhcTc ->
  String ->
  String ->
  String ->
  InstrM MutationId
recordMutation le op origStr replStr = do
  InstrumentEnv {instrModule} <- ask
  let sp = getLocA le
  case sp of
    RealSrcSpan rss _ -> do
      let mn = moduleNameString (moduleName instrModule)
          mid =
            MutationId
              [ mn,
                op,
                show (srcSpanStartLine rss),
                show (srcSpanStartCol rss),
                show (srcSpanEndCol rss)
              ]
      tell
        [ MutationRecord
            { mutRecId = mid,
              mutRecOperator = op,
              mutRecOriginal = origStr,
              mutRecReplacement = replStr
            }
        ]
      pure mid
    UnhelpfulSpan _ -> pure (MutationId [])

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
-- > MutationId ["module", "op", "line", "colStart", "colEnd"]
buildMutationIdExpr :: DataCon -> MutationId -> TcM (LHsExpr GhcTc)
buildMutationIdExpr con (MutationId parts) = do
  let strExprs = map mkHsStringExpr parts
      listExpr = noLocA (ExplicitList (mkListTy charTy) strExprs)
      -- MutationId is a newtype, so the constructor is just a coercion.
      -- nlHsDataCon gives us the constructor as an HsExpr GhcTc.
      conExpr = nlHsDataCon con
  pure (mkHsApp conExpr listExpr)

-- | Build a string literal expression of type 'String' (= '[Char]').
mkHsStringExpr :: String -> LHsExpr GhcTc
mkHsStringExpr s =
  noLocA (HsLit NoExtField (HsString NoSourceText (mkFastString s)))
