{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds -Wno-unused-imports #-}

module Test.Syd.ReRun (withRerunByReport) where

import Autodocodec
import Control.Monad.Writer
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)
import Path
import Path.IO
import Test.Syd.Def
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest

withRerunByReport ::
  Settings ->
  (TestDefM outers inner r -> IO (Timed ResultForest)) ->
  TestDefM outers inner r ->
  IO (Timed ResultForest)
withRerunByReport sets func spec =
  if settingSkipPassed sets
    then do
      mReport <- readReport sets
      resultForest <- func (filterByMReport mReport spec)
      let newReport = collectReport sets resultForest
      let combinedReport = maybe newReport (`combineReport` newReport) mReport
      writeReport sets combinedReport
      pure resultForest
    else func spec

filterByMReport :: Maybe ReportForest -> TestDefM outers inner r -> TestDefM outers inner r
filterByMReport = maybe id filterByReport

filterByReport :: ReportForest -> TestDefM outers inner r -> TestDefM outers inner r
filterByReport report =
  censor
    ( \testForest ->
        -- Don't filter anything if everything was removed because it passed.
        -- This should be the final step that reruns all the tests because
        let (filteredResult, All allPassedOrSkipped) = runWriter (goF report testForest)
         in if allPassedOrSkipped
              then testForest
              else filteredResult
    )
  where
    goF :: ReportForest -> TestForest o i -> Writer All (TestForest o i)
    goF forest = mapM (goT forest)
    goT :: ReportForest -> TestTree o i -> Writer All (TestTree o i)
    goT forest t = case t of
      DefSpecifyNode description _ _ -> do
        case M.lookup description forest of
          Nothing -> do
            -- New test, definitely run it.
            tell $ All False
            pure t
          Just (ReportBranch _) -> do
            -- "it" turned into "describe": new, definitely run.
            tell $ All False
            pure t
          Just (ReportNode passed) -> do
            -- Don't rerun if it's already passed.
            tell $ All passed
            pure $
              if passed
                then DefPendingNode description (Just "Skipped passed test")
                else t
      DefPendingNode {} -> do
        -- Keep the pending node, it doesn't hurt.
        tell $ All True
        pure t
      DefDescribeNode description f ->
        case M.lookup description forest of
          Nothing -> do
            -- New branch, or a branch that wasn't run because of a filter,
            -- definitely run it.
            pure t
          Just (ReportNode _) -> do
            -- "describe" turned into "it": new, definitely run.
            pure t
          Just (ReportBranch deeperForest) ->
            DefDescribeNode description <$> goF deeperForest f
      DefSetupNode func f -> DefSetupNode func <$> goF forest f
      DefBeforeAllNode func f -> DefBeforeAllNode func <$> goF forest f
      DefBeforeAllWithNode func f -> DefBeforeAllWithNode func <$> goF forest f
      DefWrapNode func f -> DefWrapNode func <$> goF forest f
      DefAroundAllNode func f -> DefAroundAllNode func <$> goF forest f
      DefAroundAllWithNode func f -> DefAroundAllWithNode func <$> goF forest f
      DefAfterAllNode func f -> DefAfterAllNode func <$> goF forest f
      DefParallelismNode x f -> DefParallelismNode x <$> goF forest f
      DefRandomisationNode x f -> DefRandomisationNode x <$> goF forest f
      DefTimeoutNode func f -> DefTimeoutNode func <$> goF forest f
      DefRetriesNode func f -> DefRetriesNode func <$> goF forest f
      DefFlakinessNode x f -> DefFlakinessNode x <$> goF forest f
      DefExpectationNode x f -> DefExpectationNode x <$> goF forest f

readReport :: Settings -> IO (Maybe ReportForest)
readReport settings = do
  reportFile <- getReportFile settings
  mContents <- forgivingAbsence $ SB.readFile (fromAbsFile reportFile)
  case mContents of
    Nothing -> pure Nothing
    Just contents ->
      case eitherDecodeJSONViaCodec (LB.fromStrict contents) of
        Left _ ->
          -- If we cant decode the file, just pretend it wasn't there.
          pure Nothing
        Right report -> pure (Just report)

writeReport :: Settings -> ReportForest -> IO ()
writeReport settings report = do
  reportFile <- getReportFile settings
  ensureDir (parent reportFile)
  SB.writeFile (fromAbsFile reportFile) (SB.toStrict (encodeJSONViaCodec report))

getReportFile :: Settings -> IO (Path Abs File)
getReportFile setting = case settingReportFile setting of
  Just fp -> pure fp
  Nothing -> do
    cacheDir <- getXdgDir XdgCache (Just [reldir|sydtest|])
    resolveFile cacheDir "sydtest-report.json"

collectReport :: Settings -> Timed ResultForest -> ReportForest
collectReport settings = goF . timedValue
  where
    goF :: ResultForest -> ReportForest
    goF = M.unions . map goT
    goT :: ResultTree -> Map Text ReportTree
    goT = \case
      DescribeNode description f -> M.singleton description (ReportBranch (goF f))
      SubForestNode f -> goF f
      PendingNode _ _ -> M.empty
      SpecifyNode testReportDescription TDef {..} ->
        let report = timedValue testDefVal
            passed = not $ testRunReportFailed settings report
         in M.singleton testReportDescription (ReportNode passed)

combineReport :: ReportForest -> ReportForest -> ReportForest
combineReport = goF
  where
    goF :: ReportForest -> ReportForest -> ReportForest
    goF = M.unionWith goT
    goT :: ReportTree -> ReportTree -> ReportTree
    goT oldT newT = case (oldT, newT) of
      (ReportNode _, ReportNode newPassed) ->
        -- We could do '||' here but ignoring the old value is more accurate
        -- because the whole point is that we skip passed tests.
        ReportNode newPassed
      (ReportBranch oldForest, ReportBranch newForest) -> ReportBranch $ goF oldForest newForest
      _ -> newT

type ReportForest = Map Text ReportTree

data ReportTree
  = ReportNode !Bool
  | ReportBranch !ReportForest
  deriving stock (Show, Eq, Generic)

instance HasCodec ReportTree where
  codec = named "ReportTree" $ dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left n -> ReportNode n
        Right ts -> ReportBranch ts
      g = \case
        ReportNode n -> Left n
        ReportBranch n -> Right n
