{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Output.Pretty where

import Control.Arrow (second)
import Data.List (sortOn)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Text
import GHC.Stack
import Safe
import Test.Syd.OptParse
import Test.Syd.Output.Common
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour
import Text.Printf

renderPrettyReport :: Settings -> Timed ResultForest -> Text.Builder
renderPrettyReport settings rf =
  mconcat $
    map
      (\line -> renderChunksBuilder (settingTerminalCapabilities settings) line <> "\n")
      (outputResultReport settings rf)

outputResultReport :: Settings -> Timed ResultForest -> [[Chunk]]
outputResultReport settings trf =
  let rf = timedValue trf
   in concat
        [ outputTestsHeader,
          outputSpecForest settings 0 (resultForestWidth rf) rf,
          [ [chunk ""],
            [chunk ""]
          ],
          outputPrettySummary settings trf
        ]

outputPrettySummary :: Settings -> Timed ResultForest -> [[Chunk]]
outputPrettySummary settings trf =
  let rf = timedValue trf
   in concat
        [ outputFailuresWithHeading settings rf,
          [[chunk ""]],
          outputStats (computeTestSuiteStats settings <$> trf),
          [[chunk ""]],
          if settingProfile settings
            then outputProfilingInfo trf
            else []
        ]

outputFailuresHeader :: [[Chunk]]
outputFailuresHeader = outputHeader "Failures:"

outputFailuresWithHeading :: Settings -> ResultForest -> [[Chunk]]
outputFailuresWithHeading settings rf =
  if anyFailedTests settings rf
    then
      concat
        [ outputFailuresHeader,
          outputFailures settings rf
        ]
    else []

outputStats :: Timed TestSuiteStats -> [[Chunk]]
outputStats timed =
  let TestSuiteStats {..} = timedValue timed
      sumTimeSeconds :: Double
      sumTimeSeconds = fromIntegral testSuiteStatSumTime / 1_000_000_000
      totalTimeSeconds :: Double
      totalTimeSeconds = fromIntegral (timedTime timed) / 1_000_000_000
   in map (padding :) $
        concat
          [ [ [ chunk "Examples:                     ",
                fore green $ chunk (T.pack (show testSuiteStatExamples))
              ]
            | testSuiteStatExamples /= testSuiteStatSuccesses
            ],
            [ [ chunk "Passed:                       ",
                ( if testSuiteStatSuccesses <= 0
                    then fore red
                    else fore green
                )
                  $ chunk (T.pack (show testSuiteStatSuccesses))
              ],
              [ chunk "Failed:                       ",
                ( if testSuiteStatFailures > 0
                    then fore red
                    else fore green
                )
                  $ chunk (T.pack (show testSuiteStatFailures))
              ]
            ],
            [ [ chunk "Flaky:                        ",
                fore red $ chunk (T.pack (show testSuiteStatFlakyTests))
              ]
            | testSuiteStatFlakyTests > 0
            ],
            [ [ chunk "Pending:                      ",
                fore magenta $ chunk (T.pack (show testSuiteStatPending))
              ]
            | testSuiteStatPending > 0
            ],
            [ [ chunk "Sum of test runtimes:",
                fore yellow $ chunk $ T.pack (printf "%13.2f seconds" sumTimeSeconds)
              ],
              [ chunk "Test suite took:     ",
                fore yellow $ chunk $ T.pack (printf "%13.2f seconds" totalTimeSeconds)
              ]
            ]
          ]

outputProfilingInfo :: Timed ResultForest -> [[Chunk]]
outputProfilingInfo Timed {..} =
  map
    ( \(path, nanos) ->
        [ timeChunkFor nanos,
          " ",
          chunk $ T.intercalate "." path
        ]
    )
    ( sortOn
        snd
        ( map
            (second (timedTime . testDefVal))
            (flattenSpecForest timedValue)
        )
    )

outputTestsHeader :: [[Chunk]]
outputTestsHeader = outputHeader "Tests:"

outputHeader :: Text -> [[Chunk]]
outputHeader t =
  [ [fore blue $ chunk t],
    [chunk ""]
  ]

outputSpecForest :: Settings -> Int -> Int -> ResultForest -> [[Chunk]]
outputSpecForest settings level treeWidth = concatMap (outputSpecTree settings level treeWidth)

outputSpecTree :: Settings -> Int -> Int -> ResultTree -> [[Chunk]]
outputSpecTree settings level treeWidth = \case
  SpecifyNode t td -> outputSpecifyLines settings level treeWidth t td
  PendingNode t mr -> outputPendingLines t mr
  DescribeNode t sf -> outputDescribeLine t : map (padding :) (outputSpecForest settings (level + 1) treeWidth sf)
  SubForestNode sf -> outputSpecForest settings level treeWidth sf

outputDescribeLine :: Text -> [Chunk]
outputDescribeLine t = [fore yellow $ chunk t]

outputSpecifyLines :: Settings -> Int -> Int -> Text -> TDef (Timed TestRunReport) -> [[Chunk]]
outputSpecifyLines settings level treeWidth specifyText (TDef timed _) =
  let testRunReport = timedValue timed
      executionTime = timedTime timed
      status = testRunReportStatus settings testRunReport
      TestRunResult {..} = testRunReportReportedRun testRunReport
      withStatusColour = fore (statusColour status)
      pad = (chunk (T.pack (replicate paddingSize ' ')) :)
      timeChunk = timeChunkFor executionTime
   in concatMap
        (filter (not . null))
        [ [ [ withStatusColour $ chunk (statusCheckMark status),
              withStatusColour $ chunk specifyText,
              spacingChunk level specifyText (chunkText timeChunk) treeWidth,
              timeChunk
            ]
          ],
          map pad $ retriesChunks testRunReport,
          [ pad
              [ chunk "passed for all of ",
                case w of
                  0 -> fore red $ chunk "0"
                  _ -> fore green $ chunk (T.pack (printf "%d" w)),
                " inputs."
              ]
          | status == TestPassed,
            w <- maybeToList testRunResultNumTests
          ],
          map pad $ labelsChunks (fromMaybe 1 testRunResultNumTests) testRunResultLabels,
          map pad $ classesChunks testRunResultClasses,
          map pad $ tablesChunks testRunResultTables,
          [pad $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase]
        ]

exampleNrChunk :: Word -> Word -> Chunk
exampleNrChunk total current =
  let digits :: Word
      digits = max 2 $ succ $ floor $ logBase 10 $ (fromIntegral :: Word -> Double) total
      formatStr = "%" <> show digits <> "d"
   in chunk $ T.pack $ printf formatStr current

retriesChunks :: TestRunReport -> [[Chunk]]
retriesChunks testRunReport =
  case testRunReportRetries testRunReport of
    Nothing -> []
    Just retries ->
      let flaky = testRunReportWasFlaky testRunReport
          mMessage = case testRunReportFlakinessMode testRunReport of
            MayBeFlaky mmesg -> mmesg
            MayNotBeFlaky -> Nothing
       in if flaky
            then
              concat
                [ [["Retries: ", chunk (T.pack (show retries)), fore red " !!! FLAKY !!!"]],
                  [[fore magenta $ chunk $ T.pack message] | message <- maybeToList mMessage]
                ]
            else [["Retries: ", chunk (T.pack (show retries)), " (does not look flaky)"]]

labelsChunks :: Word -> Maybe (Map [String] Int) -> [[Chunk]]
labelsChunks _ Nothing = []
labelsChunks totalCount (Just labels)
  | M.null labels = []
  | map fst (M.toList labels) == [[]] = []
  | otherwise =
      [chunk "Labels"]
        : map
          ( pad
              . ( \(ss, i) ->
                    [ chunk
                        ( T.pack
                            ( printf
                                "%5.2f%% %s"
                                (100 * fromIntegral i / fromIntegral totalCount :: Double)
                                (commaList (map show ss))
                            )
                        )
                    ]
                )
          )
          (M.toList labels)
  where
    pad = (chunk (T.pack (replicate paddingSize ' ')) :)

classesChunks :: Maybe (Map String Int) -> [[Chunk]]
classesChunks Nothing = []
classesChunks (Just classes)
  | M.null classes = []
  | otherwise =
      [chunk "Classes"]
        : map
          ( pad
              . ( \(s, i) ->
                    [ chunk
                        ( T.pack
                            (printf "%5.2f%% %s" (100 * fromIntegral i / fromIntegral total :: Double) s)
                        )
                    ]
                )
          )
          (M.toList classes)
  where
    pad = (chunk (T.pack (replicate paddingSize ' ')) :)
    total = sum $ map snd $ M.toList classes

tablesChunks :: Maybe (Map String (Map String Int)) -> [[Chunk]]
tablesChunks Nothing = []
tablesChunks (Just tables) = concatMap (uncurry goTable) $ M.toList tables
  where
    goTable :: String -> Map String Int -> [[Chunk]]
    goTable tableName percentages =
      [chunk " "]
        : [chunk (T.pack tableName)]
        : map
          ( pad
              . ( \(s, i) ->
                    [ chunk
                        ( T.pack
                            (printf "%5.2f%% %s" (100 * fromIntegral i / fromIntegral total :: Double) s)
                        )
                    ]
                )
          )
          (M.toList percentages)
      where
        pad = (chunk (T.pack (replicate paddingSize ' ')) :)
        total = sum $ map snd $ M.toList percentages

outputPendingLines :: Text -> Maybe Text -> [[Chunk]]
outputPendingLines specifyText mReason =
  filter
    (not . null)
    [ [fore magenta $ chunk specifyText],
      case mReason of
        Nothing -> []
        Just reason -> [padding, chunk reason]
    ]

outputFailureLabels :: Maybe (Map [String] Int) -> [[Chunk]]
outputFailureLabels Nothing = []
outputFailureLabels (Just labels)
  | labels == M.singleton [] 1 = []
  | otherwise = [["Labels: ", chunk (T.pack (commaList (map show (concat $ M.keys labels))))]]

outputFailureClasses :: Maybe (Map String Int) -> [[Chunk]]
outputFailureClasses Nothing = []
outputFailureClasses (Just classes)
  | M.null classes = []
  | otherwise = [["Class: ", chunk (T.pack (commaList (M.keys classes)))]]

outputGoldenCase :: GoldenCase -> [Chunk]
outputGoldenCase = \case
  GoldenNotFound -> [fore red $ chunk "Golden output not found"]
  GoldenStarted -> [fore cyan $ chunk "Golden output created"]
  GoldenReset -> [fore cyan $ chunk "Golden output reset"]

-- The chunk for spacing between the description and the timing
--
-- initial padding | checkmark | description | THIS CHUNK | execution time
spacingChunk :: Int -> Text -> Text -> Int -> Chunk
spacingChunk level descriptionText executionTimeText treeWidth = chunk $ T.pack $ replicate paddingWidth ' '
  where
    paddingWidth =
      let preferredMaxWidth = 80
          checkmarkWidth = 2
          minimumSpacing = 1
          actualDescriptionWidth = T.length descriptionText
          actualTimingWidth = T.length executionTimeText
          totalNecessaryWidth = treeWidth + checkmarkWidth + minimumSpacing + actualTimingWidth -- All timings are the same width
          actualMaxWidth = max totalNecessaryWidth preferredMaxWidth
       in actualMaxWidth - paddingSize * level - actualTimingWidth - actualDescriptionWidth

outputFailures :: Settings -> ResultForest -> [[Chunk]]
outputFailures settings rf =
  let failures = filter (testRunReportFailed settings . timedValue . testDefVal . snd) $ flattenSpecForest rf
      nbDigitsInFailureCount :: Int
      nbDigitsInFailureCount = floor (logBase 10 (L.genericLength failures) :: Double)
      padFailureDetails = (chunk (T.pack (replicate (nbDigitsInFailureCount + 4) ' ')) :)
   in map (padding :) $
        concatMap (filter (not . null)) $
          indexed failures $ \w (ts, TDef timed cs) ->
            let testRunReport = timedValue timed
                status = testRunReportStatus settings testRunReport
                TestRunResult {..} = testRunReportReportedRun testRunReport
             in concat
                  [ [ [ fore cyan $
                          chunk $
                            T.pack $
                              replicate 2 ' '
                                ++ case headMay $ getCallStack cs of
                                  Nothing -> "Unknown location"
                                  Just (_, SrcLoc {..}) ->
                                    concat
                                      [ srcLocFile,
                                        ":",
                                        show srcLocStartLine
                                      ]
                      ],
                      map
                        (fore (statusColour status))
                        [ chunk $ statusCheckMark status,
                          chunk $ T.pack (printf ("%" ++ show nbDigitsInFailureCount ++ "d ") w),
                          chunk $ T.intercalate "." ts
                        ]
                    ],
                    map padFailureDetails $ retriesChunks testRunReport,
                    map (padFailureDetails . (: []) . chunk . T.pack) $
                      case (testRunResultNumTests, testRunResultNumShrinks) of
                        (Nothing, _) -> []
                        (Just numTests, Nothing) -> [printf "Failed after %d tests" numTests]
                        (Just numTests, Just 0) -> [printf "Failed after %d tests" numTests]
                        (Just numTests, Just numShrinks) -> [printf "Failed after %d tests and %d shrinks" numTests numShrinks],
                    map (padFailureDetails . (\c -> [chunk "Generated: ", c]) . fore yellow . chunk . T.pack) testRunResultFailingInputs,
                    map padFailureDetails $ outputFailureLabels testRunResultLabels,
                    map padFailureDetails $ outputFailureClasses testRunResultClasses,
                    map padFailureDetails $ maybe [] outputSomeException testRunResultException,
                    [padFailureDetails $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase],
                    concat [map padFailureDetails $ stringChunks ei | ei <- maybeToList testRunResultExtraInfo],
                    [[chunk ""]]
                  ]
