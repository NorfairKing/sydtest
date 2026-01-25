{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Output.Terse where

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

-- | Render a terse report
renderTerseSummary :: Settings -> Timed ResultForest -> Text.Builder
renderTerseSummary settings trf =
  mconcat $
    map
      (\line -> renderChunksBuilder (settingTerminalCapabilities settings) line <> "\n")
      (outputTerseSummary settings trf)

-- | Output the terse report as chunks.
outputTerseSummary :: Settings -> Timed ResultForest -> [[Chunk]]
outputTerseSummary settings trf =
  let rf = timedValue trf
      failures = filter (testRunReportFailed settings . timedValue . testDefVal . snd) $ flattenSpecForest rf
      stats = computeTestSuiteStats settings rf
      totalTimeSeconds = fromIntegral (timedTime trf) / 1_000_000_000 :: Double
   in concat
        [ concatMap (outputTerseFailure settings) failures,
          [outputTerseStats stats totalTimeSeconds]
        ]

-- | Output a single failure in terse format.
outputTerseFailure :: Settings -> ([Text], TDef (Timed TestRunReport)) -> [[Chunk]]
outputTerseFailure _settings (ts, TDef timed cs) =
  let testRunReport = timedValue timed
      TestRunResult {..} = testRunReportReportedRun testRunReport
      location = case headMay $ getCallStack cs of
        Nothing -> "Unknown location"
        Just (_, SrcLoc {..}) -> concat [srcLocFile, ":", show srcLocStartLine]
      testPath = T.intercalate "." ts
   in concat
        [ [ [ chunk "FAIL ",
              chunk (T.pack location),
              chunk " ",
              chunk testPath
            ]
          ],
          map
            (\l -> if null l then l else padding : l)
            (maybe [] outputSomeException testRunResultException),
          [[chunk ""]]
        ]

-- | Output the summary line in terse format.
--
-- Format: Summary: X failed, Y passed, Z pending (Ns)
outputTerseStats :: TestSuiteStats -> Double -> [Chunk]
outputTerseStats TestSuiteStats {..} totalTimeSeconds =
  concat $
    concat
      [ [[padding]],
        [ [ chunk "Passed: ",
            ( if testSuiteStatSuccesses <= 0
                then fore red
                else fore green
            )
              $ chunk (T.pack (show testSuiteStatSuccesses))
          ],
          [ chunk ", Failed: ",
            ( if testSuiteStatFailures > 0
                then fore red
                else fore green
            )
              $ chunk (T.pack (show testSuiteStatFailures))
          ]
        ],
        [ [ chunk ", Flaky: ",
            fore red $ chunk (T.pack (show testSuiteStatFlakyTests))
          ]
          | testSuiteStatFlakyTests > 0
        ],
        [ [ chunk ", Pending: ",
            fore magenta $ chunk (T.pack (show testSuiteStatPending))
          ]
          | testSuiteStatPending > 0
        ],
        [ [ fore yellow $ chunk $ T.pack (printf " (%0.2f s)" totalTimeSeconds)
          ]
        ]
      ]
