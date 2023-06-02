{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.SVG (writeSvgReport) where

import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Graphics.Svg as Svg
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest

writeSvgReport :: FilePath -> Timed ResultForest -> IO ()
writeSvgReport fp trf = do
  let svgLBs = Svg.renderBS $ timedResultForestElement trf
  let completeFile =
        mconcat
          [ "<html><head><style>",
            style,
            "</style></head><body><div id=\"container\">",
            svgLBs,
            "</div></body></html>"
          ]
  LB.writeFile fp completeFile

timedResultForestElement :: Timed ResultForest -> Svg.Element
timedResultForestElement trf =
  let tests = flattenSpecForest (timedValue trf)
      runBegin = timedBegin trf
      runEnd = timedEnd trf
      totalDuration = runEnd - runBegin
      maximumMay [] = Nothing
      maximumMay l = Just $ maximum l
      maxWorker :: Int
      maxWorker = fromMaybe 0 $ maximumMay $ map (timedWorker . testDefVal . snd) tests
      nanosPerSecond = 1_000_000_000
      maximumTime = ceiling (fromIntegral totalDuration / fromIntegral nanosPerSecond :: Double) * nanosPerSecond
   in ( \e ->
          with
            (svg11_ e)
            [ Height_ <<- fromString (show (workerY (maxWorker + 1)) <> "px"),
              Width_ <<- fromString (show (timeX maximumTime maximumTime) <> "px")
            ]
      )
        $ mconcat
          [ -- Thread labels
            g_ [] $
              mconcat $
                flip map [0 .. maxWorker] $ \workerIx ->
                  text_
                    [ X_ <<- "0",
                      Y_ <<- fromString (show (workerY workerIx))
                    ]
                    (toElement (show workerIx)),
            -- Timing labels
            g_ []
              $ mconcat
              $ flip
                map
                [ 0,
                  nanosPerSecond -- In steps of 1 second
                  .. totalDuration
                ]
              $ \t ->
                mconcat
                  [ -- Label
                    text_
                      [ X_ <<- fromString (show (timeX maximumTime t)),
                        Y_ <<- fromString (show (topBarHeight - fontSize))
                      ]
                      (toElement (show (t `div` 1_000_000_000) <> " s")),
                    -- Line
                    line_
                      [ X1_ <<- fromString (show (timeX maximumTime t)),
                        Y1_ <<- fromString (show topBarHeight),
                        X2_ <<- fromString (show (timeX maximumTime t)),
                        Y2_ <<- fromString (show (workerY (maxWorker + 1))),
                        Class_ <<- "time"
                      ]
                      ""
                  ],
            g_ [] $
              mconcat $
                flip map tests $ \(path, TDef timed _) ->
                  let begin = timedBegin timed - runBegin
                      end = timedEnd timed - runBegin
                      duration = end - begin
                      workerIx = timedWorker timed
                      title =
                        T.pack $
                          unlines
                            [ show $ T.intercalate "." path,
                              show (duration `div` 1_000_000) <> "ms"
                            ]
                   in mconcat
                        [ rect_
                            [ X_ <<- fromString (show (timeX maximumTime begin)),
                              Y_ <<- fromString (show (workerY workerIx - barHeight `div` 2)),
                              Font_size_ <<- fromString (show fontSize),
                              Width_ <<- fromString (show (nanosToX totalDuration duration)),
                              Height_ <<- fromString (show barHeight),
                              Class_ <<- "test ",
                              Style_ <<- testStyle duration
                            ]
                            ( title_
                                []
                                ( text_
                                    []
                                    (toElement title)
                                )
                            )
                        ]
          ]

fontSize :: Int
fontSize = 20

timeX :: Word64 -> Word64 -> Int
timeX maximumTime time = leftBarWidth + nanosToX maximumTime time

workerY :: Int -> Int
workerY workerIx = topBarHeight + (workerIx + 1) * barHeight + workerIx * barSpacing

topBarHeight :: Int
topBarHeight = 50

leftBarWidth :: Int
leftBarWidth = 50

barHeight :: Int
barHeight = 40

barSpacing :: Int
barSpacing = 5

nanosToX :: Word64 -> Word64 -> Int
nanosToX totalDuration n =
  round $
    fromIntegral n / (fromIntegral totalDuration / (1_800 :: Double))

testStyle :: Word64 -> Text
testStyle runtime =
  let (fill, stroke) = testColours runtime
   in T.pack $
        concat
          [ "fill: ",
            renderRedGreen fill,
            ";",
            "stroke:",
            renderRedGreen stroke,
            ";"
          ]

data RedGreen
  = RedGreen
      !Word8 -- Red
      !Word8 -- Green

renderRedGreen :: RedGreen -> String
renderRedGreen (RedGreen r g) = concat ["rgb(", show r, ",", show g, ",0)"]

testColours :: Word64 -> (RedGreen, RedGreen)
testColours duration =
  let fill = testFill duration
      stroke = testStroke fill
   in (fill, stroke)

-- Red to green are the colours
-- (ff, 00, 00) -> (ff, ff, 00), (00, ff, ff)
testFill :: Word64 -> RedGreen
testFill duration =
  let t :: Double
      t =
        max
          1 -- We don't care about any differences below 1 ms, and they could
          -- cause trouble with the logarithm.
          (fromIntegral duration / 1_000_000)
      midway :: Double
      midway = 500 -- ms
      -- This means that tlog will be between
      -- 0(1ms) and 1(500ms): green
      -- 1(500ms) and 2(around 200sec): red
      tlog = min 2 $ logBase midway t
   in if tlog <= 1
        then -- Faster than 500ms
        -- Between green and yellow
        -- So definitely maximum green.
        --
        -- The faster the test, the darker the colour should be,
        -- The faster the test, the smaller tlog, the smaller the red component.
          RedGreen (round (tlog * 200)) 255
        else -- Slower than 500 ms
        -- Between yellow and red.
        -- So definitely maximum red.
        -- The slower the test, the darker the colour should be.
          RedGreen 255 (round ((2 - tlog) * 255))

-- Make the stroke colour based on the fill colour
testStroke :: RedGreen -> RedGreen
testStroke (RedGreen r g) =
  let darken c = round (fromIntegral c * 0.75 :: Double)
   in RedGreen (darken r) (darken g)

style :: LB.ByteString
style =
  LB.intercalate
    "\n"
    [ "div#container {",
      "  height: 100%;",
      "  width: 100%;",
      "  overflow: scroll;",
      "}",
      "svg {",
      "  border: 1px dotted grey;",
      "}",
      ".test {",
      "  pointer-events: all;",
      "  stroke-width: 3;",
      "}",
      ".test:hover {",
      "  stroke: magenta !important;",
      "}",
      ".time {",
      "  stroke: black;",
      "  stroke-width: 1;",
      "  stroke-dasharray: 10,10;",
      "  opacity: 0.5",
      "}"
    ]
