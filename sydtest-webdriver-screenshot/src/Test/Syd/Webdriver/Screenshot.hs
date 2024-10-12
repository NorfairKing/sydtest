{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Because of webdriver using dangerous constructors
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
-- For the undefined trick
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Test.Syd.Webdriver.Screenshot where

import Codec.Picture as Picture
import Codec.Picture.Types (createMutableImage, mutableImageData)
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as Vector
import Debug.Trace
import Path
import Path.IO
import System.Exit
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver as WD

-- | A screenshot with location
data Screenshot = Screenshot
  { -- | File location for comparisons
    screenshotFile :: !(Path Abs File),
    -- | Decoded image
    screenshotImage :: !(Picture.Image PixelRGB8)
  }

-- | Take a screenshot and turn it into a golden test.
goldenScreenshotHere :: FilePath -> WebdriverTestM app (GoldenTest Screenshot)
goldenScreenshotHere fp = pureGoldenScreenshot fp <$> WD.screenshot

-- | Make a golden test for a given screenshot in lazy 'LB.ByteString' form.
pureGoldenScreenshot :: FilePath -> LB.ByteString -> GoldenTest Screenshot
pureGoldenScreenshot fp contents =
  GoldenTest
    { goldenTestRead = do
        relFile <- parseRelFile fp
        currentDir <- getCurrentDir
        let resolvedFile = currentDir </> relFile
        mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile resolvedFile
        forM mContents $ \cts -> do
          case decodePng cts of
            Left err -> die err
            Right dynamicImage ->
              pure $
                Screenshot
                  { screenshotFile = resolvedFile,
                    screenshotImage = convertRGB8 dynamicImage
                  },
      goldenTestProduce = do
        image <- normaliseImage contents
        relFile <- parseRelFile fp

        tempDir <- resolveDir' "screenshot-comparison"
        let tempFile = tempDir </> relFile
        ensureDir $ parent tempFile
        -- Write it to a file so we can compare it if it differs.
        writePng (fromAbsFile tempFile) image

        pure $
          Screenshot
            { screenshotFile = tempFile,
              screenshotImage = image
            },
      goldenTestWrite = \(Screenshot _ actual) -> do
        relFile <- parseRelFile fp
        currentDir <- getCurrentDir
        let resolvedFile = currentDir </> relFile
        ensureDir $ parent resolvedFile
        writePng (fromAbsFile resolvedFile) actual,
      goldenTestCompare = \(Screenshot actualPath actual) (Screenshot expectedPath expected) ->
        if actual == expected
          then pure Nothing
          else do
            tempDir <- resolveDir' "screenshot-comparison"
            relFile <- parseRelFile fp
            diffRelFile <- replaceExtension ".diff" relFile >>= addExtension ".png"
            let diffFile = tempDir </> diffRelFile
            ensureDir $ parent diffFile
            writePng (fromAbsFile diffFile) (computeImageDiff actual expected)
            pure $
              Just $
                ExpectationFailed $
                  trace "sim" $
                    unlines
                      [ "Screenshots differ.",
                        "expected: " <> fromAbsFile expectedPath,
                        "actual: " <> fromAbsFile actualPath,
                        "diff: " <> fromAbsFile diffFile,
                        "similarity: " <> show (imageSimilarity actual expected)
                      ]
    }

imageSimilarity :: Image PixelRGB8 -> Image PixelRGB8 -> Double
imageSimilarity actual expected =
  let width = max (imageWidth actual) (imageWidth expected)
      height = max (imageHeight actual) (imageHeight expected)
   in (/ fromIntegral (height * width)) $
        sum $
          flip map [0 .. width - 1] $ \w ->
            sum $
              flip map [0 .. height - 1] $ \h ->
                let actualPixel = lookupPixelAt actual w h
                    expectedPixel = lookupPixelAt expected w h
                 in pixelSimilarity actualPixel expectedPixel

pixelSimilarity :: Maybe PixelRGB8 -> Maybe PixelRGB8 -> Double
pixelSimilarity Nothing _ = 0
pixelSimilarity _ Nothing = 0
pixelSimilarity (Just (PixelRGB8 r1 g1 b1)) (Just (PixelRGB8 r2 g2 b2)) =
  if and [r1 == r2, g1 == g2, b1 == b2]
    then 1
    else 0

computeImageDiff :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
computeImageDiff actual expected =
  let width = max (imageWidth actual) (imageWidth expected)
      height = max (imageHeight actual) (imageHeight expected)
   in Image
        { imageWidth = width,
          imageHeight = height,
          imageData = Vector.create $ do
            mutableImage <- createMutableImage width height (PixelRGB8 0 0 0)
            forM_ [0 .. width - 1] $ \w ->
              forM_ [0 .. height - 1] $ \h -> do
                let actualPixel = lookupPixelAt actual w h
                    expectedPixel = lookupPixelAt expected w h
                writePixel mutableImage w h (computePixelDiff actualPixel expectedPixel)
            pure $ mutableImageData mutableImage
        }

lookupPixelAt :: (Pixel a) => Image a -> Int -> Int -> Maybe a
lookupPixelAt image w h
  | w < 0 = Nothing
  | h < 0 = Nothing
  | w >= imageWidth image = Nothing
  | h >= imageHeight image = Nothing
  | otherwise = Just $ pixelAt image w h

computePixelDiff :: Maybe PixelRGB8 -> Maybe PixelRGB8 -> PixelRGB8
computePixelDiff Nothing _ = PixelRGB8 0 0 255
computePixelDiff _ Nothing = PixelRGB8 255 0 0
computePixelDiff (Just (PixelRGB8 r1 g1 b1)) (Just (PixelRGB8 r2 g2 b2)) =
  if or [r1 /= r2, g1 /= g2, b1 /= b2]
    then PixelRGB8 0 255 0
    else PixelRGB8 0 0 0

debugScreenshot :: FilePath -> WebdriverTestM app ()
debugScreenshot fp = do
  contents <- screenshot
  liftIO $ do
    image <- normaliseImage contents
    writePng fp image

normaliseImage :: LB.ByteString -> IO (Image PixelRGB8)
normaliseImage contents = do
  let sb = LB.toStrict contents
  case decodePng sb of
    Left err -> expectationFailure $ "Could not parse screenshot as png: " <> err
    Right dynamicImage -> do
      let image = convertRGB8 dynamicImage
      pure image
