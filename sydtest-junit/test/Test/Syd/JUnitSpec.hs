module Test.Syd.JUnitSpec (spec) where

import qualified Data.ByteString as SB
import Path
import Path.IO
import Test.Syd
import Test.Syd.JUnit
import Test.Syd.OptParse
import Test.Syd.Path
import Test.Syd.TestUtils
import Text.XML.JUnit

spec :: Spec
spec = tempDirSpec "sydtest-junit" $ sequential $ do
  it "renders the xml the same way" $ \tdir -> do
    result <- sydTestResult defaultSettings exampleSpec

    -- We have to go via this temp file because the JUnit library does not have
    -- a way to compute the XML purely.
    tempFile <- resolveFile tdir "example.xml"
    let fp = fromAbsFile tempFile

    writeXmlReport fp $ renderJUnitSuites $ eraseTiming result
    actual <- SB.readFile fp
    pure $ pureGoldenByteStringFile "test_resources/example.xml" actual

  it "can be run with 'just' sydTestJUnit'" $ \tdir -> do
    withCurrentDir tdir $
      sydTestJUnit passingSpec

exampleSpec :: Spec
exampleSpec = do
  describe "subSuite" $ do
    it "passes" True
    it "fails" False
  describe "withMessage" $ do
    it "errors" (error "failure message" :: Bool)
  pending "skipped"

passingSpec :: Spec
passingSpec = do
  describe "subSuite" $ do
    it "passes" True
  pending "skipped"
