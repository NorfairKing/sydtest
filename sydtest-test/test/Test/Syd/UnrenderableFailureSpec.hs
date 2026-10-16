{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.UnrenderableFailureSpec (spec) where

import Control.DeepSeq (force)
import Control.Exception
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Runner.Single (reportUnrenderableFailures)
import Text.Colour (chunkText)

-- | An exception that cannot be rendered, the way @effectful@'s errors cannot
-- be compared: a 'displayException' that throws.
data UnrenderableDisplay = UnrenderableDisplay
  deriving (Show)

instance Exception UnrenderableDisplay where
  displayException _ = throw (userError "this displayException throws")

failedWith :: SomeException -> TestRunResult
failedWith e =
  TestRunResult
    { testRunResultStatus = TestFailed,
      testRunResultException = Just e,
      testRunResultNumTests = Nothing,
      testRunResultNumShrinks = Nothing,
      testRunResultFailingInputs = [],
      testRunResultLabels = Nothing,
      testRunResultClasses = Nothing,
      testRunResultTables = Nothing,
      testRunResultGoldenCase = Nothing,
      testRunResultExtraInfo = Nothing
    }

-- | What the output does with a result, which is where an unrenderable failure
-- used to take the suite down.
--
-- The extra info as well as the exception, because the pretty output renders
-- both of them in its failures.
render :: TestRunResult -> IO T.Text
render result =
  evaluate $
    force $
      T.concat
        [ T.concat $
            map chunkText $
              concat $
                maybe [] outputSomeException $
                  testRunResultException result,
          T.pack $ fromMaybe "" $ testRunResultExtraInfo result
        ]

spec :: Spec
spec = describe "reportUnrenderableFailures" $ do
  it "leaves a failure that renders alone" $ do
    let result = failedWith (toException (userError "this renders fine"))
    reported <- reportUnrenderableFailures result
    rendered <- render reported
    rendered `shouldSatisfy` T.isInfixOf "this renders fine"
    testRunResultStatus reported `shouldBe` TestFailed

  it "replaces a failure that does not render with one that does" $ do
    let result = failedWith (toException UnrenderableDisplay)
    reported <- reportUnrenderableFailures result
    rendered <- render reported
    rendered `shouldSatisfy` T.isInfixOf "could not be rendered"
    -- What threw is still named, because that is the part worth acting on.
    rendered `shouldSatisfy` T.isInfixOf "this displayException throws"

  it "keeps the test failed, rather than making the rendering the failure" $ do
    let result = failedWith (toException UnrenderableDisplay)
    reported <- reportUnrenderableFailures result
    testRunResultStatus reported `shouldBe` TestFailed

  it "keeps nothing else that could not be rendered either" $ do
    let result = (failedWith (toException UnrenderableDisplay)) {testRunResultFailingInputs = ["an input"]}
    reported <- reportUnrenderableFailures result
    testRunResultFailingInputs reported `shouldBe` []

  it "replaces extra info that does not render either" $ do
    -- The pretty output renders the extra info of a failure as well, so
    -- leaving an unrenderable one in place would take the suite down there
    -- instead.
    let result =
          (failedWith (toException (userError "this renders fine")))
            { testRunResultExtraInfo = Just (error "this extra info throws")
            }
    reported <- reportUnrenderableFailures result
    rendered <- render reported
    rendered `shouldSatisfy` T.isInfixOf "could not be rendered"
    testRunResultExtraInfo reported `shouldBe` Nothing
