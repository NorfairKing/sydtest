{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.WaiSpec (spec) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Client as HTTP
import Test.Syd
import Test.Syd.OptParse
import Test.Syd.Wai
import Test.Syd.Wai.Example

-- | An exception that a test throws itself, but that is classified as
-- asynchronous, as @effectful@'s @Error@ effect does.
-- See https://github.com/NorfairKing/sydtest/issues/110
data SynchronousButClassifiedAsynchronous = SynchronousButClassifiedAsynchronous
  deriving (Show, Eq)

instance Exception SynchronousButClassifiedAsynchronous where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: Spec
spec = do
  describe "a test that throws an asynchronously-classified exception fails with that exception" $ do
    let settings = defaultSettings {settingRetries = 1}

    it "WaiClientM env ()" $ do
      specForest <-
        execTestDefM settings $
          waiClientSpec exampleApplication $
            it "throws" (liftIO (throwIO SynchronousButClassifiedAsynchronous) :: WaiClientM () ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> WaiClientM env ()" $ do
      specForest <-
        execTestDefM settings $
          waiClientSpec exampleApplication $
            itWithBoth "throws" (const (liftIO (throwIO SynchronousButClassifiedAsynchronous)) :: HTTP.Manager -> WaiClientM () ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "WaiClientM env (GoldenTest a)" $ do
      specForest <-
        execTestDefM settings $
          waiClientSpec exampleApplication $
            it
              "throws"
              ( pure
                  ( GoldenTest
                      { goldenTestRead = pure (Just ()),
                        goldenTestProduce = throwIO SynchronousButClassifiedAsynchronous,
                        goldenTestWrite = \() -> pure (),
                        goldenTestCompare = \() () -> pure Nothing
                      }
                  ) ::
                  WaiClientM () (GoldenTest ())
              )
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> WaiClientM env (GoldenTest a)" $ do
      specForest <-
        execTestDefM settings $
          waiClientSpec exampleApplication $
            itWithBoth
              "throws"
              ( const
                  ( pure
                      ( GoldenTest
                          { goldenTestRead = pure (Just ()),
                            goldenTestProduce = throwIO SynchronousButClassifiedAsynchronous,
                            goldenTestWrite = \() -> pure (),
                            goldenTestCompare = \() () -> pure Nothing
                          }
                      )
                  ) ::
                  HTTP.Manager -> WaiClientM () (GoldenTest ())
              )
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

  managerSpec $
    waiSpec exampleApplication $ do
      itWithBoth "echos this example" $ \man p -> do
        let body = "hello world"
        req <- (\r -> r {port = fromIntegral p, requestBody = RequestBodyLBS body}) <$> parseRequest "http://localhost"
        resp <- httpLbs req man
        responseBody resp `shouldBe` body
  waiClientSpec exampleApplication $ do
    describe "get" $ do
      it "can GET the root and get a 200" $ do
        resp <- get "/"
        liftIO $ responseStatus resp `shouldBe` ok200
      it "can GET the /redirect and get a 303 with a location header" $ do
        resp <- get "/redirect"
        liftIO $ case lookup "Location" $ responseHeaders resp of
          Nothing -> expectationFailure "should have found a location header"
          Just l -> l `shouldBe` "/"
      it "carries cookies correctly" $ do
        _ <- get "/set-cookie"
        resp <- get "/expects-cookie"
        liftIO $ responseStatus resp `shouldBe` ok200
    describe "post" $
      it "can POST the root and get a 200" $ do
        resp <- post "/" "hello world"
        liftIO $ responseStatus resp `shouldBe` ok200
        liftIO $ responseBody resp `shouldBe` "hello world"
    describe "put" $
      it "can PUT the root and get a 200" $ do
        resp <- put "/" "hello world"
        liftIO $ responseStatus resp `shouldBe` ok200
        liftIO $ responseBody resp `shouldBe` "hello world"
    describe "patch" $
      it "can PATCH the root and get a 200" $ do
        resp <- patch "/" "hello world"
        liftIO $ responseStatus resp `shouldBe` ok200
        liftIO $ responseBody resp `shouldBe` "hello world"
    describe "options" $
      it "can OPTIONS the root and get a 200" $ do
        resp <- options "/"
        liftIO $ responseStatus resp `shouldBe` ok200
    describe "request" $
      it "can make a weird request" $ do
        resp <- request "HELLO" "" [] ""
        liftIO $ responseStatus resp `shouldBe` ok200
    describe "shouldRespondWith" $ do
      it "works with a number" $
        get "/" `shouldRespondWith` 200
      it "works with a string" $
        get "/" `shouldRespondWith` ""
