{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Because of webdriver using dangerous constructors
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
-- For the undefined trick
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Test.Syd.Webdriver where

import Codec.Picture as Picture
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import GHC.Stack
import Network.HTTP.Client as HTTP
import Network.Socket
import Network.Socket.Free
import Network.Socket.Wait as Port
import Network.URI
import Path
import Path.IO
import System.Exit
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process.Typed
import Test.Syd.Wai
import Test.WebDriver as WD hiding (setWindowSize)
import Test.WebDriver.Class (WebDriver (..))
import qualified Test.WebDriver.Commands.Internal as WD
import qualified Test.WebDriver.JSON as WD
import Test.WebDriver.Session (WDSessionState (..))

-- | A monad for webdriver tests.
-- This instantiates the 'WebDriver' class, as well as the 'IsTest' class.
newtype WebdriverTestM app a = WebdriverTestM
  { unWebdriverTestM :: ReaderT (WebdriverTestEnv app) WD a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (WebdriverTestEnv app),
      -- We don't want 'MonadBaseControl IO' or 'MonadBase IO', but we have to
      -- because webdriver uses them.
      MonadBaseControl IO,
      MonadBase IO
    )

data WebdriverTestEnv app = WebdriverTestEnv
  { -- | The base url of the app we test, so that we can test external sites just like local ones.
    webdriverTestEnvURI :: !URI,
    -- | The webdriver configuration
    webdriverTestEnvConfig :: !WDConfig,
    -- | The app that we'll test.
    --
    -- You can put any piece of data here. In the case of yesod tests, we'll put an @App@ here.
    webdriverTestEnvApp :: !app
  }

instance WDSessionState (WebdriverTestM app) where
  getSession = WebdriverTestM getSession
  putSession = WebdriverTestM . putSession

instance WebDriver (WebdriverTestM app) where
  doCommand m p a = WebdriverTestM $ doCommand m p a

instance IsTest (WebdriverTestM app ()) where
  type Arg1 (WebdriverTestM app ()) = ()
  type Arg2 (WebdriverTestM app ()) = WebdriverTestEnv app
  runTest wdTestFunc = runTest (\() wdte -> runWebdriverTestM wdte wdTestFunc)

instance IsTest (WebdriverTestM app (GoldenTest a)) where
  type Arg1 (WebdriverTestM app (GoldenTest a)) = ()
  type Arg2 (WebdriverTestM app (GoldenTest a)) = WebdriverTestEnv app
  runTest wdTestFunc = runTest (\() wdte -> runWebdriverTestM wdte wdTestFunc)

-- | Run a webdriver test.
runWebdriverTestM :: WebdriverTestEnv app -> WebdriverTestM app a -> IO a
runWebdriverTestM env (WebdriverTestM func) = WD.runSession (webdriverTestEnvConfig env) $
  WD.finallyClose $ do
    setImplicitWait 10_000
    setScriptTimeout 10_000
    setPageLoadTimeout 10_000
    runReaderT func env

openPath :: String -> WebdriverTestM app ()
openPath p = do
  uri <- asks webdriverTestEnvURI
  let url = show uri <> p
  openPage url

type WebdriverSpec app = TestDef '[SeleniumServerHandle, HTTP.Manager] (WebdriverTestEnv app)

webdriverSpec ::
  (HTTP.Manager -> SetupFunc (URI, app)) ->
  WebdriverSpec app ->
  Spec
webdriverSpec appSetupFunc =
  managerSpec
    . modifyMaxSuccess (`div` 50)
    . setupAroundWith' (\man () -> appSetupFunc man)
    . setupAroundAll seleniumServerSetupFunc
    . webdriverTestEnvSpec

webdriverTestEnvSpec ::
  TestDef '[SeleniumServerHandle, HTTP.Manager] (WebdriverTestEnv app) ->
  TestDef '[SeleniumServerHandle, HTTP.Manager] (URI, app)
webdriverTestEnvSpec = setupAroundWith' go2 . setupAroundWith' go1
  where
    go1 ::
      SeleniumServerHandle ->
      (SeleniumServerHandle -> SetupFunc (WebdriverTestEnv app)) ->
      SetupFunc (WebdriverTestEnv app)
    go1 ssh func = func ssh
    go2 ::
      HTTP.Manager ->
      (URI, app) ->
      SetupFunc (SeleniumServerHandle -> SetupFunc (WebdriverTestEnv app))
    go2 man (uri, app) = pure $ \ssh -> webdriverTestEnvSetupFunc ssh man uri app

webdriverTestEnvSetupFunc ::
  SeleniumServerHandle ->
  HTTP.Manager ->
  URI ->
  app ->
  SetupFunc (WebdriverTestEnv app)
webdriverTestEnvSetupFunc SeleniumServerHandle {..} manager uri app = do
  chromeExecutable <- liftIO $ do
    chromeFile <- parseRelFile "chromium"
    mExecutable <- findExecutable chromeFile
    case mExecutable of
      Nothing -> die "No chromium found on PATH."
      Just executable -> pure executable

  userDataDir <- tempDirSetupFunc "chromium-user-data"

  let browser =
        chrome
          { chromeOptions =
              [ "--user-data-dir=" <> fromAbsDir userDataDir,
                "--headless",
                "--no-sandbox", -- Bypass OS security model to run on nix as well
                "--disable-dev-shm-usage", -- Overcome limited resource problem
                "--disable-gpu",
                "--use-gl=angle",
                "--use-angle=swiftshader",
                "--window-size=1920,1080"
              ],
            chromeBinary = Just $ fromAbsFile chromeExecutable
          }
  let caps =
        WD.defaultCaps
          { browser = browser
          }
  let webdriverTestEnvConfig =
        WD.defaultConfig
          { wdPort = (fromIntegral :: PortNumber -> Int) seleniumServerHandlePort,
            wdHTTPManager = Just manager,
            wdCapabilities = caps
          }
  let webdriverTestEnvURI = uri
      webdriverTestEnvApp = app
  pure WebdriverTestEnv {..}

seleniumServerSetupFunc :: SetupFunc SeleniumServerHandle
seleniumServerSetupFunc = do
  tempDir <- tempDirSetupFunc "selenium-server"
  portInt <- liftIO getFreePort
  let processConfig =
        setStdout nullStream $
          setStderr nullStream $
            setWorkingDir (fromAbsDir tempDir) $
              proc
                "selenium-server"
                [ "-port",
                  show portInt
                ]
  _ <- typedProcessSetupFunc processConfig
  liftIO $ Port.wait "127.0.0.1" portInt
  let seleniumServerHandlePort = fromIntegral portInt
  pure SeleniumServerHandle {..}

data SeleniumServerHandle = SeleniumServerHandle
  { seleniumServerHandlePort :: PortNumber
  }

data Screenshot = Screenshot
  { screenshotFile :: !(Path Abs File),
    screenshotImage :: !(Picture.Image PixelRGB8)
  }

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
        let sb = LB.toStrict contents
        case decodePng sb of
          Left err -> expectationFailure $ "Could not parse screenshot as png: " <> err
          Right dynamicImage -> do
            let image = convertRGB8 dynamicImage
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
          then Nothing
          else
            Just $
              ExpectationFailed $
                unlines
                  [ "Screenshots differ.",
                    "expected: " <> fromAbsFile expectedPath,
                    "actual: " <> fromAbsFile actualPath
                  ]
    }

-- We have to override this because it returns something.
-- So we remove the 'noReturn'.
setWindowSize :: (HasCallStack, WebDriver wd) => (Word, Word) -> wd ()
setWindowSize (w, h) =
  WD.ignoreReturn $
    WD.doWinCommand methodPost currentWindow "/size" $
      object ["width" .= w, "height" .= h]
