{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.MongoDB
  ( -- * Golden tests
    goldenBSONDocumentFile,
    pureGoldenBSONDocumentFile,

    -- * Integration tests
    mongoSpec,
    mongoConnectionSetupFunc,
    MongoServerHandle (..),
    mongoServerSpec,
    mongoServerSetupFunc,
    mongoServerSetupFunc',
  )
where

import Control.Monad (forM_, void)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bson as Bson
import Data.Bson.Binary
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import Data.Yaml as Yaml
import Database.MongoDB as Mongo
import Network.Socket as Socket
import Network.Socket.Free
import qualified Network.Socket.Wait as Socket
import Path
import Path.IO
import System.Process
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process

-- | Test that the produced 'Bson.Document' is the same as what we find in the given golden file.
goldenBSONDocumentFile :: FilePath -> IO Bson.Document -> GoldenTest Bson.Document
goldenBSONDocumentFile fp produceActualDocument =
  GoldenTest
    { goldenTestRead = do
        ap <- resolveFile' fp
        exists <- doesFileExist ap
        if exists
          then
            Just <$> do
              sb <- SB.readFile (fromAbsFile ap)
              case runGetOrFail getDocument (LB.fromStrict sb) of
                Left (_, _, err) -> expectationFailure err
                Right (_, _, d) -> pure d
          else pure Nothing,
      goldenTestProduce = produceActualDocument,
      goldenTestWrite = \d -> do
        ap <- resolveFile' fp
        ensureDir (parent ap)
        SB.writeFile (fromAbsFile ap) $ LB.toStrict $ runPut $ putDocument d,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then pure Nothing
          else do
            assertion <- stringsNotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow expected)
            pure $ Just (Context assertion (goldenContext fp))
    }

-- | Test that the given 'Bson.Document' is the same as what we find in the given golden file.
pureGoldenBSONDocumentFile :: FilePath -> Bson.Document -> GoldenTest Bson.Document
pureGoldenBSONDocumentFile fp actualDocument = goldenBSONDocumentFile fp $ pure actualDocument

data MongoServerHandle = MongoServerHandle
  { mongoServerHandleProcessHandle :: !ProcessHandle,
    mongoServerHandlePort :: !PortNumber
  }

-- | Provide access to a real 'Mongo.Pipe' for each test.
--
-- Example usage:
--
-- >  mongoSpec $ do
-- >    it "can write and read an example value" $ \pipe -> do
-- >      Mongo.access pipe master "example-database" $ do
-- >        let collection = "example-collection"
-- >            exampleVal = ["hello" =: ("world" :: Text)]
-- >        i <- insert collection exampleVal
-- >        r <- findOne (select ["_id" =: i] collection)
-- >        liftIO $ r `shouldBe` Just (("_id" =: i) : exampleVal)
-- >      pure () :: IO ()
--
-- This function uses 'mongoServerSpec' as well as 'mongoConnectionSetupFunc' to run a mongo server, provide access to it and clean up before the test.
mongoSpec :: TestDefM (MongoServerHandle ': outers) Mongo.Pipe result -> TestDefM outers inner result
mongoSpec = mongoServerSpec . setupAroundWith' (\serverHandle _ -> mongoConnectionSetupFunc serverHandle)

-- | Connect to the given mongo server and clean up beforehand.
mongoConnectionSetupFunc :: MongoServerHandle -> SetupFunc Mongo.Pipe
mongoConnectionSetupFunc MongoServerHandle {..} = do
  let h = Host "127.0.0.1" $ PortNumber mongoServerHandlePort
  pipe <- bracketSetupFunc (Mongo.connect h) Mongo.close
  liftIO $
    Mongo.access pipe master "dummy" $ do
      databases <- Mongo.allDatabases
      forM_ databases $ \database ->
        Mongo.useDb database $ do
          collections <- allCollections
          forM_ collections $ \collection -> do
            void $ Mongo.deleteAll collection [([], [])] -- #types, amirite
  pure pipe

-- | Run a mongo server as an external resource
mongoServerSpec :: TestDefM (MongoServerHandle ': outers) inner result -> TestDefM outers inner result
mongoServerSpec = setupAroundAll mongoServerSetupFunc . sequential -- Must run sequentially because state is shared.

-- | Set up, and clean up after, a mongo server in a temporary directory.
mongoServerSetupFunc :: SetupFunc MongoServerHandle
mongoServerSetupFunc = do
  td <- tempDirSetupFunc "sydtest-hedis"
  mongoServerSetupFunc' td

-- | Set up, and clean up after, a mongo server, in the given directory.
mongoServerSetupFunc' :: Path Abs Dir -> SetupFunc MongoServerHandle
mongoServerSetupFunc' td = do
  pidFile <- resolveFile td "mongo.pid"
  logFile <- resolveFile td "mongo.log"
  dataDir <- resolveDir td "data"
  ensureDir dataDir
  portInt <- liftIO $ do
    (portInt, _socket) <- openFreePort
    Socket.close _socket
    pure portInt
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  let configFileContents =
        Yaml.encode $
          object
            [ "systemLog"
                .= object
                  [ "destination" .= ("file" :: Text),
                    "path" .= fromAbsFile logFile
                  ],
              "net"
                .= object
                  [ "port" .= portInt
                  ],
              "processManagement"
                .= object
                  ["fork" .= False, "pidFilePath" .= fromAbsFile pidFile],
              -- It would be nice to use the in-memory storage engine
              -- but that's only available in mongodb enterprise >=3.2.
              "storage"
                .= object
                  [ "dbPath" .= fromAbsDir dataDir
                  ]
            ]
  configFile <- tempBinaryFileWithContentsSetupFunc "config-file" configFileContents
  let pc =
        ( proc
            "mongod"
            ["--config", fromAbsFile configFile]
        )
          { cwd = Just $ fromAbsDir td
          }
  (_, _, _, ph) <- processSetupFunc pc
  liftIO $ Socket.wait "127.0.0.1" portInt
  pure $
    MongoServerHandle
      { mongoServerHandleProcessHandle = ph,
        mongoServerHandlePort = pn
      }
