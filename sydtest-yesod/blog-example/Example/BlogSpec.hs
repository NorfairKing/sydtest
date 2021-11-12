{-# LANGUAGE OverloadedStrings #-}

module Example.BlogSpec (spec) where

import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist (selectList)
import Database.Persist.Sql (Entity (..), SqlPersistT)
import Example.Blog
import Network.HTTP.Client as HTTP
import Path
import Path.IO
import Test.QuickCheck
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent.Sqlite
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod

appSetupFunc :: HTTP.Manager -> SetupFunc App
appSetupFunc _ = do
  tdir <- tempDirSetupFunc "sydtest-yesod"
  sessionKeyFile <- resolveFile tdir "client_session_key.aes"
  pool <- connectionPoolSetupFunc migrateThoughts
  pure $
    App
      { appSessionKeyFile = fromAbsFile sessionKeyFile,
        appConnectionPool = pool
      }

testDB :: SqlPersistT IO a -> YesodClientM App a
testDB func = do
  pool <- asks $ appConnectionPool . yesodClientSite
  liftIO $ runSqlPool func pool

spec :: Spec
spec = managerSpec $
  yesodSpecWithSiteSetupFunc appSetupFunc $ do
    -- A simple read-only test: We can request the home page succesfully.
    it "can GET the home page" $ do
      get HomeR
      statusIs 200

    -- A simple test that shows a post request, and shows that empty forms will not be accepted.
    it "cannot post if the form data is missing" $ do
      get NewThoughtR
      statusIs 200
      request $ do
        setMethod "POST"
        setUrl NewThoughtR
        addToken -- For the CSRF protection
      statusIs 400

    -- A simple form POST request test.
    -- Each part of the form needs to be added using 'addPostParam'
    -- Don't forget the 'addToken' piece to make sure you don't run into CSRF errors.
    it "can post this example blogpost" $ do
      get NewThoughtR
      statusIs 200
      request $ do
        setMethod "POST"
        setUrl NewThoughtR
        addToken
        addPostParam "title" "example title"
        addPostParam "contents" "example contents"
      statusIs 200

    -- A property test that does the same as the simple POST request test above,
    -- Note that ut for _any_ title and contents instead of specific above.
    -- Note that we need to use nonempty title and contents so they don't look missing to the server.
    it "can post any blogpost" $ \yc ->
      forAll (arbitrary `suchThat` (not . null)) $ \title ->
        forAll (arbitrary `suchThat` (not . null)) $ \contents ->
          runYesodClientM yc $ do
            get NewThoughtR
            statusIs 200
            request $ do
              setMethod "POST"
              setUrl NewThoughtR
              addToken
              addPostParam "title" (T.pack title)
              addPostParam "contents" (T.pack contents)
            statusIs 200

    -- A proprety test that does the above, but also goes and finds the thought that was posted,
    -- both on the page and in the database.
    -- We generate alphanumeric titles and contents just so that we don't have to depend on `genvalidity` here and figure out encodings correctly.
    it "can post any blogpost and then look it up" $ \yc ->
      forAll (listOf (choose ('a', 'z')) `suchThat` (not . null)) $ \title ->
        forAll (listOf (choose ('a', 'z')) `suchThat` (not . null)) $ \contents ->
          runYesodClientM yc $ do
            let titleText = T.pack title
            let contentsText = T.pack contents
            get NewThoughtR
            statusIs 200
            request $ do
              setMethod "POST"
              setUrl NewThoughtR
              addToken
              addPostParam "title" titleText
              addPostParam "contents" contentsText
            statusIs 200
            thoughts <- testDB $ selectList [] []
            Entity thoughtId thought <- case thoughts of
              [] -> liftIO $ expectationFailure "Expected to find at least one thought, found none."
              [e] -> pure e
              _ -> liftIO $ expectationFailure "Found more than one thought."
            liftIO $ do
              thoughtTitle thought `shouldBe` titleText
              thoughtContents thought `shouldBe` contentsText
            get $ ThoughtR thoughtId
            bodyContains $ T.unpack titleText
            bodyContains $ T.unpack contentsText
            statusIs 200
