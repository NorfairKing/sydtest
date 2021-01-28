{-# LANGUAGE OverloadedStrings #-}

module Example.BlogSpec (spec) where

import qualified Data.Text as T
import Example.Blog
import Network.HTTP.Client as HTTP
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Yesod

-- | A 'SetupFunc' to provide the App and clean up around it.
--
-- The 'HTTP.Manager' here is the one that is used for doing the test requests.
-- It is setup only once and it is shared accross tests for performance reasons.
-- If you also need an 'HTTP.Manager' in your 'App', then you can use it in your 'SetupFunc' here
-- but in this case we don't need it.
appSetupFunc :: HTTP.Manager -> SetupFunc () App
appSetupFunc _ = do
  pool <- connectionPoolSetupFunc migrateThoughts
  pure $ App {appConnectionPool = pool}

spec :: Spec
spec = yesodSpecWithSiteSetupFunc appSetupFunc $ do
  -- A simple read-only test: We can request the home page succesfully.
  yit "can GET the home page" $ do
    get HomeR
    statusIs 200

  -- A simple test that shows a post request, and shows that empty forms will not be accepted.
  yit "cannot post if the form data is missing" $ do
    get HomeR
    statusIs 200
    request $ do
      setMethod "POST"
      setUrl HomeR
      addToken
    statusIs 400

  -- A simple form POST request test.
  -- Each part of the form needs to be added using 'addPostParam'
  -- Don't forget the 'addToken' piece to make sure you don't run into XSRF errors.
  yit "can post this example blogpost" $ do
    get HomeR
    statusIs 200
    request $ do
      setMethod "POST"
      setUrl HomeR
      addToken
      addPostParam "title" "example title"
      addPostParam "contents" "example contents"
    statusIs 200

  -- A property test that does the same as the simple POST request test above,
  -- Njte that ut for _any_ title and contents instead of specific above.
  -- Note that we need to use nonempty title and contents so they don't look missing to the server.
  it "can post any blogpost" $ \yc ->
    forAll (arbitrary `suchThat` (not . null)) $ \title ->
      forAll (arbitrary `suchThat` (not . null)) $ \contents ->
        runYesodClientM yc $ do
          get HomeR
          statusIs 200
          request $ do
            setMethod "POST"
            setUrl HomeR
            addToken
            addPostParam "title" (T.pack title)
            addPostParam "contents" (T.pack contents)
          statusIs 200
