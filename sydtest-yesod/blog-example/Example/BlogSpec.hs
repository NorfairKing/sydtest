{-# LANGUAGE OverloadedStrings #-}

module Example.BlogSpec (spec) where

-- import Data.Text (Text)
import qualified Data.Text as T
import Example.Blog
import Test.QuickCheck
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = yesodSpec App $ do
  yit "can GET the home page" $ do
    get HomeR
    statusIs 200
  yit "cannot post if the form data is missing" $ do
    get HomeR
    statusIs 200
    request $ do
      setMethod "POST"
      setUrl HomeR
      addToken
    statusIs 400
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
  -- We need to use nonempty title and contents so they don't look missing to the server.
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
