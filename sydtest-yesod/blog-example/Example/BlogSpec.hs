{-# LANGUAGE OverloadedStrings #-}

module Example.BlogSpec (spec) where

import Example.Blog
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
