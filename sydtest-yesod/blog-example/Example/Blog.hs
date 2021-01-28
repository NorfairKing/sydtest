{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.Blog where

import Data.Text (Text)
import Yesod

data App = App

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET POST
|]

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data NewThought = NewThought
  { newThoughtTitle :: Text,
    newThoughtContents :: Text
  }

newThoughtForm :: Form NewThought
newThoughtForm =
  renderDivs $
    NewThought
      <$> areq textField ("Title" {fsName = Just "title"}) Nothing
      <*> (unTextarea <$> areq textareaField ("Contents" {fsName = Just "contents"}) Nothing)

getHomeR :: Handler Html
getHomeR = do
  ((res, form), enctype) <- runFormPost newThoughtForm
  case res of
    FormSuccess f ->
      defaultLayout
        [whamlet|
          <p>You've posted a thought!
          <p>the title was #{newThoughtTitle f}
          <p>the contents were #{Textarea $ newThoughtContents f}
        |]
    FormMissing ->
      defaultLayout
        [whamlet|
          <p>Hello world!
          <form enctype="#{enctype}" method="post">
            ^{form}
            <input type="submit">
              Submit thought
        |]
    FormFailure errs -> invalidArgs errs

postHomeR :: Handler Html
postHomeR = getHomeR

main :: IO ()
main = Yesod.warp 3000 App
