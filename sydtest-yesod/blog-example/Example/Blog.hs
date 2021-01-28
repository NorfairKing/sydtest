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

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer
      [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <meta name="description" content="my awesome site">
                    <meta name="author" content="Patrick Brisbin">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Fruit = Apple | Orange | Pear deriving (Eq, Ord, Read, Show)

data TheForm = TheForm
  { formText :: Text,
    formInt :: Int,
    formFruit :: Fruit
  }

theForm :: Form TheForm
theForm =
  renderDivs $
    TheForm
      <$> areq textField "Some text" Nothing
      <*> areq intField "Some number" Nothing
      <*> areq selectFruit "Some fruit" Nothing
  where
    selectFruit =
      selectField $
        return $
          mkOptionList
            [ Option "Apple" Apple "apple",
              Option "Orange" Orange "orange",
              Option "Pear" Pear "pear"
            ]

getHomeR :: Handler Html
getHomeR = do
  ((res, form), enctype) <- runFormPost theForm
  defaultLayout $ do
    setTitle "My title"

    case res of
      FormSuccess f ->
        [whamlet|
                                <p>You've posted a form!
                                <p>the text was #{formText f}
                                <p>the number was #{formInt f}
                                <p>the fruit was #{show $ formFruit f}
                                |]
      _ ->
        [whamlet|
                    <p>Hello world!
                    <form enctype="#{enctype}" method="post">
                        ^{form}
                        <input type="submit">
                    |]

postHomeR :: Handler Html
postHomeR = getHomeR

main :: IO ()
main = Yesod.warp 3000 App
