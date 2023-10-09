{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- This is an example of a yesod application that uses a database.
-- It is supposed to represent a blog with blog posts.
-- However, we call the blog posts thought so that it's not too confusing with 'POST' also being the HTTP method.
module Example.Blog where

import Control.Monad.Logger
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "migrateThoughts"]
  [persistLowerCase|
Thought
    title Text
    contents Text
    deriving Show Eq
|]

data App = App
  { appConnectionPool :: !ConnectionPool,
    appSessionKeyFile :: !FilePath
  }

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET
    /new-thought NewThoughtR GET POST
    /thought/#ThoughtId ThoughtR GET
|]

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance Yesod App where
  makeSessionBackend App {..} = Just <$> defaultClientSessionBackend 30 appSessionKeyFile

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB func = do
    pool <- getsYesod appConnectionPool
    runSqlPool func pool

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout "Welcome!, feel free to post some thoughts at /new-thought."

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

getNewThoughtR :: Handler Html
getNewThoughtR = do
  ((res, form), enctype) <- runFormPost newThoughtForm
  case res of
    FormSuccess f -> do
      thoughtId <-
        runDB $
          insert $
            Thought
              { thoughtTitle = newThoughtTitle f,
                thoughtContents = newThoughtContents f
              }
      defaultLayout
        [whamlet|
          <p>You've posted a thought!
          <p>the title was #{newThoughtTitle f}
          <p>the contents were #{newThoughtContents f}
          <a href=@{ThoughtR thoughtId}>
            Look at your thought
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

postNewThoughtR :: Handler Html
postNewThoughtR = getNewThoughtR

getThoughtR :: ThoughtId -> Handler Html
getThoughtR thoughtId = do
  thought <- runDB $ get404 thoughtId
  defaultLayout
    [whamlet|
      <h1> #{thoughtTitle thought}
      <p>
        #{thoughtContents thought}
    |]

main :: IO ()
main = runStderrLoggingT $
  withSqlitePool "example.sqlite3" 1 $ \pool -> do
    runSqlPool (runMigration migrateThoughts) pool
    liftIO $
      Yesod.warp 3000 $
        App
          { appConnectionPool = pool,
            appSessionKeyFile = "client_session_key.aes"
          }
