{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod.Def
  ( yesodSpec,
    yesodSpecWithSiteGenerator,
    yesodSpecWithSiteGeneratorAndArgument,
    yesodSpecWithSiteSupplier,
    yesodSpecWithSiteSupplierWith,
    yesodSpecWithSiteSetupFunc,
    yesodSpecWithSiteSetupFunc',
    yesodClientSetupFunc,
    YesodSpec,
    yit,
    ydescribe,
  )
where

import GHC.Stack (HasCallStack)
import Network.HTTP.Client as HTTP
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Yesod.Client
import Yesod.Core as Yesod

-- | Run a test suite using the given 'site'.
--
-- If your 'site' contains any resources that need to be set up, you probably want to be using one of the following functions instead.
--
-- Example usage with a minimal yesod 'App':
--
-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE OverloadedStrings     #-}
-- > {-# LANGUAGE QuasiQuotes           #-}
-- > {-# LANGUAGE TemplateHaskell       #-}
-- > {-# LANGUAGE TypeFamilies          #-}
-- >
-- > module Minimal where
-- >
-- > import Yesod
-- > import Test.Syd
-- >
-- > data App = App -- | Empty App type
-- >
-- > mkYesod "App" [parseRoutes|
-- >     / HomeR GET
-- > |]
-- >
-- > instance Yesod App
-- >
-- > getHomeR :: Handler Html
-- > getHomeR = "Hello, world!"
-- >
-- > main :: IO ()
-- > main = Yesod.warp 3000 App
-- >
-- > testMain :: IO ()
-- > testMain = sydTest spec
-- >
-- > spec :: Spec
-- > spec = yesodSpec App $ do
-- >   it "returns 200 on the homepage" $ do
-- >     get HomeR
-- >     statusIs 200
--
-- This function exists for backward compatibility with yesod-test.
yesodSpec :: YesodDispatch site => site -> YesodSpec site -> Spec
yesodSpec site = yesodSpecWithSiteGenerator $ pure site

-- | Run a test suite using the given 'site' generator.
--
-- If your 'site' contains any resources that you will want to have set up beforhand, you will probably want to use 'yesodSpecWithSiteGeneratorAndArgument' or 'yesodSpecWithSiteSupplierWith' instead.
--
--
-- Example usage with a yesod 'App' that contains a secret key that is generated at startup but not used during tests:
--
-- > data Key = Key -- The implementation of the actual key is omitted here for brevity.
-- > genKey :: IO Key
-- > genKey = pure Key
-- >
-- > data App = App { appSecretKey :: Key }
-- >
-- > genApp :: IO App
-- > genApp = App <$> genKey
-- >
-- > main :: IO ()
-- > main = sydTest spec
-- >
-- > spec :: Spec
-- > spec = yesodSpecWithSiteGenerator genApp $ do
-- >   it "returns 200 on the homepage" $ do
-- >     get HomeR
-- >     statusIs 200
--
-- This function exists for backward compatibility with yesod-test.
yesodSpecWithSiteGenerator :: YesodDispatch site => IO site -> YesodSpec site -> Spec
yesodSpecWithSiteGenerator siteGen = yesodSpecWithSiteGeneratorAndArgument $ \() -> siteGen

-- | Run a test suite using the given 'site' generator which uses an inner resource.
--
-- If your 'site' contains any resources that you need to set up using a 'withX' function, you will want to use `yesodSpecWithSiteSupplier` instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpecWithSiteGeneratorAndArgument :: YesodDispatch site => (a -> IO site) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteGeneratorAndArgument func = yesodSpecWithSiteSupplierWith $ \f a -> func a >>= f

-- | Using a function that supplies a 'site', run a test suite.
--
-- Example usage with a yesod 'App' that contains an sqlite database connection. See 'sydtest-persistent-sqlite'.
--
-- > import Test.Syd.Persistent.Sqlite
-- >
-- > data App = App { appConnectionPool :: ConnectionPool }
-- >
-- > main :: IO ()
-- > main = sydTest spec
-- >
-- > appSupplier :: (App -> IO r) -> IO r
-- > appSupplier func =
-- >   withConnectionPool myMigration $ \pool ->
-- >     func $ App { appConnectionPool = pool}
-- >
-- > spec :: Spec
-- > spec = yesodSpecWithSiteSupplier appSupplier $ do
-- >   it "returns 200 on the homepage" $ do
-- >     get HomeR
-- >     statusIs 200
yesodSpecWithSiteSupplier :: YesodDispatch site => (forall r. (site -> IO r) -> IO r) -> YesodSpec site -> Spec
yesodSpecWithSiteSupplier func = yesodSpecWithSiteSupplierWith (\f () -> func f)

-- | Using a function that supplies a 'site', based on an inner resource, run a test suite.
yesodSpecWithSiteSupplierWith :: YesodDispatch site => (forall r. (site -> IO r) -> (inner -> IO r)) -> YesodSpec site -> SpecWith inner
yesodSpecWithSiteSupplierWith func = managerSpec . yesodSpecWithSiteSetupFunc' (\_ inner -> SetupFunc $ \takeSite -> func takeSite inner)

-- | Using a function that supplies a 'site', using a 'SetupFunc'
--
-- This function assumed that you've already set up the 'HTTP.Manager' beforehand using something like 'managerSpec'.
yesodSpecWithSiteSetupFunc ::
  YesodDispatch site =>
  (HTTP.Manager -> SetupFunc site) ->
  TestDef (HTTP.Manager ': outers) (YesodClient site) ->
  TestDef (HTTP.Manager ': outers) ()
yesodSpecWithSiteSetupFunc setupFunc = yesodSpecWithSiteSetupFunc' $ \man () -> setupFunc man

-- | Using a function that supplies a 'site', using a 'SetupFunc'.
--
-- This function assumed that you've already set up the 'HTTP.Manager' beforehand using something like 'managerSpec'.
yesodSpecWithSiteSetupFunc' ::
  YesodDispatch site =>
  (HTTP.Manager -> inner -> SetupFunc site) ->
  TestDef (HTTP.Manager ': outers) (YesodClient site) ->
  TestDef (HTTP.Manager ': outers) inner
yesodSpecWithSiteSetupFunc' setupFunc = setupAroundWith' $ \man inner -> do
  site <- setupFunc man inner
  yesodClientSetupFunc man site

yesodClientSetupFunc :: YesodDispatch site => HTTP.Manager -> site -> SetupFunc (YesodClient site)
yesodClientSetupFunc man site = do
  application <- liftIO $ Yesod.toWaiAppPlain site
  p <- applicationSetupFunc application
  let client =
        YesodClient
          { yesodClientManager = man,
            yesodClientSite = site,
            yesodClientSitePort = p
          }
  pure client

-- | For backward compatibility with yesod-test
type YesodSpec site = TestDef '[HTTP.Manager] (YesodClient site)

-- | Define a test in the 'YesodClientM site' monad instead of 'IO'.
yit ::
  forall site e.
  ( HasCallStack,
    IsTest (YesodClient site -> IO e),
    Arg1 (YesodClient site -> IO e) ~ (),
    Arg2 (YesodClient site -> IO e) ~ YesodClient site
  ) =>
  String ->
  YesodClientM site e ->
  YesodSpec site
yit s f = it s ((\cenv -> runYesodClientM cenv f) :: YesodClient site -> IO e)

-- | For compatibility with `yesod-test`
--
-- > ydescribe = describe
ydescribe :: String -> YesodSpec site -> YesodSpec site
ydescribe = describe
