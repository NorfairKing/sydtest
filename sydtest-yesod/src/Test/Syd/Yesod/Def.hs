{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod.Def
  ( yesodSpec,
    yesodSpecWithSiteGenerator,
    yesodSpecWithSiteGeneratorAndArgument,
    yesodSpecWithSiteSupplier,
    yesodSpecWithSiteSupplierWith,
    yesodSpecWithSiteSetupFunc,
    yesodClientSetupFunc,
    YesodSpec,
    yit,
    ydescribe,
  )
where

import GHC.Stack
import Network.HTTP.Client as HTTP
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Yesod.Client
import Yesod.Core as Yesod

-- | Run a test suite using the given 'site'.
--
-- If your 'site' contains any resources that need to be set up, you probably want to be using one of the following functions instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpec :: YesodDispatch site => site -> YesodSpec site -> Spec
yesodSpec site = yesodSpecWithSiteGenerator $ pure site

-- | Run a test suite using the given 'site' generator.
--
-- If your 'site' contains any resources that you will want to have set up beforhand, you will probably want to use 'yesodSpecWithSiteGeneratorAndArgument' or 'yesodSpecWithSiteSupplierWith' instead.
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
yesodSpecWithSiteSupplier :: YesodDispatch site => (forall r. (site -> IO r) -> IO r) -> YesodSpec site -> Spec
yesodSpecWithSiteSupplier func = yesodSpecWithSiteSupplierWith (\f () -> func f)

-- | Using a function that supplies a 'site', based on an inner resource, run a test suite.
yesodSpecWithSiteSupplierWith :: YesodDispatch site => (forall r. (site -> IO r) -> (a -> IO r)) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteSupplierWith func = yesodSpecWithSiteSetupFunc $ \_ -> SetupFunc func

-- | Using a function that supplies a 'site', using a 'SetupFunc'
yesodSpecWithSiteSetupFunc :: YesodDispatch site => (HTTP.Manager -> SetupFunc a site) -> TestDef (HTTP.Manager ': l) (YesodClient site) -> TestDef l a
yesodSpecWithSiteSetupFunc setupFunc = managerSpec . setupAroundWith' (\man -> setupFunc man `connectSetupFunc` yesodClientSetupFunc man)

yesodClientSetupFunc :: YesodDispatch site => HTTP.Manager -> SetupFunc site (YesodClient site)
yesodClientSetupFunc man = wrapSetupFunc $ \site -> do
  application <- liftIO $ Yesod.toWaiAppPlain site
  p <- unwrapSetupFunc applicationSetupFunc application
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
yit :: forall site. HasCallStack => String -> YesodClientM site () -> YesodSpec site
yit s f = it s ((\cenv -> runYesodClientM cenv f) :: YesodClient site -> IO ())

-- | For backward compatibility
ydescribe :: String -> YesodSpec site -> YesodSpec site
ydescribe = describe
