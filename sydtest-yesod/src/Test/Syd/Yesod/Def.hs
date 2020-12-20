{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod.Def where

import GHC.Stack
import Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp as Warp
import Test.Syd
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
yesodSpecWithSiteSupplier :: YesodDispatch site => ((site -> IO ()) -> IO ()) -> YesodSpec site -> Spec
yesodSpecWithSiteSupplier func = yesodSpecWithSiteSupplierWith (\f () -> func f)

-- | Using a function that supplies a 'site', based on an inner resource, run a test suite.
yesodSpecWithSiteSupplierWith :: YesodDispatch site => ((site -> IO ()) -> (a -> IO ())) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteSupplierWith func = aroundWith func . beforeAll (newManager defaultManagerSettings) . aroundWith' yesodSpecWithFunc

-- | Turn a function that takes a 'YesodClient site' into a function that only takes a 'site'.
yesodSpecWithFunc :: YesodDispatch site => (HTTP.Manager -> YesodClient site -> IO ()) -> (HTTP.Manager -> site -> IO ())
yesodSpecWithFunc func man = unSetupFunc (yesodSetupFunc man) (func man)

yesodSetupFunc :: YesodDispatch site => HTTP.Manager -> SetupFunc site (YesodClient site)
yesodSetupFunc man = SetupFunc $ \takeClient site ->
  Warp.testWithApplication (Yesod.toWaiAppPlain site) $ \p ->
    let client =
          YesodClient
            { yesodClientManager = man,
              yesodClientSite = site,
              yesodClientSitePort = p
            }
     in takeClient client

-- | For backward compatibility with yesod-test
type YesodSpec site = TestDefM '[HTTP.Manager] (YesodClient site) ()

-- | Define a test in the 'YesodClientM site' monad instead of 'IO'.
yit :: forall site. HasCallStack => String -> YesodClientM site () -> YesodSpec site
yit s f = it s ((\cenv -> runYesodClientM cenv f) :: YesodClient site -> IO ())

-- | For backward compatibility
ydescribe :: String -> YesodSpec site -> YesodSpec site
ydescribe = describe
