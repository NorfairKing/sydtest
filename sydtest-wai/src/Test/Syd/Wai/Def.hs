{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Wai.Def where

import Network.HTTP.Client as HTTP
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Test.Syd

-- | Run a given 'Wai.Application' around every test.
--
-- This provides the port on which the application is running.
waiSpec :: Wai.Application -> TestDef l Port -> TestDef l ()
waiSpec application = waiSpecWithSetupFunc $ pure application

-- | Run a 'Wai.Application' around every test by setting it up with the given setup function.
--
-- This provides the port on which the application is running.
waiSpecWith :: (forall r. (Application -> IO r) -> IO r) -> TestDef l Port -> TestDef l ()
waiSpecWith appFunc = waiSpecWithSetupFunc $ makeSimpleSetupFunc appFunc

-- | Run a 'Wai.Application' around every test by setting it up with the given setup function that can take an argument.
-- a
-- This provides the port on which the application is running.
waiSpecWith' :: (forall r. (Application -> IO r) -> (a -> IO r)) -> TestDef l Port -> TestDef l a
waiSpecWith' appFunc = waiSpecWithSetupFunc $ SetupFunc appFunc

-- | Run a 'Wai.Application' around every test by setting it up with the given 'SetupFunc'.
-- a
-- This provides the port on which the application is running.
waiSpecWithSetupFunc :: SetupFunc a Application -> TestDef l Port -> TestDef l a
waiSpecWithSetupFunc setupFunc = setupAroundWith (setupFunc `connectSetupFunc` applicationSetupFunc)

-- | A 'SetupFunc' to run an application and provide its port.
applicationSetupFunc :: SetupFunc Application Port
applicationSetupFunc = SetupFunc $ \func application ->
  Warp.testWithApplication (pure application) $ \p ->
    func p

-- | Create a 'HTTP.Manager' before all tests in the given group.
managerSpec :: TestDef (HTTP.Manager ': l) a -> TestDef l a
managerSpec = beforeAll $ HTTP.newManager HTTP.defaultManagerSettings
