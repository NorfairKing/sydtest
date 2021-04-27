module Test.Syd.Wai
  ( -- * Functions to run a test suite
    waiSpec,
    waiSpecWith,
    waiSpecWith',
    waiSpecWithSetupFunc,
    managerSpec,

    -- *** Setup functions
    applicationSetupFunc,

    -- ** Core
    WaiClient (..),
    WaiClientState (..),
    WaiClientM (..),
    runWaiClientM,

    -- * Making requests
    get,
    post,
    put,
    patch,
    options,
    delete,
    request,

    -- * Just to make sure we didn't forget any exports
    module Test.Syd.Wai.Client,
    module Test.Syd.Wai.Def,
    module Test.Syd.Wai.Request,

    -- * Reexports
    module HTTP,
  )
where

import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Test.Syd.Wai.Client
import Test.Syd.Wai.Def
import Test.Syd.Wai.Request
