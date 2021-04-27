{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Test.Syd.Wai
  ( -- * Functions to run a test suite

    -- ** A test suite that uses a running wai applications
    waiSpec,
    waiSpecWith,
    waiSpecWith',
    waiSpecWithSetupFunc,

    -- ** A test suite that uses a running wai application and calls it using the functions provided in this package
    waiClientSpec,
    waiClientSpecWithSetupFunc,
    waiClientSpecWithSetupFunc',

    -- ** A test suite that uses a single HTTP manager accross tests
    managerSpec,

    -- *** Setup functions
    waiClientSetupFunc,
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
    performRequest,

    -- ** Assertions
    ResponseMatcher (..),
    MatchHeader (..),
    MatchBody (..),
    shouldRespondWith,
    Body,
    (<:>),

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
import Test.Syd.Wai.Matcher
import Test.Syd.Wai.Request
