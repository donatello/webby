-- |
-- Module      : Webby
-- Description : An easy to use Haskell web-server inspired by Scotty.
-- License     : Apache License 2.0
-- Maintainer  : aditya.mmy@gmail.com
module Webby
  ( WebbyM,

    -- * Routing and handler functions
    RoutePattern,
    Route,
    mkRoute,
    post,
    get,
    put,
    delete,

    -- * Captures
    Captures,
    captures,
    getCapture,

    -- * Request parsing
    flag,
    header,
    headers,
    jsonData,
    param,
    param_,
    params,
    request,
    requestBodyLBS,
    requestBodyLength,
    getRequestBodyChunkAction,

    -- * Response modification
    setStatus,
    addHeader,
    setHeader,
    blob,
    json,
    text,
    stream,

    -- * Application
    mkWebbyApp,
    Application,

    -- * Webby server configuration
    WebbyServerConfig,
    defaultWebbyServerConfig,
    setRoutes,
    setExceptionHandler,

    -- * Handler flow control
    finish,

    -- * Exceptions thrown
    WebbyError (..),
  )
where

import           Network.Wai  (Application)
import           Webby.Server
import           Webby.Types
