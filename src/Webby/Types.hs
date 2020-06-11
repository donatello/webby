{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Webby.Types where

-- We directly depend on unliftio-core's Control.Monad.IO.Unlift, so we can
-- deriving MonadUnliftIO via GeneralizedNewtypeDeriving. If we depend on the
-- exported class from unliftio's UnliftIO module, we have problem building with
-- stack and LTS 16.0. FIXME: fix this unliftio has the right dep.
import qualified Control.Monad.IO.Unlift as Un
import qualified Data.Binary.Builder as Bu
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified UnliftIO as U
import qualified UnliftIO.Concurrent as Conc
import Prelude

-- | A data type to represent parts of the response constructed in the
-- handler when servicing a request.
data WyResp = WyResp
  { wrStatus :: Status,
    wrHeaders :: ResponseHeaders,
    wrRespData :: Either StreamingBody Bu.Builder,
    wrResponded :: Bool
  }

defaultWyResp :: WyResp
defaultWyResp = WyResp status200 [] (Right Bu.empty) False

data WebbyExceptionHandler env = forall e. Exception e => WebbyExceptionHandler (e -> (WebbyM env) ())

-- | The reader environment used by the web framework. It is
-- parameterized by the application's environment data type.
data WEnv env = WEnv
  { weResp :: Conc.MVar WyResp,
    weCaptures :: Captures,
    weRequest :: Request,
    weAppEnv :: env,
    weExceptionHandler :: Maybe (WebbyExceptionHandler env)
  }

-- | The main monad transformer stack used in the web-framework.
--
-- The type of a handler for a request is `WebbyM appEnv ()`. The
-- `appEnv` parameter is used by the web application to store an
-- (read-only) environment. For e.g. it can be used to store a
-- database connection pool.
newtype WebbyM env a = WebbyM
  { unWebbyM :: ReaderT (WEnv env) (ResourceT IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WEnv env), Un.MonadUnliftIO)

runWebbyM :: WEnv w -> WebbyM w a -> IO a
runWebbyM env = runResourceT . flip runReaderT env . unWebbyM

-- | A route pattern represents logic to match a request to a handler.
data RoutePattern = RoutePattern Method [Text]
  deriving (Eq, Show)

-- | A route is a pair of a route pattern and a handler.
type Route env = (RoutePattern, WebbyM env ())

-- | Captures are simply extracted path elements in a HashMap
type Captures = H.HashMap Text Text

-- | Internal type used to terminate handler processing by throwing and
-- catching an exception.
data FinishThrown = FinishThrown
  deriving (Show)

instance U.Exception FinishThrown

-- | Various kinds of errors thrown by this library - these can be
-- caught by handler code.
data WebbyError
  = WebbyJSONParseError Text
  | WebbyParamParseError
      { wppeParamName :: Text,
        wppeErrMsg :: Text
      }
  | WebbyMissingCapture Text
  deriving (Show)

instance U.Exception WebbyError where
  displayException (WebbyParamParseError pName msg) =
    T.unpack $ sformat ("Param parse error: " % st % " " % st) pName msg
  displayException (WebbyJSONParseError _) = "Invalid JSON body"
  displayException (WebbyMissingCapture capName) =
    T.unpack $ sformat (st % " missing") capName

data WebbyServerConfig env = WebbyServerConfig
  { wscRoutes :: [Route env],
    wscExceptionHandler :: Maybe (WebbyExceptionHandler env)
  }

defaultWebbyServerConfig :: WebbyServerConfig env
defaultWebbyServerConfig =
  WebbyServerConfig
    { wscRoutes = [],
      wscExceptionHandler = Nothing :: Maybe (WebbyExceptionHandler env)
    }

setRoutes ::
  [Route env] ->
  WebbyServerConfig env ->
  WebbyServerConfig env
setRoutes routes wsc =
  wsc
    { wscRoutes = routes
    }

setExceptionHandler ::
  Exception e =>
  (e -> WebbyM env ()) ->
  WebbyServerConfig env ->
  WebbyServerConfig env
setExceptionHandler exceptionHandler wsc =
  WebbyServerConfig
    { wscRoutes = wscRoutes wsc,
      wscExceptionHandler = Just $ WebbyExceptionHandler exceptionHandler
    }
