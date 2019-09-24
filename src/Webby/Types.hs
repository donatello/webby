{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Webby.Types where

import qualified Control.Monad.Logger  as Log
import qualified Data.Binary.Builder   as Bu
import qualified Data.HashMap.Strict   as H
import qualified Data.Text             as T
import           System.Log.FastLogger (FormattedTime)
import qualified System.Log.FastLogger as FLog
import qualified UnliftIO              as U
import qualified UnliftIO.Concurrent   as Conc

import           WebbyPrelude

-- | A data type to represent parts of the response constructed in the
-- handler when servicing a request.
data WyResp = WyResp { wrStatus    :: Status
                     , wrHeaders   :: ResponseHeaders
                     , wrRespData  :: Either StreamingBody Bu.Builder
                     , wrResponded :: Bool
                     }

defaultWyResp :: WyResp
defaultWyResp = WyResp status200 [] (Right Bu.empty) False

-- | The reader environment used by the web framework. It is
-- parameterized by the application's environment data type.
data WEnv appEnv = WEnv { weResp      :: Conc.MVar WyResp
                        , weCaptures  :: Captures
                        , weRequest   :: Request
                        , weAppEnv    :: appEnv
                        , weLoggerSet :: FLog.LoggerSet
                        , weLogTime   :: IO FormattedTime
                        }

-- | The main monad transformer stack used in the web-framework.
--
-- The type of a handler for a request is `WebbyM appEnv ()`. The
-- `appEnv` parameter is used by the web application to store an
-- (read-only) environment. For e.g. it can be used to store a
-- database connection pool.
newtype WebbyM appEnv a = WebbyM
    { unWebbyM :: ReaderT (WEnv appEnv) (ResourceT IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WEnv appEnv))

instance U.MonadUnliftIO (WebbyM appData) where
    askUnliftIO = WebbyM $ ReaderT $
                  \(w :: WEnv appData) -> U.withUnliftIO $
                  \u -> return $
                  U.UnliftIO (U.unliftIO u . flip runReaderT w . unWebbyM)

instance Log.MonadLogger (WebbyM env) where
    monadLoggerLog loc src lvl msg = do
        lset <- asks weLoggerSet
        ltime <- asks weLogTime
        tstr <- liftIO ltime
        let logstr = Log.toLogStr tstr <>
                     (Log.defaultLogStr loc src lvl $ Log.toLogStr msg)
        liftIO $ FLog.pushLogStr lset logstr

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
data WebbyError = WebbyJSONParseError Text
                | WebbyParamParseError { wppeParamName :: Text
                                       , wppeErrMsg    :: Text
                                       }
                | WebbyMissingCapture Text
                deriving Show

instance U.Exception WebbyError where
    displayException (WebbyParamParseError pName msg) =
        T.unpack $ sformat ("Param parse error: " % st % " " % st) pName msg

    displayException (WebbyJSONParseError _) = "Invalid JSON body"

    displayException (WebbyMissingCapture capName) =
        T.unpack $ sformat (st % " missing") capName
