{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Text as T
import Network.HTTP.Types (status500)
import qualified Network.Wai.Handler.Warp as W
import Relude hiding (get, put)
import qualified UnliftIO.Exception as E
import Webby

-- An example exception handler web-applications can install with webby
appExceptionHandler :: T.Text -> (E.SomeException -> WebbyM appEnv ())
appExceptionHandler appName (exception :: E.SomeException) = do
  setStatus status500
  let msg = appName <> " failed with " <> show exception
  putTextLn msg
  text msg

newtype AppEnv = AppEnv Text
  deriving (Eq, Show)

-- To demonstrate that appEnv is avaliable via MonadReader interface and no
-- additional boiler-plate is required
class HasAppName env where
  getAppName :: env -> Text

instance HasAppName AppEnv where
  getAppName (AppEnv name) = name

fetchAppName :: (HasAppName env, MonadReader env m, MonadIO m) => m Text
fetchAppName = do
  asks getAppName

main :: IO ()
main = do
  -- Define the API routes handled by your web-application
  let routes =
        [ get "/api/a" (text "a\n"),
          get "/api/b" (text "b\n"),
          post
            "/api/capture/:id"
            ( do
                idVal :: Int <- getCapture "id"
                text (T.pack (show idVal) `T.append` "\n")
            ),
          get
            "/api/isOdd/:val"
            ( do
                val :: Integer <- getCapture "val"
                -- if val is odd return the number else we short-circuit the handler
                unless (odd val) finish
                text $ show val
            ),
          get
            "/api/showAppName"
            ( do
                appName <- fetchAppName
                text appName
            ),
          get "/aaah" (liftIO $ E.throwString "oops!")
        ]
      -- Set the routes definition and exception handler for your
      -- web-application
      webbyConfig =
        setExceptionHandler (appExceptionHandler "MyApp") $
          setRoutes routes
            defaultWebbyServerConfig
      -- Application environment in this example is a simple Text literal.
      -- Usually, application environment would contain database connections
      -- etc.
      appEnv = AppEnv "test-webby"

  webbyApp <- mkWebbyApp appEnv webbyConfig
  putStrLn "Starting webserver..."
  W.runEnv 7000 webbyApp
