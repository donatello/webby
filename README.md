# Webby

An easy to use Haskell web-server inspired by Scotty.

# Build

Clone the repo and run `stack build`

# Example

``` haskell
module Main where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai              as W
import           Network.HTTP.Types       (status500)

import           UnliftIO                 (liftIO)
import qualified UnliftIO.Exception       as E

import           Relude                   hiding (get, put)
import           Relude.Print             (putTextLn)
import qualified Data.Text                as T

import           Webby

-- An example exception handler web-applications can install with webby
appExceptionHandler :: T.Text -> (E.SomeException -> WebbyM appEnv ())
appExceptionHandler appName = \(exception :: E.SomeException) -> do
    setStatus status500
    let msg = appName <> " failed with " <> show exception
    putTextLn msg
    text msg

main :: IO ()
main = do
    -- Define the API routes handled by your web-application
    let routes = [ get "/api/a" (text "a\n")
                 , get "/api/b" (text "b\n")
                 , post "/api/capture/:id"
                   (do idVal :: Int <- getCapture "id"
                       text $ (T.pack (show idVal) `T.append` "\n")
                   )
                 , get "/api/isOdd/:val"
                   (do val :: Integer <- getCapture "val"
                       -- if val is odd return the number else we short-circuit the handler
                       unless (odd val) finish
                       text $ show val
                    )
                 , get "/api/showEnv" (do env <- getAppEnv
                                          text env
                                      )
                 , get "/aaah" (liftIO $ E.throwString "oops!")
                 ]

        -- Set the routes definition and exception handler for your
        -- web-application
        webbyConfig = setExceptionHandler (appExceptionHandler "MyApp") $
                      setRoutes routes $
                      defaultWebbyServerConfig

        -- Application environment in this example is a simple Text literal.
        -- Usually, application environment would contain database connections
        -- etc.
        appEnv = "MyEnv" :: T.Text

    webbyApp <- mkWebbyApp appEnv webbyConfig
    putStrLn "Starting webserver..."
    W.runEnv 7000 webbyApp
```

You can try the example above, by cloning the repo and running the
example:

``` shell
$ examples/Basic.hs
```

In another shell, let's `curl` the server:

``` shell
$ curl http://localhost:7000/api/a
a
$ curl http://localhost:7000/api/b
b
$ curl -XPOST http://localhost:7000/api/capture/32
32
$ curl http://localhost:7000/api/showEnv
MyEnv
$ curl http://localhost:7000/aaah
MyApp failed with Control.Exception.Safe.throwString called with:

oops!
Called from:
  throwString (examples/Basic.hs:55:42 in main:Main)
```
