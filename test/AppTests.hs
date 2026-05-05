{-# LANGUAGE OverloadedStrings #-}

module AppTests (appTests) where

import qualified Data.Aeson as A
import qualified Network.Wai.Test as WT
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified UnliftIO.Exception as E
import Webby
import Prelude

newtype TestEnv = TestEnv ()

data Boom = Boom
  deriving stock (Show)

instance E.Exception Boom

routes :: [Route TestEnv]
routes =
  [ get "/hello" (text "hi"),
    get "/users/:id" $ do
      uid <- getCapture "id"
      text uid,
    get "/teapot" $ do
      setStatus status418
      _ <- finish
      setStatus status200,
    get "/boom" (E.throwIO Boom),
    get "/search" $ do
      mq <- param "q"
      text $ fromMaybe "missing" mq,
    get "/page" $ do
      n <- param_ "n" :: WebbyM TestEnv Int
      text $ show n,
    post "/echo-json" $ do
      v <- jsonData :: WebbyM TestEnv A.Value
      json v,
    get "/add-headers" $ do
      addHeader ("X-Foo", "a")
      addHeader ("X-Foo", "b")
      text "ok",
    get "/set-headers" $ do
      setHeader ("X-Foo", "a")
      setHeader ("X-Foo", "b")
      text "ok"
  ]

boomHandler :: Boom -> WebbyM TestEnv ()
boomHandler _ = do
  setStatus status503
  text "boom-handled"

mkApp :: IO Application
mkApp =
  mkWebbyApp (TestEnv ()) $
    setExceptionHandler boomHandler $
      setRoutes routes defaultWebbyServerConfig

mkReq :: ByteString -> Request
mkReq = WT.setPath defaultRequest

mkJsonPost :: ByteString -> LByteString -> WT.SRequest
mkJsonPost path body =
  WT.SRequest
    (WT.setPath
        (defaultRequest
            { requestMethod = methodPost,
              requestHeaders = [(hContentType, "application/json")]
            })
        path)
    body

run :: WT.Session a -> IO a
run sess = mkApp >>= WT.runSession sess

appTests :: TestTree
appTests =
  testGroup
    "App round-trip"
    [ testCase "404 on unmatched route" $
        run $ do
          r <- WT.request (mkReq "/nope")
          liftIO $ WT.simpleStatus r @?= status404,
      testCase "200 + body on basic GET" $
        run $ do
          r <- WT.request (mkReq "/hello")
          liftIO $ do
            WT.simpleStatus r @?= status200
            WT.simpleBody r @?= "hi",
      testCase "capture extraction echoes path segment" $
        run $ do
          r <- WT.request (mkReq "/users/42")
          liftIO $ do
            WT.simpleStatus r @?= status200
            WT.simpleBody r @?= "42",
      testCase "finish short-circuits and preserves earlier setStatus" $
        run $ do
          r <- WT.request (mkReq "/teapot")
          liftIO $ WT.simpleStatus r @?= status418,
      testCase "custom exceptionHandler runs on thrown exception" $
        run $ do
          r <- WT.request (mkReq "/boom")
          liftIO $ do
            WT.simpleStatus r @?= status503
            WT.simpleBody r @?= "boom-handled",
      testCase "param returns Just for present query param" $
        run $ do
          r <- WT.request (mkReq "/search?q=foo")
          liftIO $ do
            WT.simpleStatus r @?= status200
            WT.simpleBody r @?= "foo",
      testCase "param returns Nothing when missing" $
        run $ do
          r <- WT.request (mkReq "/search")
          liftIO $ do
            WT.simpleStatus r @?= status200
            WT.simpleBody r @?= "missing",
      testCase "param_ parses typed value" $
        run $ do
          r <- WT.request (mkReq "/page?n=42")
          liftIO $ do
            WT.simpleStatus r @?= status200
            WT.simpleBody r @?= "42",
      testCase "param_ returns 400 on unparseable value" $
        run $ do
          r <- WT.request (mkReq "/page?n=abc")
          liftIO $ WT.simpleStatus r @?= status400,
      testCase "jsonData round-trips a Value" $
        run $ do
          let body = "{\"x\":42}"
          r <- WT.srequest (mkJsonPost "/echo-json" body)
          liftIO $ do
            WT.simpleStatus r @?= status200
            A.decode (WT.simpleBody r) @?= (A.decode body :: Maybe A.Value),
      testCase "jsonData returns 400 on malformed body" $
        run $ do
          r <- WT.srequest (mkJsonPost "/echo-json" "not json")
          liftIO $ do
            WT.simpleStatus r @?= status400
            WT.simpleBody r @?= "Invalid JSON body",
      testCase "addHeader appends duplicate header values" $
        run $ do
          r <- WT.request (mkReq "/add-headers")
          liftIO $
            filter ((== "X-Foo") . fst) (WT.simpleHeaders r)
              @?= [("X-Foo", "a"), ("X-Foo", "b")],
      testCase "setHeader replaces an existing header value" $
        run $ do
          r <- WT.request (mkReq "/set-headers")
          liftIO $
            filter ((== "X-Foo") . fst) (WT.simpleHeaders r)
              @?= [("X-Foo", "b")]
    ]
