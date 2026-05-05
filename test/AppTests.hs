{-# LANGUAGE OverloadedStrings #-}

module AppTests (appTests) where

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
    get "/boom" (E.throwIO Boom)
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
            WT.simpleBody r @?= "boom-handled"
    ]
