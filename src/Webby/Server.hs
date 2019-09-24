module Webby.Server where


import qualified Control.Monad.Logger       as Log
import qualified Data.Aeson                 as A
import qualified Data.Binary.Builder        as Bu
import qualified Data.ByteString.Lazy       as LB
import qualified Data.HashMap.Strict        as H
import qualified Data.List                  as L
import qualified Data.Text                  as T
import           Network.HTTP.Types.URI     (queryToQueryText)
import           Network.Wai.Internal       (getRequestBodyChunk)
import qualified System.Log.FastLogger      as FLog
import           System.Log.FastLogger.Date (newTimeCache)
import qualified UnliftIO.Concurrent        as Conc
import qualified UnliftIO.Exception         as E
import           Web.HttpApiData

import           WebbyPrelude

import           Webby.Types

-- | Retrieve the app environment given to the application at
-- initialization.
getAppEnv :: WebbyM appEnv appEnv
getAppEnv = asks weAppEnv

runAppEnv :: ReaderT appEnv (WebbyM appEnv) a -> WebbyM appEnv a
runAppEnv appFn = do
    env <- getAppEnv
    runReaderT appFn env

-- | Retrieve all path captures
captures :: WebbyM appEnv Captures
captures = asks weCaptures

-- | Retrieve a particular capture (TODO: extend?)
getCapture :: (FromHttpApiData a) => Text -> WebbyM appEnv a
getCapture capName = do
    cs <- captures
    case H.lookup capName cs of
        Nothing  -> throwIO $ WebbyMissingCapture capName
        Just cap -> either (throwIO . WebbyParamParseError capName . show)
                    return $
                    parseUrlPiece cap

setStatus :: Status -> WebbyM appEnv ()
setStatus sts = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $ \wr -> return $ wr { wrStatus = sts}

addHeader :: Header -> WebbyM appEnv ()
addHeader h = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                  return $ wr { wrHeaders = hs ++ [h] }

-- similar to addHeader but replaces a header
setHeader :: Header -> WebbyM appEnv ()
setHeader (k, v) = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                      ohs = filter ((/= k) . fst) hs
                  return $ wr { wrHeaders = ohs ++ [(k, v)] }

resp400 :: Text -> WebbyM appEnv a
resp400 msg = do
    setStatus status400
    json $ A.object [ "error" A..= A.String msg ]
    finish

params :: WebbyM appEnv [(Text, Text)]
params = do
    qparams <- (queryToQueryText . queryString) <$> request
    return $ fmap (\(q, mv) -> (,) q $ fromMaybe "" mv) qparams

flag :: Text -> WebbyM appEnv Bool
flag name = (isJust . L.lookup name) <$> params

param :: (FromHttpApiData a) => Text -> WebbyM appEnv (Maybe a)
param p = do ps <- params
             case L.lookup p ps of
                 Nothing -> return Nothing
                 Just myParam -> either (throwIO . WebbyParamParseError p . show)
                                 (return . Just) $
                                 parseQueryParam myParam

param_ :: (FromHttpApiData a) => Text -> WebbyM appEnv a
param_ p = do myParam <- param p
              maybe (resp400 $ T.concat [p, " missing in params"])
                  return myParam

header :: HeaderName -> WebbyM appEnv (Maybe Text)
header n = do
    hs <- requestHeaders <$> request
    return $ headMay $ map (decodeUtf8Lenient . snd) $ filter ((n == ) . fst) hs

request :: WebbyM appEnv Request
request = asks weRequest

-- | Returns an action that returns successive chunks of the rquest
-- body. It returns an empty bytestring after the request body is
-- consumed.
getRequestBodyChunkAction :: WebbyM appEnv (WebbyM appEnv ByteString)
getRequestBodyChunkAction = (liftIO . getRequestBodyChunk) <$> asks weRequest

headers :: WebbyM appEnv [Header]
headers = requestHeaders <$> request

requestBodyLength :: WebbyM appEnv (Maybe Int64)
requestBodyLength = do
    hMay <- header hContentLength
    return $ do val <- toS <$> hMay
                parseInt val

finish :: WebbyM appEnv a
finish = E.throwIO FinishThrown

blob :: ByteString -> WebbyM appEnv ()
blob bs = do
    setHeader (hContentType, "application/octet-stream")
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromByteString bs }

text :: Text -> WebbyM appEnv ()
text txt = do
    setHeader (hContentType, "text/plain; charset=utf-8")
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromByteString $
                                          encodeUtf8 txt }

-- | Return the raw request body as a lazy bytestring
requestBodyLBS :: WebbyM appEnv LByteString
requestBodyLBS = do
    req <- request
    liftIO $ lazyRequestBody req

jsonData :: A.FromJSON a => WebbyM appEnv a
jsonData = do
   req <- request
   body <- liftIO $ lazyRequestBody req
   either (throwIO . WebbyJSONParseError . T.pack) return $ A.eitherDecode body

json :: A.ToJSON b => b -> WebbyM appEnv ()
json j = do
    setHeader (hContentType, "application/json; charset=utf-8")
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromLazyByteString $
                                          A.encode j }

stream :: StreamingBody -> WebbyM appEnv ()
stream s = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Left s }

matchRequest :: Request -> [(RoutePattern, a)] -> Maybe (Captures, a)
matchRequest _ [] = Nothing
matchRequest req ((RoutePattern method pathSegs, handler):rs) =
    if requestMethod req == method
    then case go (pathInfo req) pathSegs H.empty of
           Nothing -> matchRequest req rs
           Just cs -> return (cs, handler)
    else matchRequest req rs
  where
    go [] p h | mconcat p == "" = Just h
              | otherwise = Nothing
    go p [] h | mconcat p == "" = Just h
              | otherwise = Nothing
    go (p:ps) (l:pat) h | T.head l == ':' = go ps pat $ H.insert (T.drop 1 l) p h
                        | p == l = go ps pat h
                        | otherwise = Nothing

errorResponse404 :: WebbyM appEnv ()
errorResponse404 = setStatus status404

invalidRoutesErr :: [Char]
invalidRoutesErr = "Invalid route specification: contains duplicate routes or routes with overlapping capture patterns."

-- | Use this function, to create a WAI application. It takes a user/application
-- defined `appEnv` data type and a list of routes. Routes are matched in the
-- given order. If none of the requests match a request, a default 404 response
-- is returned.
mkWebbyApp :: appEnv -> [Route appEnv] -> IO Application
mkWebbyApp appEnv routes' = do
    lset <- FLog.newStdoutLoggerSet FLog.defaultBufSize
    return $ mkApp lset routes'

  where

    mkApp lset routes req respond = do
        let defaultHandler = errorResponse404
            (cs, handler) = fromMaybe (H.empty, defaultHandler) $
                            matchRequest req routes

        timeFn <- newTimeCache "%Y-%m-%dT%H:%M:%S "
        wEnv <- do v <- Conc.newMVar defaultWyResp
                   return $ WEnv v cs req appEnv lset timeFn

        (do runWebbyM wEnv handler
            webbyReply wEnv respond) `E.catches`
            [ -- Handles Webby' exceptions while parsing parameters
              -- and request body
              E.Handler (\(ex :: WebbyError) -> case ex of
                            wmc@(WebbyMissingCapture _) ->
                                respond $ responseLBS status404 [] $
                                toS $ displayException wmc

                            _ -> respond $ responseLBS status400 [] $
                                 toS $ displayException ex
                        )

              -- Handles Webby's finish statement
            , E.Handler (\(_ :: FinishThrown) -> webbyReply wEnv respond)

              -- Handles any other exception thrown in the handler
            , E.Handler (\(ex :: E.SomeException) -> do
                              Log.runStdoutLoggingT $ Log.logInfoN (show ex)
                              respond $ responseLBS status500 [] (show ex))
            ]

    webbyReply wEnv respond' = do
        let wVar = weResp wEnv
        wr <- Conc.takeMVar wVar
        case wrRespData wr of
          Left s  -> respond' $ responseStream (wrStatus wr) (wrHeaders wr) s
          Right b -> do
              let clen = LB.length $ Bu.toLazyByteString b
              respond' $ responseBuilder (wrStatus wr)
                  (wrHeaders wr ++ [(hContentLength, show clen)]) b

-- | Create a route for a user-provided HTTP request method, pattern
-- and handler function.
mkRoute :: Method -> Text -> WebbyM appEnv ()
        -> (RoutePattern, WebbyM appEnv ())
mkRoute m p h =
  let p' = if | T.null p -> "/"
              | T.head p /= '/' -> "/" <> p
              | otherwise -> p
  in (RoutePattern m (drop 1 $ T.splitOn "/" p'), h)

-- | Create a route for a POST request method, given the path pattern
-- and handler.
post :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
post = mkRoute methodPost

-- | Create a route for a GET request method, given the path pattern
-- and handler.
get :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
get = mkRoute methodGet

-- | Create a route for a PUT request method, given the path pattern
-- and handler.
put :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
put = mkRoute methodPut

-- | Create a route for a DELETE request method, given path pattern and handler.
delete :: Text -> WebbyM appEnv () -> (RoutePattern, WebbyM appEnv ())
delete = mkRoute methodDelete
