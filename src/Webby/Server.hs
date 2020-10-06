module Webby.Server where

import qualified Data.Aeson as A
import qualified Data.Binary.Builder as Bu
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified UnliftIO.Concurrent as Conc
import qualified UnliftIO.Exception as E
import Web.HttpApiData
import Webby.Types
import Prelude

asksWEnv :: (WEnv appEnv -> a) -> WebbyM appEnv a
asksWEnv getter = WebbyM $ lift $ asks getter

-- | Retrieve all path captures
captures :: WebbyM appEnv Captures
captures = asksWEnv weCaptures

-- | Retrieve a particular capture
getCapture :: (FromHttpApiData a) => Text -> WebbyM appEnv a
getCapture capName = do
  cs <- captures
  case H.lookup capName cs of
    Nothing -> throwIO $ WebbyMissingCapture capName
    Just cap ->
      either
        (throwIO . WebbyParamParseError capName . show)
        return
        $ parseUrlPiece cap

-- | Set response status
setStatus :: Status -> WebbyM appEnv ()
setStatus sts = do
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $ \wr -> return $ wr {wrStatus = sts}

-- | Append given header to the response headers
addHeader :: Header -> WebbyM appEnv ()
addHeader h = do
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $
    \wr -> do
      let hs = wrHeaders wr
      return $ wr {wrHeaders = hs ++ [h]}

-- | Similar to 'addHeader' but replaces a header
setHeader :: Header -> WebbyM appEnv ()
setHeader (k, v) = do
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $
    \wr -> do
      let hs = wrHeaders wr
          ohs = filter ((/= k) . fst) hs
      return $ wr {wrHeaders = ohs ++ [(k, v)]}

resp400 :: Text -> WebbyM appEnv a
resp400 msg = do
  setStatus status400
  json $ A.object ["error" A..= A.String msg]
  finish

-- | Get all request query params as a list of key-value pairs
params :: WebbyM appEnv [(Text, Text)]
params = do
  qparams <- (queryToQueryText . queryString) <$> request
  return $ fmap (\(q, mv) -> (,) q $ fromMaybe "" mv) qparams

-- | Checks if the request contains the given query param
flag :: Text -> WebbyM appEnv Bool
flag name = (isJust . L.lookup name) <$> params

-- | Gets the given query param's value
param :: (FromHttpApiData a) => Text -> WebbyM appEnv (Maybe a)
param p = do
  ps <- params
  case L.lookup p ps of
    Nothing -> return Nothing
    Just myParam ->
      either
        (throwIO . WebbyParamParseError p . show)
        (return . Just)
        $ parseQueryParam myParam

-- | Similar to 'param' except that it returns the handler with a '400
-- BadRequest' if the query param is missing.
param_ :: (FromHttpApiData a) => Text -> WebbyM appEnv a
param_ p = do
  myParam <- param p
  maybe
    (resp400 $ T.concat [p, " missing in params"])
    return
    myParam

-- | Get the given header's value
header :: HeaderName -> WebbyM appEnv (Maybe Text)
header n = do
  hs <- requestHeaders <$> request
  return $ headMay $ map (decodeUtf8 . snd) $ filter ((n ==) . fst) hs

-- | Get the 'Network.Wai.Request' of the handler
request :: WebbyM appEnv Request
request = asksWEnv weRequest

-- | Returns an action that returns successive chunks of the rquest
-- body. It returns an empty bytestring after the request body is
-- consumed.
getRequestBodyChunkAction :: WebbyM appEnv (WebbyM appEnv ByteString)
getRequestBodyChunkAction = (liftIO . getRequestBodyChunk) <$> asksWEnv weRequest

-- | Get all the request headers
headers :: WebbyM appEnv [Header]
headers = requestHeaders <$> request

-- | Returns request body size in bytes
requestBodyLength :: WebbyM appEnv (Maybe Int64)
requestBodyLength = do
  hMay <- header hContentLength
  return $ do
    val <- hMay
    parseInt val

-- | Used to return early from an API handler
finish :: WebbyM appEnv a
finish = E.throwIO FinishThrown

-- | Send a binary stream in the response body
blob :: ByteString -> WebbyM appEnv ()
blob bs = do
  setHeader (hContentType, "application/octet-stream")
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $
    \wr -> return $ wr {wrRespData = Right $ Bu.fromByteString bs}

-- | Send plain-text in the response body
text :: Text -> WebbyM appEnv ()
text txt = do
  setHeader (hContentType, "text/plain; charset=utf-8")
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $
    \wr ->
      return $
        wr
          { wrRespData =
              Right $
                Bu.fromByteString $
                  encodeUtf8 txt
          }

-- | Return the raw request body as a lazy bytestring
requestBodyLBS :: WebbyM appEnv LByteString
requestBodyLBS = do
  req <- request
  liftIO $ lazyRequestBody req

-- | Parse the request body as a JSON object and return it. Raises
-- 'WebbyJSONParseError' exception if parsing is unsuccessful.
jsonData :: A.FromJSON a => WebbyM appEnv a
jsonData = do
  req <- request
  body <- liftIO $ lazyRequestBody req
  either (throwIO . WebbyJSONParseError . T.pack) return $ A.eitherDecode body

-- | Set the body of the response to the JSON encoding of the given value. Also
-- sets "Content-Type" header to "application/json; charset=utf-8" if it has not
-- already been set.
json :: A.ToJSON b => b -> WebbyM appEnv ()
json j = do
  setHeader (hContentType, "application/json; charset=utf-8")
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $
    \wr ->
      return $
        wr
          { wrRespData =
              Right $
                Bu.fromLazyByteString $
                  A.encode j
          }

-- | Set the body of the response to a StreamingBody. Doesn't set the
-- "Content-Type" header, so you probably want to do that on your own with
-- 'setHeader'.
stream :: StreamingBody -> WebbyM appEnv ()
stream s = do
  wVar <- asksWEnv weResp
  Conc.modifyMVar_ wVar $
    \wr -> return $ wr {wrRespData = Left s}

matchRequest :: Request -> [(RoutePattern, a)] -> Maybe (Captures, a)
matchRequest _ [] = Nothing
matchRequest req ((RoutePattern method pathSegs, handler) : rs) =
  if requestMethod req == method
    then case go (pathInfo req) pathSegs H.empty of
      Nothing -> matchRequest req rs
      Just cs -> return (cs, handler)
    else matchRequest req rs
  where
    go [] p h
      | mconcat p == "" = Just h
      | otherwise = Nothing
    go p [] h
      | mconcat p == "" = Just h
      | otherwise = Nothing
    go (p : ps) (l : pat) h
      | T.head l == ':' = go ps pat $ H.insert (T.drop 1 l) p h
      | p == l = go ps pat h
      | otherwise = Nothing

errorResponse404 :: WebbyM appEnv ()
errorResponse404 = setStatus status404

invalidRoutesErr :: [Char]
invalidRoutesErr = "Invalid route specification: contains duplicate routes or routes with overlapping capture patterns."

-- | Use this function to create a WAI application. It takes a user/application
-- defined @appEnv@ data type and a list of routes. Routes are matched in the
-- given order. If none of the requests match a request, a default 404 response
-- is returned.
mkWebbyApp :: env -> WebbyServerConfig env -> IO Application
mkWebbyApp env wsc =
  return $ mkApp
  where
    shortCircuitHandler =
      [ -- Handler for FinishThrown exception to guide
        -- short-circuiting handlers to early completion
        E.Handler (\(ex :: FinishThrown) -> E.throwIO ex)
      ]
    mkApp req respond = do
      let defaultHandler = errorResponse404
          routes = wscRoutes wsc
          exceptionHandlerMay = wscExceptionHandler wsc
          (cs, handler) =
            fromMaybe (H.empty, defaultHandler) $
              matchRequest req routes

      wEnv <- do
        v <- Conc.newMVar defaultWyResp
        return $ WEnv v cs req env exceptionHandlerMay
      ( do
          runWebbyM wEnv $
            handler
              `E.catches` ( shortCircuitHandler
                              <> fmap (\(WebbyExceptionHandler e) -> E.Handler e) (maybeToList exceptionHandlerMay)
                          )
          webbyReply wEnv respond
        )
        `E.catches` [
                      -- Handles Webby' exceptions while parsing parameters
                      -- and request body
                      E.Handler
                        ( \(ex :: WebbyError) -> case ex of
                            wmc@(WebbyMissingCapture _) ->
                              respond $
                                responseLBS status404 [] $
                                  encodeUtf8 $
                                    displayException wmc
                            _ ->
                              respond $
                                responseLBS status400 [] $
                                  encodeUtf8 $
                                    displayException ex
                        ),
                      -- Handles Webby's finish statement
                      E.Handler (\(_ :: FinishThrown) -> webbyReply wEnv respond)
                    ]
    webbyReply wEnv respond' = do
      let wVar = weResp wEnv
      wr <- Conc.takeMVar wVar
      case wrRespData wr of
        Left s -> respond' $ responseStream (wrStatus wr) (wrHeaders wr) s
        Right b -> do
          let clen = LB.length $ Bu.toLazyByteString b
          respond' $
            responseBuilder
              (wrStatus wr)
              (wrHeaders wr ++ [(hContentLength, show clen)])
              b

-- | Create a route for a user-provided HTTP request method, pattern
-- and handler function.
mkRoute ::
  Method ->
  Text ->
  WebbyM appEnv () ->
  (RoutePattern, WebbyM appEnv ())
mkRoute m p h =
  let p' =
        if
            | T.null p -> "/"
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
