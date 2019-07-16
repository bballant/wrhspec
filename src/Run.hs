{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  (
    run
  , webApp
  , getCache
  ) where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map.Lazy              as M
import           Data.Text.Read
import           Fib
import           Import
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           WaiRIO

helloWorld :: Response
helloWorld =
  responseLBS
    status200
    [(hContentType, "text/plain")]
    "Hello, world!\n"

getCache :: RIO App (Map Integer Integer)
getCache = do
  options    <- appOptions <$> ask
  maybeCache <- liftIO $ Data.Aeson.decodeFileStrict (optionsCachePath options)
  return $ fromMaybe M.empty maybeCache

getFib
  :: Integer
  -> RIO App Integer
getFib n =
  cachedFib n <$> getCache

showResponse
  :: Show a
  => a
  -> Response
showResponse x =
  responseLBS
    status200
    [(hContentType, "text/plain")]
    (LB8.pack . show $ x)

notFound :: Response
notFound =
  responseLBS
    status404
    [(hContentType, "text/plain")]
    "404 - Not Found"

badRequest :: Response
badRequest =
  responseLBS
    status400
    [(hContentType, "text/plain")]
    "400 - Bad Request"

handleGet
  :: Request
  -> RIO App Response
handleGet request =
  case pathInfo request of
    []            -> return helloWorld
    ["fib", nStr] -> case decimal nStr of
                       Right (n, _) -> showResponse <$> getFib n
                       Left  _      -> return badRequest
    _             -> return notFound

webApp :: ApplicationT (RIO App)
webApp request respond =
  respond =<<
    case requestMethod request of
      "GET" -> handleGet request
      _     -> return notFound

run :: RIO App ()
run = do
  appEnv <- ask
  let options = appOptions appEnv
      port    = optionsPort options
  waiApp <- runApplicationT webApp
  liftIO $ Warp.run port waiApp
