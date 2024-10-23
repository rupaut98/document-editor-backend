-- src/Server.hs
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant
import Api.Document (DocumentAPI, documentAPI, documentServer)
import WebSocket.Collab (collabApp)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.HTTP.Types (status200, methodOptions)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import Data.Tagged (Tagged(..))  -- Import Tagged

-- Custom CORS Middleware
corsMiddleware :: Middleware
corsMiddleware app req respond = do
    if requestMethod req == methodOptions
        then respond $ responseLBS
            status200
            [ ("Access-Control-Allow-Origin", "http://localhost:3000")
            , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
            , ("Access-Control-Allow-Headers", "Content-Type, Authorization")
            , ("Access-Control-Allow-Credentials", "true")
            , ("Access-Control-Max-Age", "3600")
            ]
            ""
        else do
            app req $ \res -> respond $ mapResponseHeaders addCORS res
  where
    addCORS headers =
        ("Access-Control-Allow-Origin", "http://localhost:3000") : headers

-- Combine the APIs with the handlers
type CombinedAPI = DocumentAPI :<|> Raw

server :: Server CombinedAPI
server = documentServer :<|> Tagged (staticApp (defaultFileServerSettings "static"))

-- Application for REST API
appAPI :: Application
appAPI = serve (Proxy :: Proxy CombinedAPI) server

-- WebSocket Application
wsApp :: WS.ServerApp
wsApp = collabApp

-- Combine REST API and WebSocket handling
combinedApp :: Application
combinedApp = websocketsOr WS.defaultConnectionOptions wsApp appAPI

-- Apply Custom CORS Middleware
appWithCors :: Application
appWithCors = corsMiddleware combinedApp

-- Start the Warp server with CORS and WebSocket support
startApp :: IO ()
startApp = do
    putStrLn "Server is running on port 8080"
    run 8080 appWithCors
