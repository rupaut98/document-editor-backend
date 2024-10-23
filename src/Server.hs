{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant
import Api.Document (DocumentAPI, documentAPI, documentServer)  -- Proper import here
import WebSocket.Collab (collabApp)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Middleware.Cors

-- Combine the API with the handlers
server :: Server DocumentAPI  -- Use DocumentAPI directly
server = documentServer

-- Application for Warp server and WebSocket handling
app :: Application
app = serve documentAPI server  -- Use documentAPI directly

corsPolicy:: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:3000"], True)  -- Allow http://localhost:3000
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]  -- Allowed HTTP methods
  , corsRequestHeaders = ["Content-Type", "Authorization"]  -- Allowed headers
  , corsExposedHeaders = ["Set-Cookie"]  -- Exposed headers (if needed)
  , corsMaxAge = Just 3600  -- Cache preflight response for 1 hour
  }

-- Start the Warp server with WebSocket support
startApp :: IO ()
startApp = do
    let staticFiles = staticApp (defaultFileServerSettings "static")
    let wsApp = websocketsOr WS.defaultConnectionOptions collabApp app

    let appWithCors = cors (const $ Just corsPolicy) app
    
    run 8080 wsApp
