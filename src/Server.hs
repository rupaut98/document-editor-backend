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

-- Combine the API with the handlers
server :: Server DocumentAPI  -- Use DocumentAPI directly
server = documentServer

-- Application for Warp server and WebSocket handling
app :: Application
app = serve documentAPI server  -- Use documentAPI directly

-- Start the Warp server with WebSocket support
startApp :: IO ()
startApp = do
    let staticFiles = staticApp (defaultFileServerSettings "static")
    let wsApp = websocketsOr WS.defaultConnectionOptions collabApp app
    run 8080 wsApp
