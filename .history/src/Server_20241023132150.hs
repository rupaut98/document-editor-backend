{-# LANGUAGE OverloadedStrings #-}
module Server where

import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import WebSocket.Collab (collabApp)
import Api.Document (documentAPI, documentServer)
import Api.Auth (authAPI, authServer)

startApp :: IO ()
startApp = do
    -- Setup the application, combining WebSockets and REST API
    let app = websocketsOr defaultConnectionOptions collabApp (serve documentAPI documentServer)
    run 8080 app
