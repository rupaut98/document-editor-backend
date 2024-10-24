-- src/Server.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant
import Servant.Auth.Server
import Api.AuthAPI
import Api.AuthServer
import Api.DocumentAPI
import Api.DocumentServer
import WebSocket.Collab (collabApp)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.HTTP.Types (status200, methodOptions)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import Data.Tagged (Tagged(..))
import Model
import Database.Persist.Sqlite
import Auth
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (liftIO)

-- Combine the APIs with the handlers
type CombinedAPI = AuthAPI :<|> DocumentAPI :<|> Raw

-- Define the server with authentication
serverWithAuth :: ConnectionPool -> CookieSettings -> JWTSettings -> Server CombinedAPI
serverWithAuth pool cookieCfg jwtCfg =
         authServer pool cookieCfg jwtCfg
    :<|> documentServer pool cookieCfg jwtCfg
    :<|> Tagged (staticApp (defaultFileServerSettings "static"))

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

-- Start the Warp server with CORS and WebSocket support
startApp :: IO ()
startApp = do
    -- Initialize Cookie and JWT settings
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings

    -- Initialize the application with authentication context
    let context = cookieCfg :. jwtCfg :. EmptyContext
        port = 8080

    -- Initialize WebSocket shared state if needed
    -- docsState <- initDocsState -- Implement if required

    -- Run database migrations
    runStderrLoggingT $ withSqlitePool "documents.db" 10 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ runMigration migrateAll

        -- Define WebSocket application
        -- let wsApp = collabApp docsState -- Implement if required

        -- Combine REST API and WebSocket handling
        -- let appAPI = serveWithContext (Proxy :: Proxy CombinedAPI) context (serverWithAuth pool cookieCfg jwtCfg)
        --     combinedApp = websocketsOr WS.defaultConnectionOptions wsApp appAPI

        -- Since we're focusing on authentication first, skip WebSockets for now
        let appAPI = serveWithContext (Proxy :: Proxy CombinedAPI) context (serverWithAuth pool cookieCfg jwtCfg)
            combinedApp = appAPI

        -- Apply CORS middleware
        let appWithCors = corsMiddleware combinedApp

        -- Run the server
        putStrLn $ "Server is running on port " ++ show port
        run port appWithCors
