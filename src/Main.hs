-- src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Auth
import Model
import Server (api, serverWithAuth)

main :: IO ()
main = do
    -- Initialize Cookie and JWT settings
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings

    -- Initialize the application with authentication context
    let context = cookieCfg :. jwtCfg :. EmptyContext
        port = 8080

    -- Run database migrations
    runStderrLoggingT $ withSqlitePool "documents.db" 10 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ runMigration migrateAll

        -- Run the server
        putStrLn $ "Server is running on port " ++ show port
        run port $ serveWithContext api context (serverWithAuth pool cookieCfg jwtCfg)
