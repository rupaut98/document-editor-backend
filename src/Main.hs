-- src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Connection (PostgresConfig(..), createPostgresPool)
import Data.Pool (Pool)
import Auth
import Model
import Server (api, serverWithAuth)
import Database.Utils (runBeamPostgresPool)

main :: IO ()
main = do
    -- Initialize Cookie and JWT settings
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings

    -- Database connection string
    let connStr = "host=localhost dbname=documents_db user=rupakraut password=rupakraut"

    -- Create a connection pool
    pool <- runStdoutLoggingT $
            createPostgresPool (PostgresConfig connStr) 10

    -- Initialize the application with authentication context
    let context = cookieCfg :. jwtCfg :. EmptyContext
        port = 8080

    -- Run database migrations (if any)
    runStdoutLoggingT $ runBeamPostgresPool pool $ runMigration migrateAll

    -- Run the server
    putStrLn $ "Server is running on port " ++ show port
    run port $ serveWithContext api context (serverWithAuth pool cookieCfg jwtCfg)
