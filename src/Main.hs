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
import Database.Beam.Postgres.Connection (withPostgresConn)
import Data.Pool (Pool, createPool, withResource)
import Auth
import Model
import Server (api, serverWithAuth)

-- Function to create a PostgreSQL connection pool
createPostgresPool :: IO (Pool Pg.Connection)
createPostgresPool = do
    let connStr = "host=localhost dbname=documents_db user=rupakraut password=rupakraut"
    createPool (connectPostgreSQL (fromString connStr)) Pg.close 1 10 10  -- Adjust pool parameters as needed

-- Helper function to run Beam actions with the connection pool
runBeamPostgresPool :: Pool Pg.Connection -> Pg a -> IO a
runBeamPostgresPool pool action = withResource pool (`runBeamPostgres` action)

main :: IO ()
main = do
    -- Initialize Cookie and JWT settings
    myKey <- generateKey
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings

    -- Create a connection pool
    pool <- createPostgresPool

    -- Run database migrations (if any)
    runBeamPostgresPool pool $ runMigration migrateAll

    -- Initialize the application with authentication context
    let context = cookieCfg :. jwtCfg :. EmptyContext
        port = 8080

    -- Run the server
    putStrLn $ "Server is running on port " ++ show port
    run port $ serveWithContext api context (serverWithAuth pool cookieCfg jwtCfg)
