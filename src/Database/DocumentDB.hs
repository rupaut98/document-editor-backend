{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentDB where

import Database.PostgreSQL.Simple
import Model (Document)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Control.Exception (catch, SomeException)
import Database.Dotenv (loadFile, defaultConfig)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- Load environment variables from .env
initializeEnv :: IO ()
initializeEnv = do
    result <- loadFile defaultConfig
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error loading .env file: " ++ show err
            exitFailure
        Right () -> return ()

-- Connect to PostgreSQL using environment variables
connectDB :: IO Connection
connectDB = do
    initializeEnv
    host <- getEnvVar "PG_HOST"
    user <- getEnvVar "PG_USER"
    password <- getEnvVar "PG_PASSWORD"
    database <- getEnvVar "PG_DATABASE"
    connect defaultConnectInfo {
        connectHost = host,
        connectUser = user,
        connectPassword = password,
        connectDatabase = database
    }

-- Helper function to get environment variables
getEnvVar :: String -> IO String
getEnvVar var = do
    maybeVal <- lookupEnv var
    case maybeVal of
        Just val -> return val
        Nothing -> do
            hPutStrLn stderr $ "Environment variable " ++ var ++ " not set."
            exitFailure

-- Get all documents from the database
getDocuments :: Connection -> IO [Document]
getDocuments conn = query_ conn "SELECT * FROM documents"

-- Insert a new document into the database
createDocument :: Connection -> Document -> IO Int64
createDocument conn doc = execute conn "INSERT INTO documents (title, content) VALUES (?, ?)" (docTitle doc, docContent doc)
