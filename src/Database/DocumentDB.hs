{-# LANGUAGE OverloadedStrings #-}

module Database.DocumentDB where

import Database.PostgreSQL.Simple
import Model (Document)

-- Connect to PostgreSQL
connectDB :: IO Connection
connectDB = connect defaultConnectInfo {
    connectHost = "localhost",
    connectUser = "rupakraut",
    connectPassword = "rupakraut",
    connectDatabase = "documents_db"
}

-- Get all documents from the database
getDocuments :: Connection -> IO [Document]
getDocuments conn = query_ conn "SELECT * FROM documents"

-- Insert a new document into the database
createDocument :: Connection -> Document -> IO Int64
createDocument conn doc = execute conn "INSERT INTO documents (title, content) VALUES (?, ?)" (docTitle doc, docContent doc)
