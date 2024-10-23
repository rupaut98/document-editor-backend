{-# LANGUAGE OverloadedStrings #-}

module WebSocket.Collab where

import qualified Network.WebSockets as WS
import qualified Data.Text as T  -- Import the Text module

-- A simple WebSocket handler
collabApp :: WS.PendingConnection -> IO ()
collabApp pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn (T.pack "Welcome to the document collaboration!")  -- Use Text
    talk conn

-- Function to handle messages from clients
talk :: WS.Connection -> IO ()
talk conn = do
    msg <- WS.receiveData conn :: IO T.Text  -- Use Text for receiving messages
    putStrLn ("Received message: " ++ T.unpack msg)  -- Convert Text to String for printing
    WS.sendTextData conn (T.append "You said: " msg)  -- Use Text to send response
    talk conn
