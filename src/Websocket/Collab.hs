{-# LANGUAGE OverloadedStrings #-}

module WebSocket.Collab where

import qualified Network.WebSockets as WS

-- A simple WebSocket handler
collabApp :: WS.PendingConnection -> IO ()
collabApp pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn ("Welcome to the document collaboration!" :: String)
    talk conn

-- Function to handle messages from clients
talk :: WS.Connection -> IO ()
talk conn = do
    msg <- WS.receiveData conn
    putStrLn ("Received message: " ++ msg)
    WS.sendTextData conn ("You said: " <> msg)
    talk conn
