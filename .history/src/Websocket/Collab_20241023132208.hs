{-# LANGUAGE OverloadedStrings #-}
module WebSocket.Collab where

import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, modifyMVar_, readMVar)

-- A handler for WebSocket connections for collaboration
collabApp :: WS.PendingConnection -> IO ()
collabApp pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    talk conn

-- Function to handle real-time updates from clients
talk :: WS.Connection -> IO ()
talk conn = do
    msg <- WS.receiveData conn
    -- Process the message and broadcast to other clients
    WS.sendTextData conn ("You said: " <> msg)
    talk conn
