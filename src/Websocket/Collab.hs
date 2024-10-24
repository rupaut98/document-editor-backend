-- src/WebSocket/Collab.hs
{-# LANGUAGE OverloadedStrings #-}

module WebSocket.Collab where

import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- A simple WebSocket handler
collabApp :: WS.PendingConnection -> IO ()
collabApp pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn ("Welcome to the document collaboration!" :: Text)
    talk conn

-- Function to handle messages from clients
talk :: WS.Connection -> IO ()
talk conn = do
    msg <- WS.receiveData conn :: IO Text  -- Explicitly specify the type as Text
    TIO.putStrLn ("Received message: " <> msg)  -- Use Text-aware putStrLn
    WS.sendTextData conn ("You said: " <> msg)
    talk conn
