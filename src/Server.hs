-- src/Server.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Servant
import Servant.Auth.Server
import Database.Beam
import Database.Beam.Postgres
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runBeamPostgresPool)
import Data.Pool (Pool)
import Auth
import Model
import Api.AuthAPI
import Api.AuthServer
import Api.DocumentAPI
import Api.DocumentServer
import WebSocket.Collab (collabApp)  -- If used
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

-- Combine the APIs
type CombinedAPI = AuthAPI :<|> DocumentAPI :<|> Raw

-- Define the server with authentication
serverWithAuth :: Pool Pg.Connection -> CookieSettings -> JWTSettings -> Server CombinedAPI
serverWithAuth pool cookieCfg jwtCfg =
         authServer pool cookieCfg jwtCfg
    :<|> documentServer pool cookieCfg jwtCfg
    :<|> serveDirectoryFileServer "static"  -- Serve static files from "static" directory
