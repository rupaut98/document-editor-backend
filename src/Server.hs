-- src/Server.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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
import Api.DocumentAPI
import Api.AuthServer
import Api.DocumentServer

type CombinedAPI = AuthAPI :<|> DocumentAPI :<|> Raw

serverWithAuth :: Pool Postgres -> CookieSettings -> JWTSettings -> Server CombinedAPI
serverWithAuth pool cookieCfg jwtCfg =
         authServer pool cookieCfg jwtCfg
    :<|> documentServer pool cookieCfg jwtCfg
    :<|> serveDirectoryFileServer "static"

-- Implement serveDirectoryFileServer as per your requirements
