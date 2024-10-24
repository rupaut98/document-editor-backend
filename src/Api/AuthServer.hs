-- src/Api/AuthServer.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.AuthServer where

import Servant
import Servant.Auth.Server
import Model
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Connection (Pg.Connection)
import Control.Monad.IO.Class (liftIO)
import Auth
import Crypto.BCrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad (void)
import Data.Pool (Pool)
import Api.AuthAPI (AuthAPI, AuthRequest(..), AuthResponse(..)) 


authServer :: Pool Pg.Connection -> CookieSettings -> JWTSettings -> Server AuthAPI
authServer pool cookieCfg jwtCfg = registerHandler :<|> loginHandler
  where
    
    registerHandler :: AuthRequest -> Handler NoContent
    registerHandler (AuthRequest uname pwd) = do
        users <- liftIO $ runBeamPostgresPool pool $ runSelectReturningList $ select $
            filter_ (\u -> userUsername u ==. val_ uname) (_users documentDb)
        
        if not (null users)
            then throwError err400 { errBody = "Username already exists." }
            else do
                
                mHashed <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 pwd)
                case mHashed of
                    Nothing -> throwError err500 { errBody = "Failed to hash password." }
                    Just hashed -> do
                        _ <- liftIO $ runBeamPostgresPool pool $ runInsert $
                            insert (_users documentDb) $
                            insertExpressions [ User default_ (val_ uname) (val_ $ decodeUtf8 hashed) ]
                        return NoContent

    -- Handler for user login
    loginHandler :: AuthRequest -> Handler AuthResponse
    loginHandler (AuthRequest uname pwd) = do
        users <- liftIO $ runBeamPostgresPool pool $ runSelectReturningList $ select $
            filter_ (\u -> userUsername u ==. val_ uname) (_users documentDb)
        
        case users of
            [] -> throwError err401 { errBody = "Invalid username or password." }
            (user:_) -> do
                -- Verify the password
                let isValid = validatePassword (encodeUtf8 $ userPassword user) (encodeUtf8 pwd)
                if isValid
                    then do
                        let authUser = AuthenticatedUser (userId user) (userUsername user)
                        -- Generate JWT token
                        mToken <- liftIO $ makeJWT authUser jwtCfg Nothing
                        case mToken of
                            Left err -> throwError err500 { errBody = "Failed to generate token." }
                            Right token -> return $ AuthResponse (decodeUtf8 token)
                    else throwError err401 { errBody = "Invalid username or password." }
