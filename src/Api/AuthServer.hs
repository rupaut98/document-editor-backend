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
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Auth
import Crypto.BCrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (isJust)

-- Define the server for AuthAPI
authServer :: ConnectionPool -> CookieSettings -> JWTSettings -> Server AuthAPI
authServer pool cookieCfg jwtCfg = registerHandler :<|> loginHandler
  where
    -- Handler for user registration
    registerHandler :: AuthRequest -> Handler NoContent
    registerHandler (AuthRequest uname pwd) = do
        existingUser <- liftIO $ flip runSqlPersistMPool pool $ getBy (UniqueUsername uname)
        if isJust existingUser
            then throwError err400 { errBody = "Username already exists." }
            else do
                -- Hash the password
                mHashed <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 $ T.unpack pwd)
                case mHashed of
                    Nothing -> throwError err500 { errBody = "Failed to hash password." }
                    Just hashed -> do
                        -- Insert the new user into the database
                        _ <- liftIO $ flip runSqlPersistMPool pool $ insert $ User uname (decodeUtf8 hashed)
                        return NoContent

    -- Handler for user login
    loginHandler :: AuthRequest -> Handler AuthResponse
    loginHandler (AuthRequest uname pwd) = do
        mUser <- liftIO $ flip runSqlPersistMPool pool $ getBy (UniqueUsername uname)
        case mUser of
            Nothing -> throwError err401 { errBody = "Invalid username or password." }
            Just (Entity userId user) -> do
                -- Verify the password
                let isValid = validatePassword (encodeUtf8 $ T.unpack $ userPassword user) (encodeUtf8 $ T.unpack pwd)
                if isValid
                    then do
                        let authUser = AuthenticatedUser userId uname
                        -- Generate JWT token
                        mToken <- liftIO $ makeJWT authUser jwtCfg Nothing
                        case mToken of
                            Left err -> throwError err500 { errBody = "Failed to generate token." }
                            Right token -> return $ AuthResponse (T.pack $ show token)
                    else throwError err401 { errBody = "Invalid username or password." }
